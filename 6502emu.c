/* 6502emu.c */

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include "6502emu.h"
#include "riscv-vm.h"

/* Debug configuration */
#define DEBUG 0
#if DEBUG
#define debug_printf(...) printf(__VA_ARGS__)
#else
#define debug_printf(...) (void)0
#endif

/* Memory space */
static uint8_t memory[65536];
uint8_t emu_ext_mem[0x1000000]; /* REU and GeoRAM */

/* Bit map indicating which bytes have been written */
static uint8_t written[65536/8];

/* REU registers */
static uint8_t reu_regs[11];
static uint8_t reu_shadow[11];
enum reu_addrs {
    reu_status,
    reu_command,
    reu_c64_address_0,
    reu_c64_address_1,
    reu_xmem_address_0,
    reu_xmem_address_1,
    reu_xmem_address_2,
    reu_xfer_size_0,
    reu_xfer_size_1,
    reu_irq_mask,
    reu_address_control
};
static void do_reu(void);

/* Cycle count */
unsigned long emu_cycles;

/* Registers */
struct Registers {
    uint8_t a;
    uint8_t x;
    uint8_t y;
    uint8_t s;
    uint8_t flags; /* N V - B D I Z C */
    uint16_t pc;
    unsigned long cycles;
};
static struct Registers regs;

/* Set up the emulator */
void
emu_init(void)
{
    memset(memory, 0, sizeof(memory));
    memset(written, 0, sizeof(written));
    memset(&regs, 0, sizeof(regs));
    reu_regs[reu_status] = 0x10;
    reu_regs[reu_command] = 0x10;
    reu_regs[reu_c64_address_0] = 0x00;
    reu_regs[reu_c64_address_1] = 0x00;
    reu_regs[reu_xmem_address_0] = 0x00;
    reu_regs[reu_xmem_address_1] = 0x00;
    reu_regs[reu_xmem_address_2] = 0x00;
    reu_regs[reu_xfer_size_0] = 0xFF;
    reu_regs[reu_xfer_size_1] = 0xFF;
    reu_regs[reu_irq_mask] = 0x1F;
    reu_regs[reu_address_control] = 0x3F;
    memcpy(reu_shadow, reu_regs, sizeof(reu_shadow));
    emu_cycles = 0;
}

/* Write one byte */
void
emu_write_byte(uint16_t address, uint8_t byte)
{
    if (0xDF00 <= address && address <= 0xDFFF) {
        /* REU control registers */
        unsigned reu_reg = address & 0x1F;
        if (reu_reg < sizeof(reu_regs) && reu_reg != reu_status) {
            reu_regs[reu_reg] = byte;
            reu_shadow[reu_reg] = byte;
            if (reu_reg == reu_command) {
                do_reu();
            }
        }
    } else {
        memory[address] = byte;
        written[address >> 3] |= 0x1 << (address & 0x7);
        if (address == 0xFF00 && (reu_regs[reu_command] & 0x10) == 0) {
            /* REU 0xFF00 trigger */
            reu_regs[reu_command] |= 0x10;
            do_reu();
        }
    }
    debug_printf("Write  to %04X: %02X\n", address, byte);
}

/* Read one byte, and check for uninitialized memory */
int
emu_read_byte(uint16_t address)
{
    uint8_t byte;

    if (0xDF00 <= address && address <= 0xDFFF) {
        /* REU control registers */
        unsigned reu_reg = address & 0x1F;
        if (reu_reg < sizeof(reu_regs)) {
            byte = reu_regs[reu_reg];
            if (reu_reg == reu_status) {
                reu_regs[reu_status] &= 0x1F;
            }
        } else {
            byte = 0xFF;
        }
    } else {
        if (!(written[address >> 3] & (0x1 << (address & 0x7)))) {
            fprintf(stderr, "Read of uninitialized memory at %04X\n", (unsigned)address);
            return -1;
        }
        byte = memory[address];
    }
    debug_printf("Read from %04X: %02X\n", address, byte);
    return byte;
}

/* REU is implemented here; this is called on writing the REU command register */
static void
do_reu(void)
{
    uint8_t cmd = reu_regs[reu_command];
    if ((cmd & 0x90) == 0x90) {
        /* Transfer extent */
        uint16_t c64_address = (reu_regs[reu_c64_address_0] << 0)
                             | (reu_regs[reu_c64_address_1] << 8);
        uint32_t xmem_address = ((uint32_t)reu_regs[reu_xmem_address_0] <<  0)
                              | ((uint32_t)reu_regs[reu_xmem_address_1] <<  8)
                              | ((uint32_t)reu_regs[reu_xmem_address_2] << 16);
        uint16_t xfer_size = (reu_regs[reu_xfer_size_0] << 0)
                           | (reu_regs[reu_xfer_size_1] << 8);

        /* Transfer type and address increment flags */
        uint8_t xfer_type = cmd & 0x03;
        uint8_t incr_flags = reu_regs[reu_address_control] & 0xC0;

        /* Set this if verify fails */
        bool bad_compare = false;

        /* Do transfer */
#if 0
        switch (xfer_type) {
        case 0x00:
            printf("Write REU %06lX from main memory %04X, %u bytes\n",
                    (unsigned long)xmem_address, (unsigned)c64_address, (unsigned)xfer_size);
            break;

        case 0x01:
            printf("Read REU %06lX to main memory %04X, %u bytes\n",
                    (unsigned long)xmem_address, (unsigned)c64_address, (unsigned)xfer_size);
            break;

        case 0x02:
            printf("Swap REU %06lX and main memory %04X, %u bytes\n",
                    (unsigned long)xmem_address, (unsigned)c64_address, (unsigned)xfer_size);
            break;

        case 0x03:
            printf("Compare REU %06lX and main memory %04X, %u bytes\n",
                    (unsigned long)xmem_address, (unsigned)c64_address, (unsigned)xfer_size);
            break;
        }
#endif
        do {
            switch (xfer_type) {
            case 0x00: /* Main memory to REU */
                emu_ext_mem[xmem_address] = memory[c64_address];
                ++emu_cycles;
                break;

            case 0x01: /* REU to main memory */
                memory[c64_address] = emu_ext_mem[xmem_address];
                written[c64_address >> 3] |= 0x1 << (c64_address & 0x7);
                ++emu_cycles;
                break;

            case 0x02: /* Swap REU and main memory */
                {
                    uint8_t swap = memory[c64_address];
                    memory[c64_address] = emu_ext_mem[xmem_address];
                    written[c64_address >> 3] |= 0x1 << (c64_address & 0x7);
                    emu_ext_mem[xmem_address] = swap;
                    emu_cycles += 2;
                }
                break;

            case 0x03: /* Compare REU and main memory */
                bad_compare = memory[c64_address] != emu_ext_mem[xmem_address];
                ++emu_cycles;
                break;
            }
            if (!(incr_flags & 0x40)) {
                xmem_address = (xmem_address + 1) & 0xFFFFFFFF;
            }
            if (!(incr_flags & 0x80)) {
                c64_address = (c64_address + 1) & 0xFFFF;
            }
            --xfer_size;
        } while (xfer_size != 0 && !bad_compare);

        /* Update the status register */
        if (bad_compare) {
            reu_regs[reu_status] |= 0x20;
        } else {
            reu_regs[reu_status] &= 0xDF;
        }
        reu_regs[reu_status] |= 0x40;

        /* Update the address and length registers */
        if (cmd & 0x20) { /* autoload */
            reu_regs[reu_c64_address_0] = reu_shadow[reu_c64_address_0];
            reu_regs[reu_c64_address_1] = reu_shadow[reu_c64_address_1];
            reu_regs[reu_xmem_address_1] = reu_shadow[reu_xmem_address_1];
            reu_regs[reu_xmem_address_1] = reu_shadow[reu_xmem_address_1];
            reu_regs[reu_xmem_address_1] = reu_shadow[reu_xmem_address_1];
            reu_regs[reu_xfer_size_0] = reu_shadow[reu_xfer_size_0];
            reu_regs[reu_xfer_size_1] = reu_shadow[reu_xfer_size_1];
        } else {
            reu_regs[reu_c64_address_0] = (uint8_t)(c64_address >>  0);
            reu_regs[reu_c64_address_1] = (uint8_t)(c64_address >>  8);
            reu_regs[reu_xmem_address_1] = (uint8_t)(xmem_address >>  0);
            reu_regs[reu_xmem_address_1] = (uint8_t)(xmem_address >>  8);
            reu_regs[reu_xmem_address_1] = (uint8_t)(xmem_address >> 16);
            reu_regs[reu_xfer_size_0] = (uint8_t)(xfer_size >>  0);
            reu_regs[reu_xfer_size_1] = (uint8_t)(xfer_size >>  8);
        }
        reu_regs[reu_command] &= 0x7F;
    }
}

/* Set up the registers */
void
emu_set_regs(
        uint8_t a,
        uint8_t x,
        uint8_t y,
        uint8_t s,
        uint8_t flags,
        uint16_t pc)
{
    regs.a = a;
    regs.x = x;
    regs.y = y;
    regs.s = s;
    regs.flags = flags;
    regs.pc = pc;
}

/* Read a ROM image */
bool
emu_read_rom(uint16_t address, const char *name)
{
    FILE *fp = fopen(name, "rb");
    if (fp == NULL) {
        return false;
    }
    size_t max_size = sizeof(memory) - address;
    size_t size = fread(memory + address, 1, max_size, fp);
    if (ferror(fp)) {
        fclose(fp);
        return false;
    }
    fclose(fp);
    size_t i = 0;
    while (i < size && ((address + i) & 0x7) != 0) {
        written[(address + i) >> 3] |= 0x1 << ((address + i) & 0x7);
        ++i;
    }
    for (; i + 8 <= size; i += 8) {
        written[(address + i) >> 3] = 0xFF;
    }
    while (i < size) {
        written[(address + i) >> 3] |= 0x1 << ((address + i) & 0x7);
        ++i;
    }
    return true;
}

bool
emu_written(uint16_t address)
{
    return written[address >> 3] & (0x1 << (address & 0x7));
}

static void print_regs(void);
static bool do_invalid(uint8_t opcode);
static bool do_ADC(uint8_t opcode);
static bool do_AND(uint8_t opcode);
static bool do_ASL(uint8_t opcode);
static bool do_ASL_A(uint8_t opcode);
static bool do_BIT(uint8_t opcode);
static bool do_branch(uint8_t opcode);
static bool do_BRK(uint8_t opcode);
static bool do_CLC(uint8_t opcode);
static bool do_CLD(uint8_t opcode);
static bool do_CLI(uint8_t opcode);
static bool do_CLV(uint8_t opcode);
static bool do_CMP(uint8_t opcode);
static bool do_CPX(uint8_t opcode);
static bool do_CPX_imm(uint8_t opcode);
static bool do_CPY(uint8_t opcode);
static bool do_CPY_imm(uint8_t opcode);
static bool do_DEC(uint8_t opcode);
static bool do_DEX(uint8_t opcode);
static bool do_DEY(uint8_t opcode);
static bool do_EOR(uint8_t opcode);
static bool do_INC(uint8_t opcode);
static bool do_INX(uint8_t opcode);
static bool do_INY(uint8_t opcode);
static bool do_JMP_abs(uint8_t opcode);
static bool do_JMP_ind(uint8_t opcode);
static bool do_JSR(uint8_t opcode);
static bool do_LDA(uint8_t opcode);
static bool do_LDX(uint8_t opcode);
static bool do_LDX_imm(uint8_t opcode);
static bool do_LDX_zpy(uint8_t opcode);
static bool do_LDX_aby(uint8_t opcode);
static bool do_LDY(uint8_t opcode);
static bool do_LDY_imm(uint8_t opcode);
static bool do_LSR(uint8_t opcode);
static bool do_LSR_A(uint8_t opcode);
static bool do_NOP(uint8_t opcode);
static bool do_ORA(uint8_t opcode);
static bool do_PHA(uint8_t opcode);
static bool do_PHP(uint8_t opcode);
static bool do_PLA(uint8_t opcode);
static bool do_PLP(uint8_t opcode);
static bool do_ROL(uint8_t opcode);
static bool do_ROL_A(uint8_t opcode);
static bool do_ROR(uint8_t opcode);
static bool do_ROR_A(uint8_t opcode);
static bool do_RTI(uint8_t opcode);
static bool do_RTS(uint8_t opcode);
static bool do_SBC(uint8_t opcode);
static bool do_SEC(uint8_t opcode);
static bool do_SED(uint8_t opcode);
static bool do_SEI(uint8_t opcode);
static bool do_STA(uint8_t opcode);
static bool do_STX(uint8_t opcode);
static bool do_STX_zpy(uint8_t opcode);
static bool do_STY(uint8_t opcode);
static bool do_TAX(uint8_t opcode);
static bool do_TAY(uint8_t opcode);
static bool do_TSX(uint8_t opcode);
static bool do_TXA(uint8_t opcode);
static bool do_TXS(uint8_t opcode);
static bool do_TYA(uint8_t opcode);

/* Run the emulation */
bool
emu_run(void)
{
    typedef bool (*opcode_handler)(uint8_t opcode);
    static const opcode_handler dispatch[256] = {
        do_BRK,     /* 0x00 */
        do_ORA,     /* 0x01 */
        do_invalid, /* 0x02 */
        do_invalid, /* 0x03 */
        do_invalid, /* 0x04 */
        do_ORA,     /* 0x05 */
        do_ASL,     /* 0x06 */
        do_invalid, /* 0x07 */
        do_PHP,     /* 0x08 */
        do_ORA,     /* 0x09 */
        do_ASL_A,   /* 0x0A */
        do_invalid, /* 0x0B */
        do_invalid, /* 0x0C */
        do_ORA,     /* 0x0D */
        do_ASL,     /* 0x0E */
        do_invalid, /* 0x0F */
        do_branch,  /* 0x10 */
        do_ORA,     /* 0x11 */
        do_invalid, /* 0x12 */
        do_invalid, /* 0x13 */
        do_invalid, /* 0x14 */
        do_ORA,     /* 0x15 */
        do_ASL,     /* 0x16 */
        do_invalid, /* 0x17 */
        do_CLC,     /* 0x18 */
        do_ORA,     /* 0x19 */
        do_invalid, /* 0x1A */
        do_invalid, /* 0x1B */
        do_invalid, /* 0x1C */
        do_ORA,     /* 0x1D */
        do_ASL,     /* 0x1E */
        do_invalid, /* 0x1F */
        do_JSR,     /* 0x20 */
        do_AND,     /* 0x21 */
        do_invalid, /* 0x22 */
        do_invalid, /* 0x23 */
        do_BIT,     /* 0x24 */
        do_AND,     /* 0x25 */
        do_ROL,     /* 0x26 */
        do_invalid, /* 0x27 */
        do_PLP,     /* 0x28 */
        do_AND,     /* 0x29 */
        do_ROL_A,   /* 0x2A */
        do_invalid, /* 0x2B */
        do_BIT,     /* 0x2C */
        do_AND,     /* 0x2D */
        do_ROL,     /* 0x2E */
        do_invalid, /* 0x2F */
        do_branch,  /* 0x30 */
        do_AND,     /* 0x31 */
        do_invalid, /* 0x32 */
        do_invalid, /* 0x33 */
        do_invalid, /* 0x34 */
        do_AND,     /* 0x35 */
        do_ROL,     /* 0x36 */
        do_invalid, /* 0x37 */
        do_SEC,     /* 0x38 */
        do_AND,     /* 0x39 */
        do_invalid, /* 0x3A */
        do_invalid, /* 0x3B */
        do_invalid, /* 0x3C */
        do_AND,     /* 0x3D */
        do_ROL,     /* 0x3E */
        do_invalid, /* 0x3F */
        do_RTI,     /* 0x40 */
        do_EOR,     /* 0x41 */
        do_invalid, /* 0x42 */
        do_invalid, /* 0x43 */
        do_invalid, /* 0x44 */
        do_EOR,     /* 0x45 */
        do_LSR,     /* 0x46 */
        do_invalid, /* 0x47 */
        do_PHA,     /* 0x48 */
        do_EOR,     /* 0x49 */
        do_LSR_A,   /* 0x4A */
        do_invalid, /* 0x4B */
        do_JMP_abs, /* 0x4C */
        do_EOR,     /* 0x4D */
        do_LSR,     /* 0x4E */
        do_invalid, /* 0x4F */
        do_branch,  /* 0x50 */
        do_EOR,     /* 0x51 */
        do_invalid, /* 0x52 */
        do_invalid, /* 0x53 */
        do_invalid, /* 0x54 */
        do_EOR,     /* 0x55 */
        do_LSR,     /* 0x56 */
        do_invalid, /* 0x57 */
        do_CLI,     /* 0x58 */
        do_EOR,     /* 0x59 */
        do_invalid, /* 0x5A */
        do_invalid, /* 0x5B */
        do_invalid, /* 0x5C */
        do_EOR,     /* 0x5D */
        do_LSR,     /* 0x5E */
        do_invalid, /* 0x5F */
        do_RTS,     /* 0x60 */
        do_ADC,     /* 0x61 */
        do_invalid, /* 0x62 */
        do_invalid, /* 0x63 */
        do_invalid, /* 0x64 */
        do_ADC,     /* 0x65 */
        do_ROR,     /* 0x66 */
        do_invalid, /* 0x67 */
        do_PLA,     /* 0x68 */
        do_ADC,     /* 0x69 */
        do_ROR_A,   /* 0x6A */
        do_invalid, /* 0x6B */
        do_JMP_ind, /* 0x6C */
        do_ADC,     /* 0x6D */
        do_ROR,     /* 0x6E */
        do_invalid, /* 0x6F */
        do_branch,  /* 0x70 */
        do_ADC,     /* 0x71 */
        do_invalid, /* 0x72 */
        do_invalid, /* 0x73 */
        do_invalid, /* 0x74 */
        do_ADC,     /* 0x75 */
        do_ROR,     /* 0x76 */
        do_invalid, /* 0x77 */
        do_SEI,     /* 0x78 */
        do_ADC,     /* 0x79 */
        do_invalid, /* 0x7A */
        do_invalid, /* 0x7B */
        do_invalid, /* 0x7C */
        do_ADC,     /* 0x7D */
        do_ROR,     /* 0x7E */
        do_invalid, /* 0x7F */
        do_invalid, /* 0x80 */
        do_STA,     /* 0x81 */
        do_invalid, /* 0x82 */
        do_invalid, /* 0x83 */
        do_STY,     /* 0x84 */
        do_STA,     /* 0x85 */
        do_STX,     /* 0x86 */
        do_invalid, /* 0x87 */
        do_DEY,     /* 0x88 */
        do_invalid, /* 0x89 */
        do_TXA,     /* 0x8A */
        do_invalid, /* 0x8B */
        do_STY,     /* 0x8C */
        do_STA,     /* 0x8D */
        do_STX,     /* 0x8E */
        do_invalid, /* 0x8F */
        do_branch,  /* 0x90 */
        do_STA,     /* 0x91 */
        do_invalid, /* 0x92 */
        do_invalid, /* 0x93 */
        do_STY,     /* 0x94 */
        do_STA,     /* 0x95 */
        do_STX_zpy, /* 0x96 */
        do_invalid, /* 0x97 */
        do_TYA,     /* 0x98 */
        do_STA,     /* 0x99 */
        do_TXS,     /* 0x9A */
        do_invalid, /* 0x9B */
        do_invalid, /* 0x9C */
        do_STA,     /* 0x9D */
        do_invalid, /* 0x9E */
        do_invalid, /* 0x9F */
        do_LDY_imm, /* 0xA0 */
        do_LDA,     /* 0xA1 */
        do_LDX_imm, /* 0xA2 */
        do_invalid, /* 0xA3 */
        do_LDY,     /* 0xA4 */
        do_LDA,     /* 0xA5 */
        do_LDX,     /* 0xA6 */
        do_invalid, /* 0xA7 */
        do_TAY,     /* 0xA8 */
        do_LDA,     /* 0xA9 */
        do_TAX,     /* 0xAA */
        do_invalid, /* 0xAB */
        do_LDY,     /* 0xAC */
        do_LDA,     /* 0xAD */
        do_LDX,     /* 0xAE */
        do_invalid, /* 0xAF */
        do_branch,  /* 0xB0 */
        do_LDA,     /* 0xB1 */
        do_invalid, /* 0xB2 */
        do_invalid, /* 0xB3 */
        do_LDY,     /* 0xB4 */
        do_LDA,     /* 0xB5 */
        do_LDX_zpy, /* 0xB6 */
        do_invalid, /* 0xB7 */
        do_CLV,     /* 0xB8 */
        do_LDA,     /* 0xB9 */
        do_TSX,     /* 0xBA */
        do_invalid, /* 0xBB */
        do_LDY,     /* 0xBC */
        do_LDA,     /* 0xBD */
        do_LDX_aby, /* 0xBE */
        do_invalid, /* 0xBF */
        do_CPY_imm, /* 0xC0 */
        do_CMP,     /* 0xC1 */
        do_invalid, /* 0xC2 */
        do_invalid, /* 0xC3 */
        do_CPY,     /* 0xC4 */
        do_CMP,     /* 0xC5 */
        do_DEC,     /* 0xC6 */
        do_invalid, /* 0xC7 */
        do_INY,     /* 0xC8 */
        do_CMP,     /* 0xC9 */
        do_DEX,     /* 0xCA */
        do_invalid, /* 0xCB */
        do_CPY,     /* 0xCC */
        do_CMP,     /* 0xCD */
        do_DEC,     /* 0xCE */
        do_invalid, /* 0xCF */
        do_branch,  /* 0xD0 */
        do_CMP,     /* 0xD1 */
        do_invalid, /* 0xD2 */
        do_invalid, /* 0xD3 */
        do_invalid, /* 0xD4 */
        do_CMP,     /* 0xD5 */
        do_DEC,     /* 0xD6 */
        do_invalid, /* 0xD7 */
        do_CLD,     /* 0xD8 */
        do_CMP,     /* 0xD9 */
        do_invalid, /* 0xDA */
        do_invalid, /* 0xDB */
        do_invalid, /* 0xDC */
        do_CMP,     /* 0xDD */
        do_DEC,     /* 0xDE */
        do_invalid, /* 0xDF */
        do_CPX_imm, /* 0xE0 */
        do_SBC,     /* 0xE1 */
        do_invalid, /* 0xE2 */
        do_invalid, /* 0xE3 */
        do_CPX,     /* 0xE4 */
        do_SBC,     /* 0xE5 */
        do_INC,     /* 0xE6 */
        do_invalid, /* 0xE7 */
        do_INX,     /* 0xE8 */
        do_SBC,     /* 0xE9 */
        do_NOP,     /* 0xEA */
        do_invalid, /* 0xEB */
        do_CPX,     /* 0xEC */
        do_SBC,     /* 0xED */
        do_INC,     /* 0xEE */
        do_invalid, /* 0xEF */
        do_branch,  /* 0xF0 */
        do_SBC,     /* 0xF1 */
        do_invalid, /* 0xF2 */
        do_invalid, /* 0xF3 */
        do_invalid, /* 0xF4 */
        do_SBC,     /* 0xF5 */
        do_INC,     /* 0xF6 */
        do_invalid, /* 0xF7 */
        do_SED,     /* 0xF8 */
        do_SBC,     /* 0xF9 */
        do_invalid, /* 0xFA */
        do_invalid, /* 0xFB */
        do_invalid, /* 0xFC */
        do_SBC,     /* 0xFD */
        do_INC,     /* 0xFE */
        do_invalid  /* 0xFF */
    };

    /* If RTS finds S at this value, exit the function */
    uint8_t initial_s = regs.s;
    /* Exits are errors unless final RTS is encountered */
    bool ok = false;

    while (true) {
        print_regs();
        int opcode = emu_read_byte(regs.pc++);
        if (opcode < 0) {
            break; /* uninitialized memory */
        }
        if (opcode == 0x60 && regs.s == initial_s) {
            /* Function has completed */
            ok = true;
            break;
        }
        if (!(*dispatch[opcode])(opcode)) {
            break; /* error detected in opcode handler */
        }
    }

    debug_printf("Return from emulation\n");
    return ok;
}

static bool
do_invalid(uint8_t opcode)
{
    /* Undocumented opcodes come here */
    fprintf(stderr, "Invalid opcode %02X fetched from %04X\n",
            (unsigned)opcode, (unsigned)regs.pc - 1);
    return false;
}

static void do_add(uint8_t byte);
static int32_t get_address(uint8_t opcode);
static int32_t get_address_zpy(void);
static int32_t get_address_aby(void);
static void compare(uint8_t reg, uint8_t byte);
static void push_byte(uint8_t byte);
static int pop_byte(void);
static void set_nz(uint8_t result);
static void set_z(uint8_t result);
static void set_v(bool carry);
static void set_c(bool carry);

static bool
do_ORA(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int32_t addr = get_address(opcode);
    if (addr < 0) {
        return false;
    }
    int byte = emu_read_byte(addr);
    if (byte < 0) {
        return false;
    }
    regs.a |= byte;
    set_nz(regs.a);
    emu_cycles += 2;
    return true;
}

static bool
do_AND(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int32_t addr = get_address(opcode);
    if (addr < 0) {
        return false;
    }
    int byte = emu_read_byte(addr);
    if (byte < 0) {
        return false;
    }
    regs.a &= byte;
    set_nz(regs.a);
    emu_cycles += 2;
    return true;
}

static bool
do_EOR(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int32_t addr = get_address(opcode);
    if (addr < 0) {
        return false;
    }
    int byte = emu_read_byte(addr);
    if (byte < 0) {
        return false;
    }
    regs.a ^= byte;
    set_nz(regs.a);
    emu_cycles += 2;
    return true;
}

static bool
do_ADC(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int32_t addr = get_address(opcode);
    if (addr < 0) {
        return false;
    }
    int byte = emu_read_byte(addr);
    if (byte < 0) {
        return false;
    }
    emu_cycles += 2;
    do_add(byte);
    return true;
}

static bool
do_STA(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int32_t addr = get_address(opcode);
    if (addr < 0) {
        return false;
    }
    emu_write_byte(addr, regs.a);
    emu_cycles += 2;
    return true;
}

static bool
do_LDA(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int32_t addr = get_address(opcode);
    if (addr < 0) {
        return false;
    }
    int byte = emu_read_byte(addr);
    if (byte < 0) {
        return false;
    }
    regs.a = byte;
    set_nz(regs.a);
    emu_cycles += 2;
    return true;
}

static bool
do_CMP(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int32_t addr = get_address(opcode);
    if (addr < 0) {
        return false;
    }
    int byte = emu_read_byte(addr);
    if (byte < 0) {
        return false;
    }
    compare(regs.a, byte);
    emu_cycles += 2;
    return true;
}

static bool
do_SBC(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int32_t addr = get_address(opcode);
    if (addr < 0) {
        return false;
    }
    int byte = emu_read_byte(addr);
    if (byte < 0) {
        return false;
    }
    if (regs.flags & 0x08) {
        byte = 0x99 - byte;
    } else {
        byte = byte ^ 0xFF;
    }
    do_add(byte);
    emu_cycles += 2;
    return true;
}

/* Common to ADC and SBC */
static void
do_add(uint8_t byte)
{
    int result;
    if (regs.flags & 0x08) {
        /* Decimal mode */
        int r1 = (regs.a & 0x0F) + (byte & 0xF0) + (regs.flags & 0x01);
        if (r1 > 0x09) {
            r1 += 0x06;
        }
        int r2 = (regs.a & 0xF0) + (byte & 0xF0);
        if (r2 > 0x90) {
            r2 += 0x60;
        }
        result = r1 + r2;
    } else {
        /* Binary mode */
        result = regs.a + byte + (regs.flags & 0x01);
        int r7 = (regs.a & 0x7F) + (byte & 0x7F) + (regs.flags & 0x01);
        int overflow = ((r7 << 1) ^ result) & 0x100;
        set_v(overflow);
    }
    regs.a = (uint8_t)result;
    set_nz(result);
    set_c(result > 0xFF);
}

static bool
do_ASL(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int32_t addr = get_address(opcode);
    if (addr < 0) {
        return false;
    }
    int byte = emu_read_byte(addr);
    if (byte < 0) {
        return false;
    }
    uint8_t result = byte << 1;
    emu_write_byte(addr, result);
    set_nz(result);
    set_c((byte & 0x80) != 0);
    emu_cycles += 4;
    return true;
}

static bool
do_LSR(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int32_t addr = get_address(opcode);
    if (addr < 0) {
        return false;
    }
    int byte = emu_read_byte(addr);
    if (byte < 0) {
        return false;
    }
    uint8_t result = byte >> 1;
    emu_write_byte(addr, result);
    set_nz(result);
    set_c((byte & 0x01) != 0);
    emu_cycles += 4;
    return true;
}

static bool
do_ROL(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int32_t addr = get_address(opcode);
    if (addr < 0) {
        return false;
    }
    int byte = emu_read_byte(addr);
    if (byte < 0) {
        return false;
    }
    uint8_t result = (byte << 1) | (regs.flags & 0x01);
    emu_write_byte(addr, result);
    set_nz(result);
    set_c((byte & 0x80) != 0);
    emu_cycles += 4;
    return true;
}

static bool
do_ROR(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int32_t addr = get_address(opcode);
    if (addr < 0) {
        return false;
    }
    int byte = emu_read_byte(addr);
    if (byte < 0) {
        return false;
    }
    uint8_t result = (byte >> 1) | ((regs.flags & 0x01) << 7);
    emu_write_byte(addr, result);
    set_nz(result);
    set_c((byte & 0x01) != 0);
    emu_cycles += 4;
    return true;
}

static bool
do_ASL_A(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    uint8_t byte = regs.a;
    uint8_t result = byte << 1;
    regs.a = result;
    set_nz(result);
    set_c((byte & 0x80) != 0);
    emu_cycles += 2;
    return true;
}

static bool
do_LSR_A(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    uint8_t byte = regs.a;
    uint8_t result = byte >> 1;
    regs.a = result;
    set_nz(result);
    set_c((byte & 0x01) != 0);
    emu_cycles += 2;
    return true;
}

static bool
do_ROL_A(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    uint8_t byte = regs.a;
    uint8_t result = (byte << 1) | (regs.flags & 0x01);
    regs.a = result;
    set_nz(result);
    set_c((byte & 0x80) != 0);
    emu_cycles += 2;
    return true;
}

static bool
do_ROR_A(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    uint8_t byte = regs.a;
    uint8_t result = (byte >> 1) | ((regs.flags & 0x01) << 7);
    regs.a = result;
    set_nz(result);
    set_c((byte & 0x01) != 0);
    emu_cycles += 2;
    return true;
}

static bool
do_BIT(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int32_t addr = get_address(opcode);
    if (addr < 0) {
        return false;
    }
    /* BIT is often used to hide another instruction. If we read an
       uninitialized byte, don't return an error, but treat as zero */
    int byte = memory[addr];
    set_z(byte & regs.a);
    regs.flags = (regs.flags & 0x3F) | (byte & 0xC0);
    emu_cycles += 2;
    return true;
}

static bool
do_branch(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    static const uint8_t flag_bits[] = {
        0x80, /* BPL, BMI */
        0x40, /* BVC, BVS */
        0x01, /* BCC, BCS */
        0x02  /* BNE, BEQ */
    };

    int offset = emu_read_byte(regs.pc++);
    if (offset < 0) {
        return false;
    }
    uint16_t address = regs.pc + (offset ^ 0x80) - 0x80;
    bool which = (opcode & 0x20) != 0;
    bool flag = (regs.flags & flag_bits[opcode >> 6]) != 0;
    emu_cycles += 2;
    if (flag == which) {
        emu_cycles += 1;
        if ((address >> 8) != (regs.pc >> 8)) {
            emu_cycles += 1;
        }
        regs.pc = address;
    }
    return true;
}

static bool
do_BRK(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int byte1 = emu_read_byte(0xFFFE);
    if (byte1 < 0) {
        return false;
    }
    int byte2 = emu_read_byte(0xFFFF);
    if (byte2 < 0) {
        return false;
    }

    regs.pc++; /* skip byte after BRK */
    push_byte(regs.pc >> 8);
    push_byte(regs.pc & 0xFF);
    push_byte(regs.flags);
    regs.flags |= 0x14;
    regs.pc = byte2 * 0x100 + byte1;
    emu_cycles += 7;
    return true;
}

static bool
do_CLC(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    regs.flags &= 0xFE;
    emu_cycles += 2;
    return true;
}

static bool
do_CLD(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    regs.flags &= 0xF7;
    emu_cycles += 2;
    return true;
}

static bool
do_CLI(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    regs.flags &= 0xFB;
    emu_cycles += 2;
    return true;
}

static bool
do_CLV(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    regs.flags &= 0xBF;
    emu_cycles += 2;
    return true;
}

static bool
do_CPX(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int32_t addr = get_address(opcode);
    if (addr < 0) {
        return false;
    }
    int byte = emu_read_byte(addr);
    if (byte < 0) {
        return false;
    }
    compare(regs.x, byte);
    emu_cycles += 2;
    return true;
}

static bool
do_CPX_imm(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int byte = emu_read_byte(regs.pc++);
    if (byte < 0) {
        return false;
    }
    compare(regs.x, byte);
    emu_cycles += 2;
    return true;
}

static bool
do_CPY(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int32_t addr = get_address(opcode);
    if (addr < 0) {
        return false;
    }
    int byte = emu_read_byte(addr);
    if (byte < 0) {
        return false;
    }
    compare(regs.y, byte);
    emu_cycles += 2;
    return true;
}

static bool
do_CPY_imm(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int byte = emu_read_byte(regs.pc++);
    if (byte < 0) {
        return false;
    }
    compare(regs.y, byte);
    emu_cycles += 2;
    return true;
}

static bool
do_DEC(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int32_t addr = get_address(opcode);
    if (addr < 0) {
        return false;
    }
    int byte = emu_read_byte(addr);
    if (byte < 0) {
        return false;
    }
    uint8_t result = byte - 1;
    emu_write_byte(addr, result);
    set_nz(result);
    emu_cycles += 4;
    return true;
}

static bool
do_DEX(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    set_nz(--regs.x);
    emu_cycles += 2;
    return true;
}

static bool
do_DEY(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    set_nz(--regs.y);
    emu_cycles += 2;
    return true;
}

static bool
do_INC(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int32_t addr = get_address(opcode);
    if (addr < 0) {
        return false;
    }
    int byte = emu_read_byte(addr);
    if (byte < 0) {
        return false;
    }
    uint8_t result = byte + 1;
    emu_write_byte(addr, result);
    set_nz(result);
    emu_cycles += 4;
    return true;
}

static bool
do_INX(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    set_nz(++regs.x);
    emu_cycles += 2;
    return true;
}

static bool
do_INY(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    set_nz(++regs.y);
    emu_cycles += 2;
    return true;
}

static bool
do_JMP_abs(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int byte1 = emu_read_byte(regs.pc++);
    if (byte1 < 0) {
        return false;
    }
    int byte2 = emu_read_byte(regs.pc++);
    if (byte2 < 0) {
        return false;
    }
    uint16_t addr = byte2 * 0x100 + byte1;
#ifndef TEST
    if (VM_kernal(addr, &regs.a, &regs.x, &regs.y)) {
        emu_cycles += 3;
        do_RTS(0x60);
        return true;
    }
#endif
    regs.pc = addr;
    emu_cycles += 3;
    return true;
}

static bool
do_JMP_ind(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int byte1 = emu_read_byte(regs.pc++);
    if (byte1 < 0) {
        return false;
    }
    int byte2 = emu_read_byte(regs.pc++);
    if (byte2 < 0) {
        return false;
    }
    uint16_t addr = byte2 * 0x100 + byte1;
    int byte3 = emu_read_byte(addr);
    if (byte3 < 0) {
        return false;
    }
    /*
     * The original 6502 has a bug in this instruction, where a vector at an
     * address ending in FF will wrap around to the start of the 256 byte page
     * instead of advancing to the next page. This bug is emulated here.
     */
    ++byte1;
    addr = byte2 * 0x100 + byte1;
    int byte4 = emu_read_byte(addr);
    if (byte4 < 0) {
        return false;
    }
    regs.pc = byte4 * 0x100 + byte3;
    emu_cycles += 5;
    return true;
}

static bool
do_JSR(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int byte1 = emu_read_byte(regs.pc++);
    if (byte1 < 0) {
        return false;
    }
    /*
     * regs.pc is not incremented, because JSR pushes the PC address of the
     * last byte of the instruction, not the first byte of the next
     * instruction
     */
    int byte2 = emu_read_byte(regs.pc);
    if (byte2 < 0) {
        return false;
    }
    uint16_t addr = byte2 * 0x100 + byte1;
#ifndef TEST
    if (VM_kernal(addr, &regs.a, &regs.x, &regs.y)) {
        emu_cycles += 6;
        ++regs.pc;
        return true;
    }
#endif
    push_byte(regs.pc >> 8);
    push_byte(regs.pc & 0xFF); 
    debug_printf("JSR to %04X\n", addr);
    regs.pc = addr;
    emu_cycles += 6;
    return true;
}

static bool
do_LDX(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int32_t addr = get_address(opcode);
    if (addr < 0) {
        return false;
    }
    int byte = emu_read_byte(addr);
    if (byte < 0) {
        return false;
    }
    regs.x = byte;
    set_nz(regs.x);
    emu_cycles += 2;
    return true;
}

static bool
do_LDX_imm(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int byte = emu_read_byte(regs.pc++);
    if (byte < 0) {
        return false;
    }
    regs.x = byte;
    set_nz(regs.x);
    emu_cycles += 2;
    return true;
}

static bool
do_LDX_zpy(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int32_t addr = get_address_zpy();
    if (addr < 0) {
        return false;
    }
    int byte = emu_read_byte(addr);
    if (byte < 0) {
        return false;
    }
    regs.x = byte;
    set_nz(regs.x);
    emu_cycles += 2;
    return true;
}

static bool
do_LDX_aby(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int32_t addr = get_address_aby();
    if (addr < 0) {
        return false;
    }
    int byte = emu_read_byte(addr);
    if (byte < 0) {
        return false;
    }
    regs.x = byte;
    set_nz(regs.x);
    emu_cycles += 2;
    return true;
}

static bool
do_LDY(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int32_t addr = get_address(opcode);
    if (addr < 0) {
        return false;
    }
    int byte = emu_read_byte(addr);
    if (byte < 0) {
        return false;
    }
    regs.y = byte;
    set_nz(regs.y);
    emu_cycles += 2;
    return true;
}

static bool
do_LDY_imm(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int byte = emu_read_byte(regs.pc++);
    if (byte < 0) {
        return false;
    }
    regs.y = byte;
    set_nz(regs.y);
    emu_cycles += 2;
    return true;
}

static bool
do_NOP(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    emu_cycles += 2;
    return true;
}

static bool
do_PHA(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    push_byte(regs.a);
    emu_cycles += 3;
    return true;
}

static bool
do_PHP(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    push_byte(regs.flags);
    emu_cycles += 3;
    return true;
}

static bool
do_PLA(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int byte = pop_byte();
    if (byte < 0) {
        return false;
    }
    regs.a = byte;
    set_nz(regs.a);
    emu_cycles += 4;
    return true;
}

static bool
do_PLP(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int byte = pop_byte();
    if (byte < 0) {
        return false;
    }
    regs.flags = byte;
    emu_cycles += 4;
    return true;
}

static bool
do_RTI(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int byte1 = pop_byte();
    if (byte1 < 0) {
        return false;
    }
    regs.flags = byte1;
    int byte2 = pop_byte();
    if (byte2 < 0) {
        return false;
    }
    int byte3 = pop_byte();
    if (byte3 < 0) {
        return false;
    }
    regs.pc = byte3 * 0x100 + byte2;
    emu_cycles += 6;
    return true;
}

static bool
do_RTS(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int byte1 = pop_byte();
    if (byte1 < 0) {
        return false;
    }
    int byte2 = pop_byte();
    if (byte2 < 0) {
        return false;
    }
    regs.pc = byte2 * 0x100 + byte1 + 1;
    emu_cycles += 6;
    return true;
}

static bool
do_SEC(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    regs.flags |= 0x01;
    emu_cycles += 2;
    return true;
}

static bool
do_SED(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    regs.flags |= 0x08;
    emu_cycles += 2;
    return true;
}

static bool
do_SEI(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    regs.flags |= 0x04;
    emu_cycles += 2;
    return true;
}

static bool
do_STX(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int32_t addr = get_address(opcode);
    if (addr < 0) {
        return false;
    }
    emu_write_byte(addr, regs.x);
    emu_cycles += 2;
    return true;
}

static bool
do_STX_zpy(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int32_t addr = get_address_zpy();
    if (addr < 0) {
        return false;
    }
    emu_write_byte(addr, regs.x);
    emu_cycles += 2;
    return true;
}

static bool
do_STY(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    int32_t addr = get_address(opcode);
    if (addr < 0) {
        return false;
    }
    emu_write_byte(addr, regs.y);
    emu_cycles += 2;
    return true;
}

static bool
do_TAX(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    regs.x = regs.a;
    set_nz(regs.x);
    emu_cycles += 2;
    return true;
}

static bool
do_TAY(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    regs.y = regs.a;
    set_nz(regs.y);
    emu_cycles += 2;
    return true;
}

static bool
do_TSX(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    regs.x = regs.s;
    set_nz(regs.x);
    emu_cycles += 2;
    return true;
}

static bool
do_TXA(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    regs.a = regs.x;
    set_nz(regs.a);
    emu_cycles += 2;
    return true;
}

static bool
do_TXS(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    regs.s = regs.x;
    emu_cycles += 2;
    return true;
}

static bool
do_TYA(uint8_t opcode)
{
    debug_printf("%s\n", __func__);
    regs.a = regs.y;
    set_nz(regs.a);
    emu_cycles += 2;
    return true;
}

/*
 * Opcodes ending in 01 have a common structure
 * Others using this are:
 * - ASL, LSR, ROL and ROR operating on memory
 * - BIT, LDX and STX in zero page and absolute modes
 * - CPX, CPY and LDY except for immediate
 * - DEC, INC and STY
 */
static int32_t
get_address(uint8_t opcode)
{
    switch (opcode & 0x1C) {
    case 0x00:
        /* ABC (zp, X) */
        {
            int byte1 = emu_read_byte(regs.pc++);
            if (byte1 < 0) {
                return -1;
            }
            uint8_t zp_addr = byte1 + regs.x;
            /*zp_addr++, not zp_addr + 1, because read from 0xFF wraps to 0x00 */
            int byte2 = emu_read_byte(zp_addr++);
            if (byte2 < 0) {
                return -1;
            }
            int byte3 = emu_read_byte(zp_addr);
            if (byte3 < 0) {
                return -1;
            }
            emu_cycles += 4;
            return byte3 * 0x100 + byte2;
        }

    case 0x04:
        /* ABC zp */
        {
            int byte1 = emu_read_byte(regs.pc++);
            if (byte1 < 0) {
                return -1;
            }
            emu_cycles += 1;
            return byte1;
        }

    case 0x08:
        /* ABC #imm */
        return regs.pc++;

    case 0x0C:
        /* ABC abs */
        {
            int byte1 = emu_read_byte(regs.pc++);
            if (byte1 < 0) {
                return -1;
            }
            int byte2 = emu_read_byte(regs.pc++);
            if (byte2 < 0) {
                return -1;
            }
            emu_cycles += 2;
            return byte2 * 0x100 + byte1;
        }

    case 0x10:
        /* ABC (zp), Y */
        {
            int byte1 = emu_read_byte(regs.pc++);
            if (byte1 < 0) {
                return -1;
            }
            uint8_t zp_addr = byte1;
            /*zp_addr++, not zp_addr + 1, because read from 0xFF wraps to 0x00 */
            int byte2 = emu_read_byte(zp_addr++);
            if (byte2 < 0) {
                return -1;
            }
            int byte3 = emu_read_byte(zp_addr);
            if (byte3 < 0) {
                return -1;
            }
            uint16_t addr1 = byte3 * 0x100 + byte2;
            uint16_t addr2 = addr1 + regs.y;
            emu_cycles += 3;
            if ((addr2 >> 8) != (addr1 >> 8)) {
                emu_cycles += 1;
            }
            return addr2;
        }

    case 0x14:
        /* ABC zp, X */
        {
            int byte1 = emu_read_byte(regs.pc++);
            if (byte1 < 0) {
                return -1;
            }
            emu_cycles += 2;
            /* Overflow past zero page wraps to zero */
            return (byte1 + regs.x) & 0xFF;
        }

    case 0x18:
        /* ABC abs, Y */
        return get_address_aby();

    case 0x1C:
        /* ABC abs, X */
        {
            int byte1 = emu_read_byte(regs.pc++);
            if (byte1 < 0) {
                return -1;
            }
            int byte2 = emu_read_byte(regs.pc++);
            if (byte2 < 0) {
                return -1;
            }
            uint16_t addr1 = byte2 * 0x100 + byte1;
            uint16_t addr2 = addr1 + regs.x;
            emu_cycles += 2;
            if ((addr2 >> 8) != (addr1 >> 8)) {
                emu_cycles += 1;
            }
            return addr2;
        }

    default: /* shouldn't happen */
        return -1;
    }
}

static int32_t
get_address_zpy(void)
{
    int byte1 = emu_read_byte(regs.pc++);
    if (byte1 < 0) {
        return -1;
    }
    /* Overflow past zero page wraps to zero */
    emu_cycles += 2;
    return (byte1 + regs.y) & 0xFF;
}

static int32_t
get_address_aby(void)
{
    int byte1 = emu_read_byte(regs.pc++);
    if (byte1 < 0) {
        return -1;
    }
    int byte2 = emu_read_byte(regs.pc++);
    if (byte2 < 0) {
        return -1;
    }
    uint16_t addr1 = byte2 * 0x100 + byte1;
    uint16_t addr2 = addr1 + regs.y;
    emu_cycles += 2;
    if ((addr2 >> 8) != (addr1 >> 8)) {
        emu_cycles += 1;
    }
    return addr2;
}

static void
compare(uint8_t reg, uint8_t byte)
{
    int result = reg + (byte ^ 0xFF) + 1;
    set_nz(result);
    set_c(result > 0xFF);
}

static void
push_byte(uint8_t byte)
{
    uint16_t address = 0x0100 + --regs.s;
    emu_write_byte(address, byte);
}

static int
pop_byte(void)
{
    uint16_t address = 0x0100 + regs.s++;
    return emu_read_byte(address);
}

static void
set_nz(uint8_t result)
{
    regs.flags = (regs.flags & 0x7F) | (result & 0x80);
    set_z(result);
}

static void
set_z(uint8_t result)
{
    if (result == 0) {
        regs.flags |= 0x02;
    } else {
        regs.flags &= 0xFD;
    }
}

static void
set_v(bool overflow)
{
    if (overflow) {
        regs.flags |= 0x40;
    } else {
        regs.flags &= 0xBF;
    }
}

static void
set_c(bool carry)
{
    if (carry) {
        regs.flags |= 0x01;
    } else {
        regs.flags &= 0xFE;
    }
}

static void
print_regs(void)
{
    debug_printf("PC=%04X A=%02X X=%02X Y=%02X S=%02X %c%c%c%c%c%c%c%c\n",
        regs.pc,
        regs.a, regs.x, regs.y, regs.s,
        (regs.flags & 0x80) ? 'N' : 'n',
        (regs.flags & 0x40) ? 'V' : 'v',
        (regs.flags & 0x20) ? 'X' : 'x',
        (regs.flags & 0x10) ? 'B' : 'b',
        (regs.flags & 0x08) ? 'D' : 'd',
        (regs.flags & 0x04) ? 'I' : 'i',
        (regs.flags & 0x02) ? 'Z' : 'z',
        (regs.flags & 0x01) ? 'C' : 'c');
}
