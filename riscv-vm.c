/* riscv-vm.c */

#include <ctype.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "6502emu.h"

/* Settings for RISC-V registers: */
/* Entry point, lower three bytes (upper byte is 0x01) */
#define RISCV_ENTRY 0x07F0
/* Fence, byte 2 (bytes 0 and 1 are zero; byte 3 is 0x01) */
#define RISCV_FENCE 0x07F3
/* Break, lower three bytes (upper byte is 0x01) */
#define RISCV_BREAK 0x07F4
/* Initial stack pointer, lower three bytes (upper byte is 0x01) */
#define RISCV_STACK 0x07F7

/* Types for interpretation of ELF binaries */
/* Descriptions of structure fields are from
 * https://en.wikipedia.org/w/index.php?title=Executable_and_Linkable_Format&oldid=1053259331
 */
typedef struct ELF_file_header {
    uint8_t e_ident[16];
    uint16_t e_type;      /* Identifies object file type. */
    uint16_t e_machine;   /* Specifies target instruction set architecture. Some examples are: */
    uint32_t e_version;   /* Set to 1 for the original version of ELF. */
    uint32_t e_entry;     /* This is the memory address of the entry point from where the process starts executing. This field is either 32 or 64 bits long depending on the format defined earlier. */
    uint32_t e_phoff;     /* Points to the start of the program header table. It usually follows the file header immediately, making the offset 0x34 or 0x40 for 32- and 64-bit ELF executables, respectively. */
    uint32_t e_shoff;     /* Points to the start of the section header table. */
    uint32_t e_flags;     /* Interpretation of this field depends on the target architecture. */
    uint16_t e_ehsize;    /* Contains the size of this header, normally 64 Bytes for 64-bit and 52 Bytes for 32-bit format. */
    uint16_t e_phentsize; /* Contains the size of a program header table entry. */
    uint16_t e_phnum;     /* Contains the number of entries in the program header table. */
    uint16_t e_shentsize; /* Contains the size of a section header table entry. */
    uint16_t e_shnum;     /* Contains the number of entries in the section header table. */
    uint16_t e_shstrndx;  /* Contains index of the section header table entry that contains the section names. */
} ELF_file_header;

enum e_ident_index {
    EI_MAG0,    /* 0x7F followed by ELF(45 4c 46) in ASCII; these four bytes constitute the magic number. */
    EI_MAG1,
    EI_MAG2,
    EI_MAG3,
    EI_CLASS,   /* This byte is set to either 1 or 2 to signify 32- or 64-bit format, respectively. */
    EI_DATA,    /* This byte is set to either 1 or 2 to signify little or big endianness, respectively. */
    EI_VERSION, /* Set to 1 for the original and current version of ELF. */
    EI_OSABI,   /* Identifies the target operating system ABI. */
    EI_ABIVERSION,
    EI_PAD      /* Currently unused, should be filled with zeros. */
};

typedef struct ELF_program_header {
    uint32_t p_type;        /* Identifies the type of the segment. */
    uint32_t p_offset;      /* Offset of the segment in the file image. */
    uint32_t p_vaddr;       /* Virtual address of the segment in memory. */
    uint32_t p_paddr;       /* Physical address of the segment in memory. */
    uint32_t p_filesz;      /* Size in bytes of the segment in the file image. May be 0. */
    uint32_t p_memsz;       /* Size in bytes of the segment in memory. May be 0. */
    uint32_t p_flags;       /* Segment-dependent flags (position for 32-bit structure). */
    uint32_t p_align;       /* A positive, integral power of 2, with p_vaddr equating p_offset modulus p_align. */
} ELF_program_header;

static void VM_print_char(uint8_t ch);

/* Entry point for 6502 code */
static uint16_t c6502_entry;

bool
VM_init(void)
{
    emu_init();

    /* Read the 6502 code into the emulator's address space */
    const char *filename = "riscv65";
    FILE *fp = fopen(filename, "rb");
    if (fp == NULL) {
        goto error;
    }

    int lo = fgetc(fp);
    int hi = fgetc(fp);
    if (lo < 0 || hi < 0) {
        goto error;
    }
    unsigned addr = (hi << 8) | lo;
    c6502_entry = addr;

    while (addr < 0x10000) {
        int byte = fgetc(fp);
        if (ferror(fp)) {
            goto error;
        }
        if (feof(fp)) {
            break;
        }
        emu_write_byte(addr++, byte);
    }
    fclose(fp);

    /* Set the zero page, so the save/restore routines don't report
     * uninitialized memory */
    for (unsigned i = 0; i < 0x90; ++i) {
        emu_write_byte(i, i);
    }

    return true;

error:
    if (fp == NULL) {
        perror(filename);
    } else {
        if (ferror(fp)) {
            perror(filename);
        } else if (feof(fp)) {
            fprintf(stderr, "%s: unexpected end of file\n", filename);
        }
        fclose(fp);
    }
    return false;
}

/* Load the RISC-V binary */
bool
VM_load(const char *filename)
{
    FILE *fp = fopen(filename, "r");
    if (fp == NULL) {
        goto error;
    }

    /* Read the file header */
    ELF_file_header fhdr;
    if (fread(&fhdr, sizeof(fhdr), 1, fp) != 1) {
        goto error;
    }
    /* Check for correct format */
    if (memcmp(fhdr.e_ident, "\177ELF", 4) != 0) {
        fprintf(stderr, "%s: Not in ELF format\n", filename);
        goto error;
    }
    if (fhdr.e_ident[EI_CLASS] != 1         /* 32 bit */
        || fhdr.e_ident[EI_DATA] != 1       /* Little endian */
        || fhdr.e_type != 2                 /* Executable */
        || fhdr.e_machine != 0xF3           /* RISC-V */
        || fhdr.e_version != 1) {           /* ELF version */
        fprintf(stderr, "%s: Unsupported system\n", filename);
        goto error;
    }

    /* Get the program entry point and check that it is valid */
    uint32_t riscv_entry = fhdr.e_entry;
    if (riscv_entry < 0x01000000 || 0x01FFFFFF < riscv_entry) {
        fprintf(stderr, "%s: Program entry point out of range: 0x%08" PRIX32 "\n",
                filename, riscv_entry);
        goto error;
    }

    /* Get the locations of the program areas */
    uint32_t riscv_break = 0;
    uint32_t max_readonly = 0;
    uint32_t min_writable = 0xFFFFFFFF;
    for (unsigned i = 0; i < fhdr.e_phnum; ++i) {
        fseek(fp, fhdr.e_phoff + fhdr.e_phentsize*i, SEEK_SET);
        ELF_program_header phdr;
        if (fread(&phdr, sizeof(phdr), 1, fp) != 1) {
            goto error;
        }
        switch (phdr.p_type) {
        case 0: /* PT_NULL */
            continue;

        case 1: /* PT_LOAD */
            {
                if (phdr.p_memsz == 0) {
                    continue;
                }

                /* Check that the segment loads to valid addresses */
                uint32_t end_addr = phdr.p_vaddr + phdr.p_memsz;
                if (end_addr < phdr.p_vaddr || phdr.p_vaddr < 0x01000000 || 0x02000000 < end_addr) {
                    fprintf(stderr, "%s: Program header %u loads to invalid addresses (0x%08" PRIX32 "-0x%08" PRIX32 ")\n",
                            filename, i, phdr.p_vaddr, end_addr-1);
                    goto error;
                }

                /* Check for valid write and executable flags */
                /* Read flag is ignored; all areas are considered readable */
                switch (phdr.p_flags & 0x3) {
                case 0: /* read-only */
                case 1: /* read and execute, or execute-only */
                    /* Read-only area; we'll allow execution also */
                    if (max_readonly < end_addr) {
                        max_readonly = end_addr;
                    }
                    break;

                case 2: /* read and write, or write-only */
                    if (min_writable > phdr.p_vaddr) {
                        min_writable = phdr.p_vaddr;
                    }
                    break;

                case 3: /* write and execute both allowed */
                    fprintf(stderr, "%s: Program header %u allows both write and execute\n",
                            filename, i);
                    goto error;
                }

                /* This will be the end of loaded content */
                if (riscv_break < end_addr) {
                    riscv_break = end_addr;
                }

                /* Load program data ento emu_ext_mem */
                fseek(fp, phdr.p_offset, SEEK_SET);
                if (fread(emu_ext_mem + (phdr.p_vaddr - 0x01000000),
                          1, phdr.p_filesz, fp) != phdr.p_filesz) {
                    goto error;
                }
                if (phdr.p_memsz < phdr.p_filesz) {
                    fprintf(stderr, "%s: Program header %u has memory size (0x%08" PRIX32 ") less than file size (0x%08" PRIX32 ")\n",
                            filename, i, phdr.p_memsz, phdr.p_filesz);
                    goto error;
                }
                memset(emu_ext_mem + (phdr.p_vaddr - 0x01000000 + phdr.p_filesz),
                       0, phdr.p_memsz - phdr.p_filesz);
            }
            break;

        default:
            fprintf(stderr, "%s: program header %u has unsupported type 0x%08" PRIX32 "\n",
                    filename, i, phdr.p_type);
            goto error;
        }
    }

    if (max_readonly == 0) {
        fprintf(stderr, "%s: No readable program headers\n", filename);
        goto error;
    }

    /* Set the fence address */
    uint32_t riscv_fence = (max_readonly + 0xFFFF) & 0xFFFF0000;
    uint32_t fence2 = min_writable & 0xFFFF0000;
    if (fence2 < riscv_fence) {
        fprintf(stderr, "%s: Cannot place fence; max_readonly=0x%08" PRIX32 " min_writable=0x%08" PRIX32 "\n",
                filename, max_readonly, min_writable);
        goto error;
    }

    fclose(fp);

    /* Write the initial stack (parameters to main) */
    uint32_t riscv_stack = 0x02000000;
    /* File name */
    size_t len = strlen(filename) + 1;
    len = (len + 3) & 0xFFFFFFFC;
    riscv_stack -= len;
    strcpy((char *)(emu_ext_mem + (riscv_stack - 0x01000000)), filename);
    uint32_t argv0 = riscv_stack;
    /* argv contents */
    riscv_stack -= 8;
    emu_ext_mem[(riscv_stack+0)-0x01000000] = (argv0 >>  0) & 0xFF;
    emu_ext_mem[(riscv_stack+1)-0x01000000] = (argv0 >>  8) & 0xFF;
    emu_ext_mem[(riscv_stack+2)-0x01000000] = (argv0 >> 16) & 0xFF;
    emu_ext_mem[(riscv_stack+3)-0x01000000] = (argv0 >> 24) & 0xFF;
    emu_ext_mem[(riscv_stack+4)-0x01000000] = 0;
    emu_ext_mem[(riscv_stack+5)-0x01000000] = 0;
    emu_ext_mem[(riscv_stack+6)-0x01000000] = 0;
    emu_ext_mem[(riscv_stack+7)-0x01000000] = 0;
    uint32_t argv = riscv_stack;
    /* argv address */
    riscv_stack -= 4;
    emu_ext_mem[(riscv_stack+0)-0x01000000] = (argv >>  0) & 0xFF;
    emu_ext_mem[(riscv_stack+1)-0x01000000] = (argv >>  8) & 0xFF;
    emu_ext_mem[(riscv_stack+2)-0x01000000] = (argv >> 16) & 0xFF;
    emu_ext_mem[(riscv_stack+3)-0x01000000] = (argv >> 24) & 0xFF;
    /* argc */
    riscv_stack -= 4;
    emu_ext_mem[(riscv_stack+0)-0x01000000] = 1;
    emu_ext_mem[(riscv_stack+1)-0x01000000] = 0;
    emu_ext_mem[(riscv_stack+2)-0x01000000] = 0;
    emu_ext_mem[(riscv_stack+3)-0x01000000] = 0;

    /* Set up the parameters for the 6502 code: */
    /* Entry point */
    emu_write_byte(RISCV_ENTRY+0, (riscv_entry >>  0) & 0xFF);
    emu_write_byte(RISCV_ENTRY+1, (riscv_entry >>  8) & 0xFF);
    emu_write_byte(RISCV_ENTRY+2, (riscv_entry >> 16) & 0xFF);
    /* Fence */
    emu_write_byte(RISCV_FENCE, (riscv_fence >> 16) & 0xFF);
    /* Break */
    emu_write_byte(RISCV_BREAK+0, (riscv_break >>  0) & 0xFF);
    emu_write_byte(RISCV_BREAK+1, (riscv_break >>  8) & 0xFF);
    emu_write_byte(RISCV_BREAK+2, (riscv_break >> 16) & 0xFF);
    /* Stack pointer */
    emu_write_byte(RISCV_STACK+0, (riscv_stack >>  0) & 0xFF);
    emu_write_byte(RISCV_STACK+1, (riscv_stack >>  8) & 0xFF);
    emu_write_byte(RISCV_STACK+2, (riscv_stack >> 16) & 0xFF);

    return true;

error:
    if (fp == NULL || ferror(fp)) {
        perror(filename);
    } else if (feof(fp)) {
        fprintf(stderr, "%s: Unexpected end of file\n", filename);
    }
    if (fp != NULL) {
        fclose(fp);
    }
    return false;
}

/* Run the emulation until it halts */
void
VM_run(void)
{
    bool ok;

    emu_set_regs(0, 0, 0, 0xF7, 0, c6502_entry);
    emu_cycles = 0;
    ok = emu_run();
    printf(" %lu cycles %s\n", emu_cycles, ok ? "OK" : "FAIL");
}

bool
VM_kernal(uint16_t addr, uint8_t *a, uint8_t *x, uint8_t *y)
{
    switch (addr) {
    case 0xFFD2: // CHROUT
        VM_print_char(*a);
        break;

    default:
        return false;
    }

    return true;
}

/* Print a character as requested by the 6502 code */
static void
VM_print_char(uint8_t ch)
{
    /* Crude PETSCII to ASCII conversion */
    if (0x41 <= ch && ch <= 0x5A) {
        ch |= 0x20;
    } else if (0xC1 <= ch && ch <= 0xDA) {
        ch &= 0x7F;
    }
    fputc(ch, stdout);
    if (ch == '\r') {
        fputc('\n', stdout);
    }
}

int
main(void)
{
    if (!VM_init()) {
        return EXIT_FAILURE;
    }
    if (!VM_load("hello")) {
        return EXIT_FAILURE;
    }
    VM_run();
    FILE *fp = fopen("riscv-core", "wb");
    if (fp) {
        fwrite(emu_ext_mem, 1, sizeof(emu_ext_mem), fp);
        fclose(fp);
    }
    return EXIT_SUCCESS;
}
