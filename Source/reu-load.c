// reu-load.c

#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "chain-prg.h"

#define PROGRAM "/usb0/hello"
#define FILESYSTEM "/usb0/filesys.img"

/* Settings for RISC-V registers: */
/* Entry point, lower three bytes (upper byte is 0x01) */
#define RISCV_ENTRY 0x07F0
/* Fence, byte 2 (bytes 0 and 1 are zero; byte 3 is 0x01) */
#define RISCV_FENCE 0x07F3
/* Break, lower three bytes (upper byte is 0x01) */
#define RISCV_BREAK 0x07F4
/* Initial stack pointer, lower three bytes (upper byte is 0x01) */
#define RISCV_STACK 0x07F7

// Chain program is copied to this unused area
#define CHAIN_ADDR 0x02A7

//////////////////////////////////////////////////////////////////////////////
//                     1541-Ultimate command interface                      //
//////////////////////////////////////////////////////////////////////////////

// Addresses for command interface
#define CMD_CONTROL        (*(uint8_t volatile *)0xDF1C)
#define CMD_STATUS         (*(uint8_t const volatile *)0xDF1C)
#define CMD_DATA           (*(uint8_t volatile *)0xDF1D)
#define CMD_IDENTIFICATION (*(uint8_t const volatile *)0xDF1D)
#define CMD_RESPONSE_DATA  (*(uint8_t const volatile *)0xDF1E)
#define CMD_STATUS_DATA    (*(uint8_t const volatile *)0xDF1F)

// CMD_CONTROL bits
enum {
    CMD_PUSH_CMD = 0x01,
    CMD_DATA_ACC = 0x02,
    CMD_ABORT    = 0x04,
    CMD_CLR_ERR  = 0x08
};

// CMD_STATUS bits
enum {
    CMD_BUSY       = 0x01,
//  CMD_DATA_ACC   = 0x02,
    CMD_ABORT_P    = 0x04,
    CMD_ERROR      = 0x08,
    CMD_STATE_MASK = 0x30,
    CMD_STATE_IDLE = 0x00,
    CMD_STATE_BUSY = 0x10,
    CMD_STATE_LAST = 0x20,
    CMD_STATE_MORE = 0x30,
    CMD_STAT_AV    = 0x40,
    CMD_DATA_AV    = 0x80
};

// Structure for command and response
typedef struct CMD_struct {
    uint8_t  target;        // out
    uint8_t  command;       // out
    uint16_t data_size;     // in-out
    uint8_t  data[512];     // in-out
    uint8_t  status_size;   // in
    uint8_t  status[128];   // in
} CMD_struct;

// Check for the presence of the command interface
static bool
cmd_present(void)
{
    return CMD_IDENTIFICATION == 0xC9;
}

static void
cmd_abort(void)
{
    if (cmd_present()) {
        CMD_CONTROL = CMD_CLR_ERR | CMD_ABORT;
        while ((CMD_STATUS & CMD_ABORT) != 0) {}
    }
}

static bool
cmd_xfer(CMD_struct *cmd)
{
    uint16_t i;
    uint16_t dsize;
    uint8_t ssize;

    // Send command
    CMD_DATA = cmd->target;
    CMD_DATA = cmd->command;
    for (i = 0; i < cmd->data_size; ++i) {
        CMD_DATA = cmd->data[i];
    }
    CMD_CONTROL = CMD_PUSH_CMD;

    // Wait for completion
    while ((CMD_STATUS & CMD_STATE_MASK) == CMD_STATE_BUSY) {}

    // Read data response
    dsize = 0;
    while (dsize < sizeof(cmd->data) && (CMD_STATUS & CMD_DATA_AV) != 0) {
        cmd->data[dsize++] = CMD_RESPONSE_DATA;
    }
    cmd->data_size = dsize;
    // Read status response
    ssize = 0;
    cmd->status[0] = '\0';
    while (ssize < sizeof(cmd->status) && (CMD_STATUS & CMD_STAT_AV) != 0) {
        cmd->status[ssize++] = CMD_STATUS_DATA;
    }
    cmd->status_size = ssize;
    CMD_CONTROL = CMD_DATA_ACC;
    return true;
}

//////////////////////////////////////////////////////////////////////////////
//                            RAM Expansion Unit                            //
//////////////////////////////////////////////////////////////////////////////

// REU registers
#define REU_STATUS          (*(uint8_t const volatile *)0xDF00)
#define REU_COMMAND         (*(uint8_t volatile *)0xDF01)
#define REU_C64_ADDRESS_0   (*(uint8_t volatile *)0xDF02)
#define REU_C64_ADDRESS_1   (*(uint8_t volatile *)0xDF03)
#define REU_XMEM_ADDRESS_0  (*(uint8_t volatile *)0xDF04)
#define REU_XMEM_ADDRESS_1  (*(uint8_t volatile *)0xDF05)
#define REU_XMEM_ADDRESS_2  (*(uint8_t volatile *)0xDF06)
#define REU_XFER_SIZE_0     (*(uint8_t volatile *)0xDF07)
#define REU_XFER_SIZE_1     (*(uint8_t volatile *)0xDF08)
#define REU_IRQ_MASK        (*(uint8_t volatile *)0xDF09)
#define REU_ADDRESS_CONTROL (*(uint8_t volatile *)0xDF0A)

// Detect the presence of a RAM Expansion Unit
static bool
reu_present(void)
{
    REU_C64_ADDRESS_0 = 0x55;
    if (REU_C64_ADDRESS_0 != 0x55) {
        return false;
    }
    REU_C64_ADDRESS_0 = 0xAA;
    if (REU_C64_ADDRESS_0 != 0xAA) {
        return false;
    }
    return true;
}

// Write to REU
static uint8_t
reu_write(uint32_t reu_address, void const *data, size_t size)
{
    uint8_t i = REU_STATUS;
    REU_C64_ADDRESS_0 = (uint16_t)data & 0xFF;
    REU_C64_ADDRESS_1 = (uint16_t)data >> 8;
    REU_XMEM_ADDRESS_0 = reu_address & 0xFF;
    REU_XMEM_ADDRESS_1 = (reu_address >> 8) & 0xFF;
    REU_XMEM_ADDRESS_2 = reu_address >> 16;
    REU_XFER_SIZE_0 = size & 0xFF;
    REU_XFER_SIZE_1 = size >> 8;
    REU_IRQ_MASK = 0;
    REU_ADDRESS_CONTROL = 0;
    REU_COMMAND = 0x90;
    i = REU_STATUS;
    return i;
}

// Fill REU
static uint8_t
reu_fill(uint32_t reu_address, uint8_t byte, uint32_t size)
{
    uint8_t i = REU_STATUS;
    while (size != 0) {
        uint16_t psize = (size > 0xFFFF) ? 0xFFFF : size;
        REU_C64_ADDRESS_0 = (uint16_t)&byte & 0xFF;
        REU_C64_ADDRESS_1 = (uint16_t)&byte >> 8;
        REU_XMEM_ADDRESS_0 = reu_address & 0xFF;
        REU_XMEM_ADDRESS_1 = (reu_address >> 8) & 0xFF;
        REU_XMEM_ADDRESS_2 = reu_address >> 16;
        REU_XFER_SIZE_0 = psize & 0xFF;
        REU_XFER_SIZE_1 = psize >> 8;
        REU_IRQ_MASK = 0;
        REU_ADDRESS_CONTROL = 0x80;
        REU_COMMAND = 0x90;
        i = REU_STATUS;
        reu_address += psize;
        size -= psize;
    }
    return i;
}

// Read from REU
static uint8_t
reu_read(uint32_t reu_address, void *data, size_t size)
{
    uint8_t i = REU_STATUS;
    REU_C64_ADDRESS_0 = (uint16_t)data & 0xFF;
    REU_C64_ADDRESS_1 = (uint16_t)data >> 8;
    REU_XMEM_ADDRESS_0 = reu_address & 0xFF;
    REU_XMEM_ADDRESS_1 = (reu_address >> 8) & 0xFF;
    REU_XMEM_ADDRESS_2 = reu_address >> 16;
    REU_XFER_SIZE_0 = size & 0xFF;
    REU_XFER_SIZE_1 = size >> 8;
    REU_IRQ_MASK = 0;
    REU_ADDRESS_CONTROL = 0;
    REU_COMMAND = 0x91;
    i = REU_STATUS;
    return i;
}

// Determine the size of the REU
static uint32_t
reu_size(void)
{
    char bytes55[8];
    char bytesAA[8];
    char rbytes[8];
    uint32_t addr;

    if (!reu_present()) {
        return 0;
    }

    // Test data at zero, at 128K and every power of two up to 16M
    addr = 0x800000;
    memset(bytes55, 0x55, sizeof(bytes55));
    memset(bytesAA, 0xAA, sizeof(bytesAA));
    while (addr > 0x8000) {
        bool good = true;
        // Write bytes55 to address zero
        reu_write(0, bytes55, sizeof(bytes55));
        // Write bytesAA to the test address
        reu_write(addr, bytesAA, sizeof(bytesAA));
        // Read back from the test address
        reu_read(addr, rbytes, sizeof(rbytes));
        if (memcmp(bytesAA, rbytes, sizeof(bytesAA)) != 0) {
            good = false;
        }
        // Read back from address zero
        reu_read(0, rbytes, sizeof(rbytes));
        if (memcmp(bytes55, rbytes, sizeof(bytes55)) != 0) {
            good = false;
        }
        if (good) {
            return addr << 1;
        }
        addr >>= 1;
    }

    return 0;
}

//////////////////////////////////////////////////////////////////////////////
//                                 UltiDOS                                  //
//////////////////////////////////////////////////////////////////////////////

// UltiDOS commands
enum {
    DOS_CMD_IDENTIFY       = 0x01,
    DOS_CMD_OPEN_FILE      = 0x02,
    DOS_CMD_CLOSE_FILE     = 0x03,
    DOS_CMD_READ_DATA      = 0x04,
    DOS_CMD_WRITE_DATA     = 0x05,
    DOS_CMD_FILE_SEEK      = 0x06,
    DOS_CMD_FILE_INFO      = 0x07,
    DOS_CMD_FILE_STAT      = 0x08,
    DOS_CMD_DELETE_FILE    = 0x09,
    DOS_CMD_RENAME_FILE    = 0x0A,
    DOS_CMD_COPY_FILE      = 0x0B,
    DOS_CMD_CHANGE_DIR     = 0x11,
    DOS_CMD_GET_PATH       = 0x12,
    DOS_CMD_OPEN_DIR       = 0x13,
    DOS_CMD_READ_DIR       = 0x14,
    DOS_CMD_CREATE_DIR     = 0x16,
    DOS_CMD_COPY_HOME_PATH = 0x17,
    DOS_CMD_LOAD_REU       = 0x21,
    DOS_CMD_SAVE_REU       = 0x22,
    DOS_CMD_MOUNT_DISK     = 0x23,
    DOS_CMD_UMOUNT_DISK    = 0x24,
    DOS_CMD_SWAP_DISK      = 0x25,
    DOS_CMD_GET_TIME       = 0x26,
    DOS_CMD_SET_TIME       = 0x27,
    DOS_CMD_ECHO           = 0xF0
};

// Flags for DOS_CMD_OPEN_FILE
enum {
    FA_READ = 0x01,
    FA_WRITE = 0x02,
    FA_CREATE_NEW = 0x04,
    FA_CREATE_ALWAYS = 0x08
};

static bool
dos_open(uint8_t target, const char *filename, uint8_t mode)
{
    static CMD_struct cmd;
    bool ok;
    size_t len = strlen(filename);

    cmd.target = target;
    cmd.command = DOS_CMD_OPEN_FILE;
    cmd.data[0] = mode;
    memcpy(cmd.data + 1, filename, len);
    cmd.data_size = len + 1;
    ok = cmd_xfer(&cmd);
    return ok;
}

static bool
dos_close(uint8_t target)
{
    static CMD_struct cmd;
    bool ok;

    cmd.target = target;
    cmd.command = DOS_CMD_CLOSE_FILE;
    cmd.data_size = 0;
    ok = cmd_xfer(&cmd);
    return ok && memcmp(cmd.status, "00", 2) == 0;
}

static bool
dos_seek(uint8_t target, uint32_t pos)
{
    static CMD_struct cmd;
    bool ok;

    cmd.target = target;
    cmd.command = DOS_CMD_FILE_SEEK;
    cmd.data_size = 4;
    cmd.data[0] = pos & 0xFF;
    cmd.data[1] = (pos >> 8) & 0xFF;
    cmd.data[2] = (pos >> 16) & 0xFF;
    cmd.data[3] = pos >> 24;
    ok = cmd_xfer(&cmd);
    return ok && memcmp(cmd.status, "00", 2) == 0;
}

static size_t
dos_read(uint8_t target, void *data, size_t len)
{
    static CMD_struct cmd;
    bool ok;

    cmd.target = target;
    cmd.command = DOS_CMD_READ_DATA;
    cmd.data_size = 2;
    cmd.data[0] = len & 0xFF;
    cmd.data[1] = len >> 8;
    ok = cmd_xfer(&cmd);
    if (!ok || cmd.status_size != 0) {
        fputs("dos_read returns status:\n", stdout);
        cmd.status[cmd.status_size] = 0;
        fputs(cmd.status, stdout);
        fputs("\n", stdout);
        return 0;
    }
    memcpy(data, cmd.data, cmd.data_size);
    return cmd.data_size;
}

static bool
dos_read_reu(uint8_t target, uint32_t data, uint32_t len)
{
    static CMD_struct cmd;
    bool ok;

    cmd.target = target;
    cmd.command = DOS_CMD_LOAD_REU;
    cmd.data_size = 8;
    cmd.data[0] = data & 0xFF;
    cmd.data[1] = (data >> 8) & 0xFF;
    cmd.data[2] = (data >> 16) & 0xFF;
    cmd.data[3] = data >> 24;
    cmd.data[4] = len & 0xFF;
    cmd.data[5] = (len >> 8) & 0xFF;
    cmd.data[6] = (len >> 16) & 0xFF;
    cmd.data[7] = len >> 24;
    ok = cmd_xfer(&cmd);
    return ok && memcmp(cmd.status, "00", 2) == 0;
}

//////////////////////////////////////////////////////////////////////////////
//                          ELF executable format                           //
//////////////////////////////////////////////////////////////////////////////

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


/* Load the RISC-V binary */
static bool
ELF_load(const char *filename)
{
    static const uint8_t magic[] = { 0x7F, 0x45, 0x4C, 0x46 };
    static const uint8_t target = 1;
    static ELF_file_header fhdr;
    uint32_t end_addr;
    uint32_t riscv_entry;
    uint32_t riscv_break;
    uint32_t max_readonly;
    uint32_t min_writable;
    unsigned i;
    uint32_t riscv_fence;
    uint32_t fence2;
    uint32_t riscv_stack;
    uint32_t stack_size;
    size_t len;
    uint8_t buf[8];
    uint32_t argv0;
    uint32_t argv;

    if (!dos_open(target, PROGRAM, FA_READ)) {
        fputs("Could not open program\n", stdout);
        return false;
    }

    /* Read the file header */
    if (dos_read(target, &fhdr, sizeof(fhdr)) != sizeof(fhdr)) {
        fputs("Could not read ELF header\n", stdout);
        goto error;
    }
    /* Check for correct format */
    if (memcmp(fhdr.e_ident, magic, sizeof(magic)) != 0) {
        fputs("Not in ELF format\n", stdout);
        goto error;
    }
    if (fhdr.e_ident[EI_CLASS] != 1         /* 32 bit */
        || fhdr.e_ident[EI_DATA] != 1       /* Little endian */
        || fhdr.e_type != 2                 /* Executable */
        || fhdr.e_machine != 0xF3           /* RISC-V */
        || fhdr.e_version != 1) {           /* ELF version */
        fputs("Unsupported system\n", stdout);
        goto error;
    }

    /* Get the program entry point and check that it is valid */
    riscv_entry = fhdr.e_entry;
    if (riscv_entry < 0x01000000 || 0x01FFFFFF < riscv_entry) {
        fputs("Program entry point out of range\n", stdout);
        goto error;
    }

    /* Get the locations of the program areas */
    riscv_break = 0;
    max_readonly = 0;
    min_writable = 0xFFFFFFFF;
    for (i = 0; i < fhdr.e_phnum; ++i) {
        static ELF_program_header phdr;
        dos_seek(target, fhdr.e_phoff + fhdr.e_phentsize*i);
        if (dos_read(target, &phdr, sizeof(phdr)) != sizeof(phdr)) {
            fputs("Could not read program header\n", stdout);
            goto error;
        }
        switch (phdr.p_type) {
        case 0: /* PT_NULL */
        case 0x70000003: /* PT_RISCV_ATTRIBUTES (ignored) */
            continue;

        case 1: /* PT_LOAD */
            if (phdr.p_memsz == 0) {
                continue;
            }

            /* Check that the segment loads to valid addresses */
            end_addr = phdr.p_vaddr + phdr.p_memsz;
            if (end_addr < phdr.p_vaddr || phdr.p_vaddr < 0x01000000 || 0x02000000 < end_addr) {
                fputs("Program header loads to invalid addresses\n", stdout);
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
                fputs("Program header allows both write and execute\n", stdout);
                goto error;
            }
            if (phdr.p_memsz < phdr.p_filesz) {
                fputs("Program header has memory size less than file size\n", stdout);
                goto error;
            }

            /* This will be the end of loaded content */
            if (riscv_break < end_addr) {
                riscv_break = end_addr;
            }

            /* Load program data into the REU */
            dos_seek(target, phdr.p_offset);
            if (!dos_read_reu(target, phdr.p_vaddr - 0x01000000, phdr.p_filesz)) {
                fputs("Cannot read program data\n", stdout);
                goto error;
            }

            /* Zero-fill to the specified memory size */
            reu_fill(phdr.p_vaddr - 0x01000000 + phdr.p_filesz, 0, phdr.p_memsz - phdr.p_filesz);
            break;

        default:
            fputs("Program header has unsupported type\n", stdout);
            goto error;
        }
    }

    if (max_readonly == 0) {
        fputs("No readable program headers\n", stdout);
        goto error;
    }

    /* Set the fence address */
    riscv_fence = (max_readonly + 0xFFFF) & 0xFFFF0000;
    fence2 = min_writable & 0xFFFF0000;
    if (fence2 < riscv_fence) {
        fputs("Cannot place fence\n", stdout);
        goto error;
    }

    dos_close(target);

    /* Write the initial stack (parameters to main) */
    /* First, determine the size: */
    /* Length of file name, plus one */
    stack_size = strlen(filename)+1;
    /* 16 bytes: argc (4), argv (4), argv[0] (4), argv[1] (4) */
    stack_size += 16;
    /* Round up to multiple of 16 */
    stack_size = (stack_size + 15) & 0xFFFFFFF0;

    /* Initial stack pointer */
    riscv_stack = 0x02000000 - stack_size;
    *(uint8_t *)(RISCV_STACK+0) = (riscv_stack >>  0) & 0xFF;
    *(uint8_t *)(RISCV_STACK+1) = (riscv_stack >>  8) & 0xFF;
    *(uint8_t *)(RISCV_STACK+2) = (riscv_stack >> 16) & 0xFF;

    /* argc */
    buf[0] = 1;
    buf[1] = 0;
    buf[2] = 0;
    buf[3] = 0;
    reu_write(riscv_stack - 0x01000000, buf, 4);
    riscv_stack += 4;

    /* argv address */
    argv = riscv_stack + 4;
    buf[0] = (argv >>  0) & 0xFF;
    buf[1] = (argv >>  8) & 0xFF;
    buf[2] = (argv >> 16) & 0xFF;
    buf[3] = (argv >> 24) & 0xFF;
    reu_write(riscv_stack - 0x01000000, buf, 4);
    riscv_stack += 4;

    /* argv contents */
    argv0 = riscv_stack + 8;
    buf[0] = (argv0 >>  0) & 0xFF;
    buf[1] = (argv0 >>  8) & 0xFF;
    buf[2] = (argv0 >> 16) & 0xFF;
    buf[3] = (argv0 >> 24) & 0xFF;
    buf[4] = 0;
    buf[5] = 0;
    buf[6] = 0;
    buf[7] = 0;
    reu_write(riscv_stack - 0x01000000, buf, 8);
    riscv_stack += 8;

    /* File name */
    len = strlen(filename) + 1;
    reu_write(riscv_stack - 0x01000000, filename, len);

    /* Set up the parameters for the 6502 code: */
    /* Stack pointer is set above */
    /* Entry point */
    *(uint8_t *)(RISCV_ENTRY+0) = (riscv_entry >>  0) & 0xFF;
    *(uint8_t *)(RISCV_ENTRY+1) = (riscv_entry >>  8) & 0xFF;
    *(uint8_t *)(RISCV_ENTRY+2) = (riscv_entry >> 16) & 0xFF;
    /* Fence */
    *(uint8_t *)(RISCV_FENCE  ) = (riscv_fence >> 16) & 0xFF;
    /* Break */
    *(uint8_t *)(RISCV_BREAK+0) = (riscv_break >>  0) & 0xFF;
    *(uint8_t *)(RISCV_BREAK+1) = (riscv_break >>  8) & 0xFF;
    *(uint8_t *)(RISCV_BREAK+2) = (riscv_break >> 16) & 0xFF;

    return true;

error:
    dos_close(target);
    return false;
}

//////////////////////////////////////////////////////////////////////////////

int
main(void)
{
    // Check that the platform is supportable

    if (!cmd_present()) {
        goto bad_platform;
    }

    if (reu_size() < 0x1000000) {
        goto bad_platform;
    }

    // Close any file that a prior (possibly failing) program left open
    cmd_abort();
    dos_close(1);
    dos_close(2);

    if (!ELF_load(PROGRAM)) {
        goto bad_program;
    }

    if (!dos_open(1, FILESYSTEM, FA_READ|FA_WRITE)) {
        goto bad_filesystem;
    }

    // Chain to the RISC-V emulator
    memcpy((void *)CHAIN_ADDR, chain_prg, chain_prg_size);
    ((void (*)(void))CHAIN_ADDR)();

    return EXIT_SUCCESS;

bad_platform:
    dos_close(1);
    dos_close(2);
    fputc(147, stdout);
         //1234567890123456789012345678901234567890
    fputs("Hardware requirements for this program:\n", stdout);
    fputs("* You must be running a 1541-Ultimate\n", stdout);
    fputs("  cartridge or Ultimate 64 board.\n", stdout);
    fputs("* The command interface must be enabled.", stdout);
    fputs("* The REU must be enabled and set to\n", stdout);
    fputs("  16 megabytes.\n", stdout);
    fputs("All of these settings can be found in\n", stdout);
    fputs("the device menu, under\n", stdout);
    fputs("       C64 and Cartridge Settings\n\n", stdout);
    fputs("If you have an Ultimate 64 board, you\n", stdout);
    fputs("should also set:\n", stdout);
    fputs("* CPU Speed to 48 MHz\n", stdout);
    fputs("* Badline Timing to Disabled\n", stdout);
    fputs("These settings can be found in the\n", stdout);
    fputs("device menu, under\n", stdout);
    fputs("         U64 Specific Settings", stdout);
    return EXIT_FAILURE;

bad_program:
    dos_close(1);
    dos_close(2);
    fputs("Could not load program:\n", stdout);
    fputs(PROGRAM, stdout);
    return EXIT_FAILURE;

bad_filesystem:
    dos_close(1);
    dos_close(2);
    fputs("Could not load file system:\n", stdout);
    fputs(FILESYSTEM, stdout);
    return EXIT_FAILURE;
}
