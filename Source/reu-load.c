// reu-load.c

#include <ctype.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "chain-prg.h"

#define PROGRAM "STARTUP"
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
//                      Conversion of ASCII to PETSCII                      //
//////////////////////////////////////////////////////////////////////////////

static const uint8_t ascii_to_petscii[] = {
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
    0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,
    0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
    0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F,
    0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
    0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F,
    0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
    0x38, 0x39, 0x3A, 0x3B, 0x3C, 0x3D, 0x3E, 0x3F,
    0x40, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7,
    0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF,
    0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7,
    0xD8, 0xD9, 0xDA, 0x5B, 0xBF, 0x5D, 0x5E, 0xA4,
    0xAD, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
    0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F,
    0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57,
    0x58, 0x59, 0x5A, 0xB3, 0xDD, 0xAB, 0xB1, 0xDF,
    0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
    0x88, 0x89, 0x8A, 0x8B, 0x8C, 0x8D, 0x8E, 0x8F,
    0x90, 0x91, 0x92, 0x0C, 0x94, 0x95, 0x96, 0x97,
    0x98, 0x99, 0x9A, 0x9B, 0x9C, 0x9D, 0x9E, 0x9F,
    0xA0, 0xA1, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7,
    0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF,
    0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7,
    0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF,
    0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
    0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F,
    0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
    0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F,
    0xE0, 0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7,
    0xE8, 0xE9, 0xEA, 0xEB, 0xEC, 0xED, 0xEE, 0xEF,
    0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7,
    0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF,
};

//////////////////////////////////////////////////////////////////////////////
// Partial implementation of FAT file system
//
// We're only interested in finding a file in the root directory, opening it,
// seeking it and reading it.
//////////////////////////////////////////////////////////////////////////////

// The boot sector
struct FS_boot_sector {
    // Common to FAT12, FAT16 and FAT32
    uint8_t BS_jmpBoot[3];
    char    BS_OEMName[8];
    uint8_t BPB_BytsPerSec[2];
    uint8_t BPB_SecPerClus;
    uint8_t BPB_RsvdSecCnt[2];
    uint8_t BPB_NumFats;
    uint8_t BPB_RootEntCnt[2];
    uint8_t BPB_TotSec16[2];
    uint8_t BPB_Media;
    uint8_t BPB_FATSz16[2];
    uint8_t BPB_SecPerTrk[2];
    uint8_t BPB_NumHeads[2];
    uint8_t BPB_HiddSec[4];
    uint8_t BPB_TotSec32[4];
    union {
        // For FAT12 and FAT16
        struct {
            uint8_t BS_DrvNum;
            uint8_t BS_Reserved1;
            uint8_t BS_BootSig;
            uint8_t BS_VolID[4];
            uint8_t BS_VolLab[11];
            uint8_t BS_FilSysType[8];
        } b16;
        // For FAT32
        struct {
            uint8_t BPB_FATSz32[4];
            uint8_t BPB_ExtFlags[2];
            uint8_t BPB_FSVer[2];
            uint8_t BPB_RootClus[4];
            uint8_t BPB_FSInfo[2];
            uint8_t BPB_BkBootSec[2];
            uint8_t BPB_Reserved[12];
            uint8_t BS_DrvNum;
            uint8_t BS_Reserved1;
            uint8_t BS_BootSig;
            uint8_t BS_VolID[4];
            uint8_t BS_VolLab[11];
            uint8_t BS_FilSysType[8];
        } b32;
    };
};

// Attribute bits for FS_dir_record
enum {
    ATTR_READ_ONLY = 0x01,
    ATTR_HIDDEN    = 0x02,
    ATTR_SYSTEM    = 0x04,
    ATTR_VOLUME    = 0x08,
    ATTR_DIRECTORY = 0x10,
    ATTR_ARCHIVE   = 0x20,
    ATTR_LONG_NAME = 0x0F,
    ATTR_LONG_NAME_MASK = 0x3F
};

// One 32-byte entry in the directory as recorded on disk
struct FS_dir_record {
    union {
        // Short name record
        struct {
            char DIR_Name[11];
            uint8_t DIR_Attr;
            uint8_t DIR_NTRes;
            uint8_t DIR_CrtTimeTenth;
            uint8_t DIR_CrtTime[2];
            uint8_t DIR_CrtDate[2];
            uint8_t DIR_AccDate[2];
            uint8_t DIR_FstClusHI[2];
            uint8_t DIR_WrtTime[2];
            uint8_t DIR_WrtDate[2];
            uint8_t DIR_FstClusLO[2];
            uint8_t DIR_FileSize[4];
        } s;
        // Long name record
        struct {
            uint8_t LDIR_Ord;
            uint8_t LDIR_Name1[5][2];
            uint8_t LDIR_Attr;
            uint8_t LDIR_Type;
            uint8_t LDIR_Chksum;
            uint8_t LDIR_Name2[6][2];
            uint8_t LDIR_FstClusLO[2];
            uint8_t LDIR_Name3[2][2];
        } l;
    };
};

struct FAT_volume {
    uint8_t fs_type; // 12, 16 or 32
    uint16_t sector_size;
    uint8_t sector_shift;
    uint16_t cluster_size;
    uint8_t cluster_shift;
    uint32_t first_fat;
    uint8_t num_fats;
    uint16_t root_dir_size;
    uint32_t num_sectors;
    uint32_t num_clusters;
    uint32_t max_cluster;
    uint32_t fat_size;
    uint32_t active_fat;
    uint32_t root_dir;
    uint32_t cluster_base;
};

static struct FAT_volume volume;

const uint8_t FAT_target = 1;

// Open the volume and retrieve its layout
static bool
FAT_open_volume(const char *filename)
{
    struct FS_boot_sector bsec;
    uint32_t root_dir_bytes;
    uint16_t root_dir_sectors;

    if (!dos_open(FAT_target, filename, FA_READ)) {
        return false;
    }

    // Read the boot sector
    if (!dos_read(FAT_target, &bsec, sizeof(bsec))) {
        goto error;
    }

    // Check for validity:
    // sector size 512, 1024, 2048 or 4096
    volume.sector_size =  bsec.BPB_BytsPerSec[0]
                       | (bsec.BPB_BytsPerSec[1] << 8);
    for (volume.sector_shift = 9;
         volume.sector_shift < 13 && (1 << volume.sector_shift) < volume.sector_size;
         ++volume.sector_shift) { }
    if ((1 << volume.sector_shift) != volume.sector_size) {
        goto error;
    }
    // cluster size 512, 1024, 2048, 4096, 8192, 16384 or 32768
    volume.cluster_size = volume.sector_size * bsec.BPB_SecPerClus;
    for (volume.cluster_shift = 9;
         volume.cluster_shift < 16 && (1 << volume.cluster_shift) < volume.cluster_size;
         ++volume.cluster_shift) { }
    if ((1 << volume.cluster_shift) != volume.cluster_size) {
        goto error;
    }

    // Set the locations of the data structures:
    // First FAT
    volume.first_fat =  bsec.BPB_RsvdSecCnt[0]
                     | (bsec.BPB_RsvdSecCnt[1] << 8);

    // Number of FATs
    volume.num_fats = bsec.BPB_NumFats;

    // Number of root directory entries
    volume.root_dir_size =  bsec.BPB_RootEntCnt[0]
                         | (bsec.BPB_RootEntCnt[1] << 8);

    // Number of sectors in the root directory
    root_dir_bytes = ((uint32_t)volume.root_dir_size << 5) + volume.sector_size - 1;
    root_dir_sectors = (uint16_t)(root_dir_bytes >> volume.sector_shift);

    // Number of sectors per FAT
    volume.fat_size =  bsec.BPB_FATSz16[0]
                    | (bsec.BPB_FATSz16[1] <<  8);
    if (volume.fat_size == 0) {
        volume.fat_size =  (uint32_t)bsec.b32.BPB_FATSz32[0]
                        | ((uint32_t)bsec.b32.BPB_FATSz32[1] <<  8)
                        | ((uint32_t)bsec.b32.BPB_FATSz32[2] << 16)
                        | ((uint32_t)bsec.b32.BPB_FATSz32[3] << 24);
    };

    // Number of sectors
    volume.num_sectors =  bsec.BPB_TotSec16[0]
                       | (bsec.BPB_TotSec16[1] << 8);
    if (volume.num_sectors == 0) {
        volume.num_sectors =  (uint32_t)bsec.BPB_TotSec32[0]
                           | ((uint32_t)bsec.BPB_TotSec32[1] <<  8)
                           | ((uint32_t)bsec.BPB_TotSec32[2] << 16)
                           | ((uint32_t)bsec.BPB_TotSec32[3] << 24);
    }

    // Root directory location
    // For FAT32, this will be replaced by a cluster number
    volume.root_dir = volume.first_fat + volume.fat_size * volume.num_fats;

    // First data sector
    volume.cluster_base = volume.root_dir + root_dir_sectors;

    // Number of clusters
    volume.num_clusters = (volume.num_sectors - volume.cluster_base)
                        / bsec.BPB_SecPerClus;

    // Clusters are numbered from 2 to max_cluster inclusive
    volume.max_cluster = volume.num_clusters + 1;
    volume.cluster_base -= 2 * bsec.BPB_SecPerClus;

    // File system type
    if (volume.num_clusters == 0) {
        goto error;
    } else if (volume.num_clusters < 0xFF5) {
        volume.fs_type = 12;
    } else if (volume.num_clusters < 0xFFF5) {
        volume.fs_type = 16;
    } else if (volume.num_clusters < 0xFFFFFF5) {
        volume.fs_type = 32;
    } else {
        goto error;
    }

    if (volume.fs_type == 32) {
        // FAT32 boot sector is laid out differently from FAT12 and FAT16
        if (bsec.b32.BS_BootSig != 0x29) {
            goto error;
        }
        volume.active_fat = volume.first_fat
                          + (bsec.b32.BPB_ExtFlags[0] & 0x0F) * volume.fat_size;
        volume.root_dir =  (uint32_t)bsec.b32.BPB_RootClus[0]
                        | ((uint32_t)bsec.b32.BPB_RootClus[1] <<  8)
                        | ((uint32_t)bsec.b32.BPB_RootClus[2] << 16)
                        | ((uint32_t)bsec.b32.BPB_RootClus[3] << 24);
    } else {
        // FAT12 or FAT16
        if (bsec.b16.BS_BootSig != 0x29) {
            goto error;
        }
        volume.active_fat = volume.first_fat;
        volume.root_dir <<= volume.sector_shift;
    }

    // Convert to byte positions
    volume.first_fat <<= volume.sector_shift;
    volume.active_fat <<= volume.sector_shift;
    volume.cluster_base <<= volume.sector_shift;

    return true;

error:
    dos_close(FAT_target);
    return false;
}

// Return the end of chain marker
static uint32_t
FAT_end_of_chain(void)
{
    switch (volume.fs_type) {
    case 12:
        return 0xFF8;

    case 16:
        return 0xFFF8;

    case 32:
        return 0xFFFFFF8;

    default:
        return 0xFFFFFFFF;
    }
}

// Given a cluster number, read the FAT entry for that cluster
static uint32_t
FAT_read_fat_entry(uint32_t cluster)
{
    uint8_t bytes[4];
    uint32_t new_cluster;

    // Check that the cluster number is in bounds
    if (cluster > volume.max_cluster) {
        return FAT_end_of_chain();
    }

    switch (volume.fs_type) {
    case 12:
        dos_seek(FAT_target, volume.active_fat + (cluster >> 1) + cluster);
        if (dos_read(FAT_target, bytes, 2) != 2) {
            goto error;
        }

        if (cluster & 1) {
            new_cluster = (bytes[0] >> 4) | (bytes[1] << 4);
        } else {
            new_cluster = bytes[0] | ((bytes[1] & 0x0F) << 8);
        }
        break;

    case 16:
        dos_seek(FAT_target, volume.active_fat + (cluster << 1));
        if (dos_read(FAT_target, bytes, 2) != 2) {
            goto error;
        }

        new_cluster = bytes[0] | (bytes[1] << 8);
        break;

    case 32:
        dos_seek(FAT_target, volume.active_fat + (cluster << 2));
        if (dos_read(FAT_target, bytes, 4) != 4) {
            goto error;
        }

        new_cluster =  (uint32_t)bytes[0]
                    | ((uint32_t)bytes[1] <<  8)
                    | ((uint32_t)bytes[2] << 16)
                    | ((uint32_t)bytes[3] << 24);
        break;

    default:
        goto error;
    }

    return new_cluster;

error:
    return 0xFFFFFFFF;
}

// Ad hoc structure used to scan a directory
struct FAT_directory {
    // Starting cluster of the directory; 0 if FAT12/FAT16 root
    uint32_t first_cluster;
    // Current cluster being scanned; 0 if FAT12/FAT16 root
    uint32_t current_cluster;
    // Offset into current cluster or FAT12/FAT16 root
    uint32_t offset;
};

// Initialize FAT_directory to point to the start of the root directory
static void
FAT_open_root(struct FAT_directory *dir)
{
    dir->offset = 0;
    if (volume.fs_type == 32) {
        dir->first_cluster = volume.root_dir;
        dir->current_cluster = volume.root_dir;
    } else {
        dir->first_cluster = 0;
        dir->current_cluster = 0;
    }
}

// Reset FAT_directory to the start of its directory
static void
FAT_rewind_dir(struct FAT_directory *dir)
{
    dir->current_cluster = dir->first_cluster;
    dir->offset = 0;
}

// Read the next directory entry
static bool
FAT_read_dir(struct FAT_directory *dir, struct FS_dir_record *rec)
{
    bool ok;

    if (dir->current_cluster == 0 && volume.fs_type != 32) {
        // Check for end of directory
        if ((dir->offset >> 5) >= volume.root_dir_size) {
            return false;
        }
        // Seek to the location of the record
        ok = dos_seek(FAT_target, volume.root_dir + dir->offset);
    } else {
        // Check for end of directory
        if (dir->current_cluster < 2 || volume.max_cluster < dir->current_cluster) {
            return false;
        }
        // Check for end of cluster
        if (dir->offset >= volume.cluster_size) {
            // Advance to next cluster
            dir->current_cluster = FAT_read_fat_entry(dir->current_cluster);
            // Check again for end of directory
            if (dir->current_cluster < 2 || volume.max_cluster < dir->current_cluster) {
                return false;
            }
            dir->offset = 0;
        }
        // Seek to the location of the record
        ok = dos_seek(FAT_target,
                      volume.cluster_base
                    + (dir->current_cluster << volume.cluster_shift)
                    + dir->offset);
    }
    if (!ok) {
        return false;
    }
    // Read one directory record
    ok = dos_read(FAT_target, rec, sizeof(*rec)) == sizeof(*rec);
    if (ok) {
        dir->offset += 32;
    }
    return ok;
}

// Properties of a directory record:

// Return true if the directory record is a long file name record
static bool
FAT_is_long_name_record(const struct FS_dir_record *rec)
{
    return ((rec->s.DIR_Attr & ATTR_LONG_NAME_MASK) == ATTR_LONG_NAME);
}

// Return true if the directory record marks the end of the directory
static bool
FAT_is_end(const struct FS_dir_record *rec)
{
    return rec->s.DIR_Name[0] == '\0';
}

// Return true if the directory record is erased
static bool
FAT_is_erased(const struct FS_dir_record *rec)
{
    return rec->s.DIR_Name[0] == '\xE5';
}

// Return true if the directory record is a volume label
static bool
FAT_is_volume_label(const struct FS_dir_record *rec)
{
    return (rec->s.DIR_Attr & ATTR_VOLUME) != 0 && !FAT_is_long_name_record(rec);
}

// Return true if the directory record is a short file name record
static bool
FAT_is_short_name_record(const struct FS_dir_record *rec)
{
    return (rec->s.DIR_Attr & ATTR_VOLUME) == 0
        && !FAT_is_end(rec) && !FAT_is_erased(rec);
}

// Return true if the directory record matches the short file name
static bool
FAT_short_name_matches(const struct FS_dir_record *rec, const char *name)
{
    uint8_t i, j;

    if (!FAT_is_short_name_record(rec)) {
        return false;
    }

    // Special case for first character
    if (name[0] == '\0') {
        return false;
    }
    if (!(toupper(ascii_to_petscii[(unsigned char)rec->s.DIR_Name[0]]) == toupper((unsigned char)name[0])
          || (rec->s.DIR_Name[0] == '\x05' && name[0] == '\xE5'))) {
        return false;
    }

    // Match the rest of the name
    i = 1;
    while (i < 8) {
        if (name[i] == '.' || name[i] == '\0') {
            break;
        }
        if (toupper(ascii_to_petscii[(unsigned char)rec->s.DIR_Name[i]]) != toupper((unsigned char)name[i])) {
            return false;
        }
        ++i;
    }
    if (name[i] != '.' && name[i] != '\0') {
        return false;
    }
    j = i;
    for (; i < 8; ++i) {
        if (rec->s.DIR_Name[i] != ' ') {
            return false;
        }
    }
    // Match the extension if the given name has one
    if (name[j] == '.') {
        ++j;
        while (i < 11) {
            if (name[j] == '\0') {
                break;
            }
            if (toupper(ascii_to_petscii[(unsigned char)rec->s.DIR_Name[i]]) != toupper((unsigned char)name[j])) {
                return false;
            }
            ++i;
            ++j;
        }
    }
    if (name[j] != '\0') {
        return false;
    }
    for (; i < 11; ++i) {
        if (rec->s.DIR_Name[i] != ' ') {
            return false;
        }
    }

    return true;
}

// Return true if the directory record matches the long file name.
// name_seg is offset by some multiple of 13 from the start of the name.
// The caller is responsible for selecting the correct offset.
static bool
FAT_long_name_matches(const struct FS_dir_record *rec, const char *name_seg)
{
    uint8_t i;

    if (!FAT_is_long_name_record(rec)) {
        return false;
    }

    // Match the first part of the name
    for (i = 0; i < 5; ++i) {
        if (rec->l.LDIR_Name1[i][1] != 0) {
            return false;
        }
        if (toupper(ascii_to_petscii[rec->l.LDIR_Name1[i][0]]) != toupper((unsigned char)name_seg[i+0])) {
            return false;
        }
        if (name_seg[i+0] == '\0') {
            return true;
        }
    }

    // Match the second part of the name
    for (i = 0; i < 6; ++i) {
        if (rec->l.LDIR_Name2[i][1] != 0) {
            return false;
        }
        if (toupper(ascii_to_petscii[rec->l.LDIR_Name2[i][0]]) != toupper((unsigned char)name_seg[i+5])) {
            return false;
        }
        if (name_seg[i+5] == '\0') {
            return true;
        }
    }

    // Match the third part of the name
    for (i = 0; i < 2; ++i) {
        if (rec->l.LDIR_Name3[i][1] != 0) {
            return false;
        }
        if (toupper(ascii_to_petscii[rec->l.LDIR_Name3[i][0]]) != toupper((unsigned char)name_seg[i+11])) {
            return false;
        }
        if (name_seg[i+11] == '\0') {
            return true;
        }
    }

    return true;
}

// Checksum for short names
static uint8_t FAT_checksum(const struct FS_dir_record *rec)
{
    uint8_t sum = 0;
    unsigned i;
    for (i = 0; i < 11; ++i) {
        sum = (uint8_t)(((sum << 7) | (sum >> 1)) + (uint8_t)rec->s.DIR_Name[i]);
    }
    return sum;
}

// Search the given directory for the given name
// Return the short file name record if found
static bool
FAT_search_dir(
        struct FAT_directory *dir,
        const char *name,
        struct FS_dir_record *rec)
{
    bool ok;
    uint8_t start_seq;
    uint8_t sequence;
    uint8_t checksum;
    const char *last_segment;
    const char *segment;

    // A matching sequence of long name records must begin with a sequence
    // number determined by the length of the name.
    // One record is needed for every thirteen characters in rec.
    start_seq = (strlen(name) + 12) / 13 + 0x40;
    last_segment = name + (start_seq - 0x41) * 13;

    FAT_rewind_dir(dir);

    // Read records until end of directory
    while (true) {
        ok = FAT_read_dir(dir, rec);
        if (!ok || FAT_is_end(rec)) {
            // End of directory, or error on read
            return false;
        }
        // The long file name match algorithm branches here on any failure,
        // so the non-matching record can start another match.
        restart:
        // Never match an erasure or a volume label
        if (FAT_is_volume_label(rec) || FAT_is_erased(rec)) {
            continue;
        }

        // A matching short name record constitutes a complete match
        if (FAT_short_name_matches(rec, name)) {
            return true;
        }

        // We can start a long name match if
        // * the record is a long name record and
        // * its sequence number is equal to start_seq and
        // * it matches the last segment of the name.
        if (FAT_is_long_name_record(rec)
        &&  rec->l.LDIR_Ord == start_seq
        &&  FAT_long_name_matches(rec, last_segment)) {
            // Record sequence number, name segment and checksum to match
            // subsequent records.
            sequence = start_seq - 0x40;
            segment = last_segment;
            checksum = rec->l.LDIR_Chksum;
            while (true) {
                // Read the next record
                ok = FAT_read_dir(dir, rec);
                if (!ok || FAT_is_end(rec)) {
                    // End of directory, or error on read
                    return false;
                }
                if (FAT_is_short_name_record(rec)) {
                    // A short name record matches if
                    // * the last sequence number is 1 and
                    // * the checksum equals the checksum from the long name
                    //   records.
                    if (sequence == 1 && checksum == FAT_checksum(rec)) {
                        return true;
                    }
                    // Other short name records end the match with a negative
                    // result.
                    goto restart;
                } else if (FAT_is_long_name_record(rec)) {
                    // A long name record continues the match if
                    // * its sequence number is one less than the last one and
                    // * its checksum matches that of the last record and
                    // * it matches the next segment of the name.
                    --sequence;
                    if (rec->l.LDIR_Ord != sequence
                    ||  rec->l.LDIR_Chksum != checksum) {
                        goto restart;
                    }
                    segment -= 13;
                    if (!FAT_long_name_matches(rec, segment)) {
                        goto restart;
                    }
                    // Continue the loop to match long name records
                } else {
                    // Other records fail to match
                    goto restart;
                }
            }
        }
    }
}

// Ad hoc record to read a file from the volume
struct FAT_file {
    uint32_t first_cluster;     // First cluster allocated to the file
    uint32_t size;              // Size of the file in bytes
    uint32_t position;          // Current position
    uint32_t current_cluster;   // Current cluster as cluster number
    uint32_t cluster_index;     // Current position in cluster chain starting with 0
    uint16_t offset;            // Current offset within the cluster
};

// Initialize FAT_file to read from the beginning of a file
static void
FAT_open_file(struct FAT_file *file, const struct FS_dir_record *rec)
{
    file->first_cluster =  (uint32_t)rec->s.DIR_FstClusLO[0]
                        | ((uint32_t)rec->s.DIR_FstClusLO[1] <<  8)
                        | ((uint32_t)rec->s.DIR_FstClusHI[0] << 16)
                        | ((uint32_t)rec->s.DIR_FstClusHI[1] << 24);
    file->size =  (uint32_t)rec->s.DIR_FileSize[0]
               | ((uint32_t)rec->s.DIR_FileSize[1] <<  8)
               | ((uint32_t)rec->s.DIR_FileSize[2] << 16)
               | ((uint32_t)rec->s.DIR_FileSize[3] << 24);
    file->position = 0;
    file->current_cluster = file->first_cluster;
    file->cluster_index = 0;
    file->offset = 0;
}

// Seek a file to the given position
static bool
FAT_seek_file(struct FAT_file *file, uint32_t position)
{
    uint32_t cindex;
    uint16_t coffset;

    // Check for seek beyond end of file
    if (position > file->size) {
        position = file->size;
    }

    // Split position into cluster index and offset
    cindex = position >> volume.cluster_shift;
    coffset = position & ((0x1 << volume.cluster_shift) - 1);

    // Special case for seek to or beyond the end of the file
    if (position == file->size) {
        file->current_cluster = FAT_end_of_chain();
        file->cluster_index = cindex;
        file->offset = coffset;
        file->position = position;
        return true;
    }

    // If the cluster index is less than the current one, seek from the
    // beginning; otherwise, seek from the current position
    if (cindex < file->cluster_index) {
        file->current_cluster = file->first_cluster;
        file->cluster_index = 0;
    }

    // Follow the cluster chain
    while (file->cluster_index < cindex) {
        file->current_cluster = FAT_read_fat_entry(file->current_cluster);
        ++file->cluster_index;
    }

    // Set the position and the offset
    file->position = position;
    file->offset = coffset;

    return true;
}

// Read from a file into local memory
static size_t
FAT_read_file(struct FAT_file *file, void *buf, size_t size)
{
    size_t read;
    bool ok;

    // Don't read beyond the end of the file
    if (file->position >= file->size) {
        return 0;
    }
    if (file->position + size > file->size
    ||  file->position + size < file->position) {
        size = file->size - file->position;
    }

    read = 0;
    while (read < size) {
        // Size to be read from the current cluster
        size_t clus_read = volume.cluster_size - file->offset;
        if (clus_read == 0) {
            // We're at the end of the cluster
            file->current_cluster = FAT_read_fat_entry(file->current_cluster);
            ++file->cluster_index;
            file->offset = 0;
            clus_read = volume.cluster_size;
        }
        // Check for errors in following the cluster chain
        if (file->current_cluster < 2 || volume.max_cluster < file->current_cluster) {
            break;
        }
        if (clus_read > size - read) {
            // File ends within the current cluster
            clus_read = size - read;
        }
        // Seek to the current file position
        ok = dos_seek(FAT_target,
                        volume.cluster_base
                      + ((uint32_t)file->current_cluster << volume.cluster_shift)
                      + file->offset);
        if (!ok) {
            break;
        }
        // Read to end of cluster or to end of file
        ok = dos_read(FAT_target, buf, clus_read) == clus_read;
        if (!ok) {
            break;
        }
        // Advance pointers and indexes
        buf = (char *)buf + clus_read;
        read += clus_read;
        file->position += clus_read;
        file->offset += clus_read;
    }

    return read;
}

// Read from a file into local memory
static uint32_t
FAT_read_file_reu(struct FAT_file *file, uint32_t data, uint32_t size)
{
    uint32_t read;
    bool ok;

    // Don't read beyond the end of the file
    if (file->position >= file->size) {
        return 0;
    }
    if (file->position + size > file->size
    ||  file->position + size < file->position) {
        size = file->size - file->position;
    }

    read = 0;
    while (read < size) {
        // Size to be read from the current cluster
        uint32_t clus_read = volume.cluster_size - file->offset;
        if (clus_read == 0) {
            // We're at the end of the cluster
            file->current_cluster = FAT_read_fat_entry(file->current_cluster);
            ++file->cluster_index;
            file->offset = 0;
            clus_read = volume.cluster_size;
        }
        // Check for errors in following the cluster chain
        if (file->current_cluster < 2 || volume.max_cluster < file->current_cluster) {
            break;
        }
        if (clus_read > size - read) {
            // File ends within the current cluster
            clus_read = size - read;
        }
        // Seek to the current file position
        ok = dos_seek(FAT_target,
                        volume.cluster_base
                      + ((uint32_t)file->current_cluster << volume.cluster_shift)
                      + file->offset);
        if (!ok) {
            break;
        }
        // Read to end of cluster or to end of file
        ok = dos_read_reu(FAT_target, data, clus_read);
        if (!ok) {
            break;
        }
        // Advance pointers and indexes
        data += clus_read;
        read += clus_read;
        file->position += clus_read;
        file->offset += clus_read;
    }

    return read;
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
    static ELF_file_header fhdr;
    struct FAT_directory dir;
    struct FS_dir_record rec;
    struct FAT_file file;
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

    FAT_open_root(&dir);
    if (!FAT_search_dir(&dir, filename, &rec)) {
        fputs("Could not open program\n", stdout);
        return false;
    }
    if (rec.s.DIR_Attr & ATTR_DIRECTORY) {
        fputs("Program is a directory\n", stdout);
        return false;
    }
    FAT_open_file(&file, &rec);

    /* Read the file header */
    if (FAT_read_file(&file, &fhdr, sizeof(fhdr)) != sizeof(fhdr)) {
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
        FAT_seek_file(&file, fhdr.e_phoff + fhdr.e_phentsize*i);
        if (FAT_read_file(&file, &phdr, sizeof(phdr)) != sizeof(phdr)) {
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
            FAT_seek_file(&file, phdr.p_offset);
            if (FAT_read_file_reu(&file, phdr.p_vaddr - 0x01000000, phdr.p_filesz) != phdr.p_filesz) {
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

    if (!FAT_open_volume(FILESYSTEM)) {
        goto bad_filesystem;
    }

    if (!ELF_load(PROGRAM)) {
        goto bad_program;
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
