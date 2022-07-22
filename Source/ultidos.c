// ultidos.c -- interface to UltiDOS on 1541-Ultimate and Ultimate 64

#include <stdio.h>
#include <string.h>
#include "cmd.h"
#include "ultidos.h"

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

// Open a file to the given target (1 or 2)
// mode is FA_READ, FA_WRITE or FA_READ|FA_WRITE; FA_WRITE may be combined
// with FA_CREATE_NEW or FA_CREATE_ALWAYS
// Return true if success
bool
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

// Close the file on the given target (1 or 2)
// Return true if success
bool
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

// Seek the file on the given target (1 or 2) to the given position
// Return true if success
bool
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

// Read from the file on the given target (1 or 2) to the given address
// Return the size read
size_t
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

// Read from the file on the given target (1 or 2) to the REU at the given
// address
// Return true if successful
bool
dos_read_reu(uint8_t target, uint32_t address, uint32_t len)
{
    static CMD_struct cmd;
    bool ok;

    cmd.target = target;
    cmd.command = DOS_CMD_LOAD_REU;
    cmd.data_size = 8;
    cmd.data[0] = address & 0xFF;
    cmd.data[1] = (address >> 8) & 0xFF;
    cmd.data[2] = (address >> 16) & 0xFF;
    cmd.data[3] = address >> 24;
    cmd.data[4] = len & 0xFF;
    cmd.data[5] = (len >> 8) & 0xFF;
    cmd.data[6] = (len >> 16) & 0xFF;
    cmd.data[7] = len >> 24;
    ok = cmd_xfer(&cmd);
    return ok && memcmp(cmd.status, "00", 2) == 0;
}

// Change the current directory
bool
dos_change_dir(uint8_t target, const char *path)
{
    static CMD_struct cmd;
    bool ok;

    cmd.target = target;
    cmd.command = DOS_CMD_CHANGE_DIR;
    strcpy(cmd.data, path);
    cmd.data_size = strlen(path);
    ok = cmd_xfer(&cmd);
    return ok && cmd.status[0] == '0';
}

// Open a directory
bool
dos_open_dir(uint8_t target)
{
    static CMD_struct cmd;
    bool ok;

    cmd.target = target;
    cmd.command = DOS_CMD_OPEN_DIR;
    cmd.data_size = 0;
    ok = cmd_xfer(&cmd);
    return ok && cmd.status[0] == '0';
}

// Read first directory entry
bool
dos_read_dir_first(uint8_t target, char *path, size_t len, uint8_t *attrs)
{
    static CMD_struct cmd;
    bool ok;

    cmd.target = target;
    cmd.command = DOS_CMD_READ_DIR;
    cmd.data_size = 0;

    ok = cmd_xfer(&cmd) && cmd.data_size != 0;
    if (ok) {
        *attrs = cmd.data[0];
        if (len > 0) {
            if (cmd.data_size > len) {
                cmd.data_size = len;
            }
            memcpy(path, cmd.data+1, cmd.data_size-1);
            path[cmd.data_size-1] = '\0';
        }
    }
    return ok;
}

// Read next directory entry
// Return false if end of directory
bool
dos_read_dir_next(uint8_t target, char *path, size_t len, uint8_t *attrs)
{
    static CMD_struct cmd;
    bool ok;

    ok = cmd_xfer_next(&cmd) && cmd.data_size != 0;
    if (ok) {
        *attrs = cmd.data[0];
        if (len > 0) {
            if (cmd.data_size > len) {
                cmd.data_size = len;
            }
            memcpy(path, cmd.data+1, cmd.data_size-1);
            path[cmd.data_size-1] = '\0';
        }
    }
    return ok;
}
