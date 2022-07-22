// ultidos.h -- interface to UltiDOS on 1541-Ultimate and Ultimate 64

#ifndef ULTIDOS_H
#define ULTIDOS_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

// Flags for dos_open mode
enum {
    FA_READ = 0x01,
    FA_WRITE = 0x02,
    FA_CREATE_NEW = 0x04,
    FA_CREATE_ALWAYS = 0x08
};

// Attributes returned when reading directory
// These are the same as FAT file systems
enum {
    FA_READONLY = 0x01,
    FA_HIDDEN = 0x02,
    FA_SYSTEM = 0x04,
    FA_VOLUME = 0x08,
    FA_DIRECTORY = 0x10,
    FA_ARCHIVE = 0x20
};

// Open a file to the given target (1 or 2)
// mode is FA_READ, FA_WRITE or FA_READ|FA_WRITE; FA_WRITE may be combined
// with FA_CREATE_NEW or FA_CREATE_ALWAYS
// Return true if success
extern bool dos_open(uint8_t target, const char *filename, uint8_t mode);

// Close the file on the given target (1 or 2)
// Return true if success
extern bool dos_close(uint8_t target);

// Seek the file on the given target (1 or 2) to the given position
// Return true if success
extern bool dos_seek(uint8_t target, uint32_t pos);

// Read from the file on the given target (1 or 2) to the given address
// Return the size read
extern size_t dos_read(uint8_t target, void *data, size_t len);

// Read from the file on the given target (1 or 2) to the REU at the given
// address
// Return true if successful
extern bool dos_read_reu(uint8_t target, uint32_t address, uint32_t len);

// Change current directory
extern bool dos_change_dir(uint8_t target, const char *path);

// Open the current directory
extern bool dos_open_dir(uint8_t target);

// Read first directory entry
extern bool dos_read_dir_first(uint8_t target, char *path, size_t len, uint8_t *attrs);

// Read next directory entry
extern bool dos_read_dir_next(uint8_t target, char *path, size_t len, uint8_t *attrs);

#endif
