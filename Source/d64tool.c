// Tool to create a "super D64" for Ultimate 64

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SIZE(x) (sizeof(x)/sizeof((x)[0]))

// Layout of tracks in the D64

typedef struct D64_track {
    unsigned first_sector;
    unsigned num_sectors;
} D64_track;

static const D64_track D64_tracks[] = {
    { .first_sector =   0, .num_sectors = 21 },
    { .first_sector =  21, .num_sectors = 21 },
    { .first_sector =  42, .num_sectors = 21 },
    { .first_sector =  63, .num_sectors = 21 },
    { .first_sector =  84, .num_sectors = 21 },
    { .first_sector = 105, .num_sectors = 21 },
    { .first_sector = 126, .num_sectors = 21 },
    { .first_sector = 147, .num_sectors = 21 },
    { .first_sector = 168, .num_sectors = 21 },
    { .first_sector = 189, .num_sectors = 21 },
    { .first_sector = 210, .num_sectors = 21 },
    { .first_sector = 231, .num_sectors = 21 },
    { .first_sector = 252, .num_sectors = 21 },
    { .first_sector = 273, .num_sectors = 21 },
    { .first_sector = 294, .num_sectors = 21 },
    { .first_sector = 315, .num_sectors = 21 },
    { .first_sector = 336, .num_sectors = 21 },
    { .first_sector = 357, .num_sectors = 19 },
    { .first_sector = 376, .num_sectors = 19 },
    { .first_sector = 395, .num_sectors = 19 },
    { .first_sector = 414, .num_sectors = 19 },
    { .first_sector = 433, .num_sectors = 19 },
    { .first_sector = 452, .num_sectors = 19 },
    { .first_sector = 471, .num_sectors = 19 },
    { .first_sector = 490, .num_sectors = 18 },
    { .first_sector = 508, .num_sectors = 18 },
    { .first_sector = 526, .num_sectors = 18 },
    { .first_sector = 544, .num_sectors = 18 },
    { .first_sector = 562, .num_sectors = 18 },
    { .first_sector = 580, .num_sectors = 18 },
    { .first_sector = 598, .num_sectors = 17 },
    { .first_sector = 615, .num_sectors = 17 },
    { .first_sector = 632, .num_sectors = 17 },
    { .first_sector = 649, .num_sectors = 17 },
    { .first_sector = 666, .num_sectors = 17 }
};

// D64 Block allocation map
typedef struct D64_bam {
    uint8_t next_track;
    uint8_t next_sector;
    uint8_t dos_version;
    uint8_t reserved1;
    uint8_t bam[35][4];
    uint8_t disk_name[16];
    uint8_t reserved2[2];
    uint8_t disk_id[2];
    uint8_t reserved3;
    uint8_t dos_type[2];
    uint8_t reserved4[4];
    uint8_t reserved5[85];
} D64_bam;

// Directory entry
typedef struct D64_dir_entry {
    uint8_t next_track;
    uint8_t next_sector;
    uint8_t file_type;
    uint8_t file_track;
    uint8_t file_sector;
    uint8_t file_name[16];
    uint8_t rel_track;
    uint8_t rel_sector;
    uint8_t rel_record_size;
    uint8_t reserved1[6];
    uint8_t file_size[2];
} D64_dir_entry;

// File type constants
enum {
    D64_DEL = 0,
    D64_SEQ = 1,
    D64_PRG = 2,
    D64_USR = 3,
    D64_REL = 4,
    D64_LOCKED = 0x40,
    D64_CLOSED = 0x80
};

// BAM and directory to be saved to the image
static D64_bam bam;
static D64_dir_entry directory[8*16];
static unsigned directory_size;

// Options passed to the program
static const char *output = NULL;
static const char *disk_name = NULL;
static const char *disk_id = NULL;
static const char *inputs[SIZE(directory)];
static unsigned num_inputs = 0;

// File structure for the image
static FILE *image_fp;

// Conversion of letters from ASCII to PETSCII
static uint8_t
ascii_to_petscii(uint8_t ch)
{
    if (0x41 <= ch && ch <= 0x5A) {
        ch += 0x80;
    } else if (0xC1 <= ch && ch <= 0xDA) {
        ch -= 0x60;
    } else if (0x61 <= ch && ch <= 0x7A) {
        ch -= 0x20;
    }
    return ch;
}

// Copy a string to a D64 structure, convert to PETSCII and pad with 0xA0
static void
petscii_pad(uint8_t *dest, size_t size, const char *src)
{
    size_t i;
    for (i = 0; i < size && src[i] != '\0'; ++i) {
        dest[i] = ascii_to_petscii(src[i]);
    }
    for (; i < size; ++i) {
        dest[i] = 0xA0;
    }
}

// Initial format of D64
static bool
D64_format(void)
{
    unsigned i;

    image_fp = fopen(output, "w+b");
    if (image_fp == NULL) {
        goto error;
    }

    // Write a blank image
    uint8_t sector[256];
    memset(sector, 0, sizeof(sector));
    unsigned num_sectors = D64_tracks[SIZE(D64_tracks)-1].first_sector
                         + D64_tracks[SIZE(D64_tracks)-1].num_sectors;
    for (i = 0; i < num_sectors; ++i) {
        fwrite(sector, 1, sizeof(sector), image_fp);
        if (ferror(image_fp)) {
            goto error;
        }
    }

    // Block Allocation Map
    bam.next_track = 18;
    bam.next_sector = 1;
    bam.dos_version = 0x41;
    bam.reserved1 = 0;
    petscii_pad(bam.disk_name, sizeof(bam.disk_name), disk_name);
    memset(bam.reserved2, 0xA0, sizeof(bam.reserved2));
    petscii_pad(bam.disk_id, sizeof(bam.disk_id), disk_id);
    bam.reserved3 = 0xA0;
    bam.dos_type[0] = '2';
    bam.dos_type[1] = 'A';
    memset(bam.reserved4, 0xA0, sizeof(bam.reserved4));
    memset(bam.reserved5, 0x00, sizeof(bam.reserved5));

    for (i = 0; i < SIZE(D64_tracks); ++i) {
        uint32_t block_bitmap = ((uint32_t)1 << D64_tracks[i].num_sectors) - 1;
        unsigned num_sectors = D64_tracks[i].num_sectors;
        bam.bam[i][0] = num_sectors;
        bam.bam[i][1] = (uint8_t)(block_bitmap >>  0);
        bam.bam[i][2] = (uint8_t)(block_bitmap >>  8);
        bam.bam[i][3] = (uint8_t)(block_bitmap >> 16);
    }

    // Directory
    memset(directory, 0, sizeof(directory));
    directory_size = 0;

    return true;

error:
    perror(output);
    if (image_fp != NULL) {
        fclose(image_fp);
        image_fp = NULL;
    }
    return false;
}

// Allocate the given track and sector
static bool
D64_allocate(unsigned track, unsigned sector)
{
    assert(track != 0 && track <= SIZE(D64_tracks));
    assert(sector < D64_tracks[track-1].num_sectors);

    uint8_t *bam_row = bam.bam[track-1];
    unsigned byte = sector >> 3;
    unsigned bit = 1 << (sector & 7);
    if (!(bam_row[byte+1] & bit)) {
        return false;
    } else {
        bam_row[byte+1] &= ~bit;
        --bam_row[0];
        return true;
    }
}

// Find the track closest to the directory that has free space
static unsigned
D64_find_track(void)
{
    unsigned prev_track;
    for (prev_track = 17; prev_track > 0; --prev_track) {
        if (bam.bam[prev_track-1][0] != 0) {
            break;
        }
    }

    unsigned next_track;
    for (next_track = 19; next_track < 35; ++next_track) {
        if (bam.bam[next_track-1][0] != 0) {
            break;
        }
    }

    if (next_track == 0) {
        return prev_track;
    }
    if (prev_track == 0) {
        return next_track;
    }

    if (next_track - 18 < 18 - prev_track) {
        return next_track;
    } else {
        return prev_track;
    }
}

// Write the given file to the image
static bool
D64_write_file(const char *file_name)
{
    FILE *input_fp = NULL; // custodial

    // Extract directory name and file type
    const char *basename = strrchr(file_name, '/');
    if (basename == NULL) {
        basename = file_name;
    } else {
        basename += strspn(basename, "/");
    }
    const char *dot = strrchr(basename, '.');
    char dir_entry[strlen(basename)+1];
    strcpy(dir_entry, basename);
    unsigned file_type = D64_SEQ | D64_CLOSED;
    if (dot != NULL) {
        dir_entry[dot - basename] = '\0';
        if (strcmp(dot, ".prg") == 0) {
            file_type = D64_PRG | D64_CLOSED;
        }
    }

    // Allocate a directory entry
    if (directory_size >= SIZE(directory)) {
        fprintf(stderr, "Directory is full\n");
        goto error;
    }
    D64_dir_entry *dir = &directory[directory_size];

    // Find a track with free space
    unsigned track = D64_find_track();
    if (track == 0) {
        fprintf(stderr, "Disk is full\n");
        goto error;
    }

    // Set sector to 0; we'll search later
    unsigned sector = 0;

    // Open the input file
    input_fp = fopen(file_name, "rb");
    if (input_fp == NULL) {
        perror(file_name);
        goto error;
    }

    // Copy sectors onto the image
    uint8_t sector1[256], sector2[256];
    uint8_t *this_sector = sector1;
    uint8_t *last_sector = NULL;
    unsigned num_sectors = 0;

    // Read the first sector from the input file
    memset(this_sector, 0, 256);
    size_t size = fread(this_sector+2, 1, 254, input_fp);
    if (ferror(input_fp)) {
        perror(file_name);
        goto error;
    }
    if (size == 0) {
        // The file is empty
        dir->file_track = 0;
        dir->file_sector = 0;
        goto write_dir;
    }
    ++num_sectors;

    // Allocate one sector
    for (sector = 0; sector < D64_tracks[track-1].num_sectors; ++sector) {
        if (D64_allocate(track, sector)) {
            break;
        }
    }
    dir->file_track = track;
    dir->file_sector = sector;
    if (size < 254) {
        this_sector[0] = 0;
        this_sector[1] = (uint8_t)size;
        goto write_last;
    }
    this_sector = sector2;
    last_sector = sector1;

    while (true) {
        // Read a sector from the input file
        memset(this_sector, 0, 256);
        size = fread(this_sector+2, 1, 254, input_fp);
        if (ferror(input_fp)) {
            perror(file_name);
            goto error;
        }
        if (size == 0) {
            // Last read ended on a sector boundary
            last_sector[0] = 0;
            last_sector[1] = 255;
            break;
        }
        ++num_sectors;

        // Seek to location to write last_sector
        fseek(image_fp, (D64_tracks[track-1].first_sector + sector) * 256, SEEK_SET);

        // Allocate another sector
        sector = (sector + 10) % D64_tracks[track-1].num_sectors;
        if (bam.bam[track-1][0] == 0) {
            // Need to find another track
            sector = 0;
            if (track < 18) {
                // If before the directory, go backward to start of disk
                while (track != 0 && bam.bam[track-1][0] == 0) {
                    --track;
                }
                if (track == 0) {
                    track = D64_find_track();
                }
            } else {
                // If after the directory, go forward to end of disk
                while (track < 35 && bam.bam[track-1][0] == 0) {
                    ++track;
                }
                if (track >= 35) {
                    track = D64_find_track();
                }
            }
            if (track == 0) {
                fprintf(stderr, "Disk is full\n");
                goto error;
            }
        }
        bool got_sector = false;
        for (unsigned tries = 0; tries < 2 && !got_sector; ++tries) {
            // Try from current sector
            for (; sector < D64_tracks[track-1].num_sectors; ++sector) {
                if (D64_allocate(track, sector)) {
                    got_sector = true;
                    break;
                }
            }
            // and again from sector 0
            if (!got_sector) {
                sector = 0;
            }
        }

        // Write last_sector
        last_sector[0] = track;
        last_sector[1] = sector;
        fwrite(last_sector, 1, 256, image_fp);
        if (ferror(image_fp)) {
            perror(output);
            goto error;
        }

        // Switch the pointers
        uint8_t *swap = this_sector;
        this_sector = last_sector;
        last_sector = swap;

        // End here if end of file
        if (size < 254) {
            last_sector[0] = 0;
            last_sector[1] = size+1;
            break;
        }
    }

    // Come here to write the last sector
write_last:
    fseek(image_fp, (D64_tracks[track-1].first_sector + sector) * 256, SEEK_SET);
    fwrite(last_sector, 1, 256, image_fp);
    if (ferror(image_fp)) {
        perror(output);
        goto error;
    }

    // Come here to write the directory
write_dir:
    dir->file_type = file_type;
    petscii_pad(dir->file_name, sizeof(dir->file_name), dir_entry);
    dir->rel_track = 0;
    dir->rel_sector = 0;
    dir->rel_record_size = 0;
    memset(dir->reserved1, 0, sizeof(dir->reserved1));
    dir->file_size[0] = (uint8_t)(num_sectors >> 0);
    dir->file_size[1] = (uint8_t)(num_sectors >> 8);

    ++directory_size;
    fclose(input_fp);
    return true;

error:
    if (input_fp != NULL) {
        fclose(input_fp);
    }
    return false;
}

// Close out the image
static bool
D64_close(void)
{
    // Interleave for the directory; entries i*8 through i*8+7 will be written
    // to sector interleave_3[i]
    static const uint8_t interleave_3[] = {
        1, 4, 7, 10, 13, 16,
        2, 5, 8, 11, 14, 17,
        3, 6, 9, 12, 15, 18
    };

    // Allocate sector for the BAM
    D64_allocate(18, 0);

    // Allocate sectors for the directory
    for (unsigned i = 0; i < SIZE(directory); i += 8) {
        D64_allocate(18, interleave_3[i/8]);
        if (i + 8 < directory_size) {
            directory[i].next_track = 18;
            directory[i].next_sector = interleave_3[i/8+1];
        } else {
            directory[i].next_track = 0;
            directory[i].next_sector = 0xFF;
            break;
        }
    }

    // Write the BAM
    fseek(image_fp, (long)D64_tracks[18-1].first_sector * 256, SEEK_SET);
    fwrite(&bam, sizeof(bam), 1, image_fp);
    if (ferror(image_fp)) {
        goto error;
    }

    // Write the directory
    for (unsigned i = 0; i < SIZE(directory); i += 8) {
        fseek(image_fp,
              (long)(D64_tracks[18-1].first_sector + interleave_3[i/8]) * 256,
              SEEK_SET);
        fwrite(directory+i, sizeof(directory[i]), 8, image_fp);
        if (ferror(image_fp)) {
            goto error;
        }
        if (directory[i].next_track == 0) {
            break;
        }
    }

    fclose(image_fp);
    image_fp = NULL;
    return true;

error:
    perror(output);
    fclose(image_fp);
    image_fp = NULL;
    return false;
}

static void
usage(const char *prog_name)
{
    fprintf(stderr, "Usage: %s --output=<image-name> [--name=<name>] [--id=<id>] <input-file>*\n",
            prog_name);
}

int
main(int argc, char **argv)
{
    // Scan argv for options and input file names
    for (int i = 1; i < argc; ++i) {
        if (strcmp(argv[i], "--help") == 0 || strcmp(argv[i], "-h") == 0) {
            usage(argv[0]);
            goto error;
        } else if (strncmp(argv[i], "--output=", 9) == 0) {
            if (output != NULL) {
                fprintf(stderr, "%s: --output specified twice\n", argv[0]);
                goto error;
            }
            if (argv[i][9] == '\0') {
                fprintf(stderr, "%s: --output requires a file name\n", argv[0]);
                goto error;
            }
            output = argv[i] + 9;
        } else if (strncmp(argv[i], "--name=", 7) == 0) {
            if (disk_name != NULL) {
                fprintf(stderr, "%s: --name specified twice\n", argv[0]);
                goto error;
            }
            disk_name = argv[i] + 7;
        } else if (strncmp(argv[i], "--id=", 5) == 0) {
            if (disk_id != NULL) {
                fprintf(stderr, "%s: --id specified twice\n", argv[0]);
                goto error;
            }
            disk_id = argv[i] + 5;
        } else {
            if (num_inputs >= SIZE(inputs)) {
                fprintf(stderr, "%s: Too many input files\n", argv[0]);
                goto error;
            }
            inputs[num_inputs++] = argv[i];
        }
    }

    // Must specify an output file
    if (output == NULL) {
        fprintf(stderr, "%s: Must specify --output=<image-name>\n", argv[0]);
        goto error;
    }

    // Defaults for name and ID
    if (disk_name == NULL) {
        disk_name = output;
    }
    if (disk_id == NULL) {
        disk_id = disk_name;
    }

    // Create the image
    bool ok = D64_format();
    if (!ok) {
        goto error;
    }

    // Copy the input files
    for (unsigned i = 0; i < num_inputs; ++i) {
        if (!D64_write_file(inputs[i])) {
            goto error;
        }
    }

    // Close the image
    ok = D64_close();
    if (!ok) {
        goto error;
    }

    return EXIT_SUCCESS;

error:
    if (image_fp != NULL) {
        fclose(image_fp);
    }
    return EXIT_FAILURE;
}
