// crttool.c -- tool for making CRT (VICE cartridge) images

#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Integers stored in CRT_header or CHIP_header are in big-endian order

// Cartridge header
// Reference: VICE User Manual, 17.14.1

typedef struct CRT_header {
    uint8_t signature[16];    // "C64 CARTRIDGE   " or C128, CBM2, VIC20, PLUS4
    uint8_t header_length[4]; // Length of header
    uint8_t crt_version[2];   // CRT file version; should be 0x02 0x00
    uint8_t hw_type[2];       // Hardware type
    uint8_t exrom;            // Assert EXROM if 0
    uint8_t game;             // Assert GAME if 0
    uint8_t subtype;          // Hardware subtype or revision
    uint8_t reserved[5];      // Should be 0
    uint8_t name[32];         // Name of cartridge, padded with nulls
} CRT_header;

// CHIP packet header
// Reference: VICE User Manual, 17.14.2

typedef struct CHIP_header {
    uint8_t signature[4];     // "CHIP"
    uint8_t packet_length[4]; // Sum of header length and packet length
    uint8_t chip_type[2];     // 0 = ROM, 1 = RAM, 2 = Flash, 3 = EEPROM
    uint8_t bank_number[2];   // Bank number
    uint8_t load_address[2];  // Load address
    uint8_t image_size[2];    // Image size
} CHIP_header;

static void
usage(const char *prog_name)
{
    fprintf(stderr, "Usage: %s --output=<file> --bank<num>=<file> [--bank<num>=<file>]... [--name=<name>]\n",
            prog_name);
    fprintf(stderr, "Example:\n");
    fprintf(stderr, "%s --bank0=init.bin --bank1=splash.bin --bank2=main.bin --output=game.crt --name='BIG GAME'\n",
            prog_name);
    fprintf(stderr, "Bank numbers may range from 0 to 63.\n");
}

int
main(int argc, char **argv)
{
    // Scan argv for names of banks and for the output name
    const char *output = NULL;
    const char *name = NULL;
    const char *banks[64] = { NULL };
    bool one_bank = false;
    for (int i = 1; i < argc; ++i) {
        if (strcmp(argv[i], "--help") == 0 || strcmp(argv[i], "-h") == 0) {
            usage(argv[0]);
            return EXIT_FAILURE;
        } else if (strncmp(argv[i], "--name=", 7) == 0) {
            if (name != NULL) {
                fprintf(stderr, "--name specified twice\n");
                return EXIT_FAILURE;
            }
            name = argv[i] + 7;
            if (name[0] == '\0') {
                fprintf(stderr, "--name= requires a string\n");
                return EXIT_FAILURE;
            }
        } else if (strncmp(argv[i], "--output=", 9) == 0) {
            if (output != NULL) {
                fprintf(stderr, "--output specified twice\n");
                return EXIT_FAILURE;
            }
            output = argv[i] + 9;
            if (output[0] == '\0') {
                fprintf(stderr, "--output= requires a file name\n");
                return EXIT_FAILURE;
            }
        } else if (strncmp(argv[i], "--bank", 6) == 0) {
            char *end;
            long banknum = strtol(argv[i] + 6, &end, 10);
            if (end == argv[i] + 6 || end[0] != '=' || end[1] == '\0') {
                fprintf(stderr, "--bank requires a bank number and a file name\n");
                return EXIT_FAILURE;
            }
            if (banknum < 0 || 63 < banknum) {
                fprintf(stderr, "Bank number must be 0 to 63\n");
                return EXIT_FAILURE;
            }
            if (banks[banknum] != NULL) {
                fprintf(stderr, "--bank%ld specified twice\n", banknum);
                return EXIT_FAILURE;
            }
            banks[banknum] = end + 1;
            one_bank = true;
        } else {
            fprintf(stderr, "Unknown option: %s\n", argv[i]);
            return EXIT_FAILURE;
        }
    }
    if (output == NULL) {
        fprintf(stderr, "Must have an --output option\n");
        return EXIT_FAILURE;
    }
    if (!one_bank) {
        fprintf(stderr, "Must have at least one --bank option\n");
        return EXIT_FAILURE;
    }

    // Open the output file
    FILE *crt = fopen(output, "wb");
    if (crt == NULL) {
        perror(output);
        return EXIT_FAILURE;
    }

    // Write the header
    {
        CRT_header header;
        memset(&header, 0, sizeof(header));

        memcpy(header.signature, "C64 CARTRIDGE   ", 16);
        header.header_length[3] = (uint8_t)sizeof(header);
        header.crt_version[0] = 0x01;
        header.crt_version[1] = 0x00;
        header.hw_type[1] = 5; // Ocean type 1
        header.exrom = 0;
        header.game = 0;
        header.subtype = 0;
        if (name == NULL) {
            name = output;
        }
        unsigned i;
        // File format specifies that the name is upper case
        for (i = 0; i < sizeof(header.name) && name[i] != '\0'; ++i) {
            header.name[i] = toupper(name[i]);
        }
        for (; i < sizeof(header.name); ++i) {
            header.name[i] = ' ';
        }

        if (fwrite(&header, sizeof(header), 1, crt) != 1) {
            perror(output);
            fclose(crt);
            return EXIT_FAILURE;
        }
    }

    // Write the bank images
    for (unsigned i = 0; i < 64; ++i) {
        if (banks[i] != NULL) {
            uint8_t image[8192];
            FILE *inp = fopen(banks[i], "rb");
            if (inp == NULL) {
                perror(banks[i]);
                fclose(crt);
                return EXIT_FAILURE;
            }
            size_t s = fread(image, 1, sizeof(image), inp);
            if (ferror(inp)) {
                perror(banks[i]);
                fclose(crt);
                fclose(inp);
                return EXIT_FAILURE;
            }
            fclose(inp);
            memset(image+s, 0, sizeof(image)-s);

            CHIP_header header2;
            // Using memset here causes packet_length to get screwed up
            header2.signature[0] = 'C';
            header2.signature[1] = 'H';
            header2.signature[2] = 'I';
            header2.signature[3] = 'P';
            header2.packet_length[0] = (uint8_t)((sizeof(image) + sizeof(header2)) >> 24);
            header2.packet_length[1] = (uint8_t)((sizeof(image) + sizeof(header2)) >> 16);
            header2.packet_length[2] = (uint8_t)((sizeof(image) + sizeof(header2)) >>  8);
            header2.packet_length[3] = (uint8_t)((sizeof(image) + sizeof(header2)) >>  0);
            header2.chip_type[0] = 0;
            header2.chip_type[1] = 0;
            header2.bank_number[0] = (uint8_t)(i >> 8);
            header2.bank_number[1] = (uint8_t)(i >> 0);
            header2.load_address[0] = 0x80;
            header2.load_address[1] = 0x00;
            header2.image_size[0] = (uint8_t)(sizeof(image) >> 8);
            header2.image_size[1] = (uint8_t)(sizeof(image) >> 0);
            if (fwrite(&header2, sizeof(header2), 1, crt) != 1) {
                perror(output);
                fclose(crt);
                return EXIT_FAILURE;
            }
            if (fwrite(image, 1, sizeof(image), crt) != sizeof(image)) {
                perror(output);
                fclose(crt);
                return EXIT_FAILURE;
            }
        }
    }

    fclose(crt);
    return EXIT_SUCCESS;
}
