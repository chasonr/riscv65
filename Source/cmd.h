// cmd.h -- Command interface for 1541-Ultimate and Ultimate 64

#ifndef CMD_H
#define CMD_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

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
extern bool cmd_present(void);

// Abort any command in progress
extern void cmd_abort(void);

// Run a command as set up in a CMD_struct
extern bool cmd_xfer(CMD_struct *cmd);

#endif
