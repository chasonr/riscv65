// cmd.c -- Command interface for 1541-Ultimate and Ultimate 64

#include "cmd.h"

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

// Check for the presence of the command interface
bool
cmd_present(void)
{
    return CMD_IDENTIFICATION == 0xC9;
}

// Abort any command in progress
void
cmd_abort(void)
{
    if (cmd_present()) {
        CMD_CONTROL = CMD_CLR_ERR | CMD_ABORT;
        while ((CMD_STATUS & CMD_ABORT) != 0) {}
    }
}

// Run a command as set up in a CMD_struct
bool
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

// After cmd_xfer, request second and subsequent packets
bool
cmd_xfer_next(CMD_struct *cmd)
{
    uint16_t dsize;

    // Wait for packet ready
    while ((CMD_STATUS & CMD_STATE_MASK) == CMD_STATE_BUSY) {}

    // Return false if no packet
    if ((CMD_STATUS & CMD_STATE_MASK) == CMD_STATE_IDLE) {
        return false;
    }

    // Read data response
    dsize = 0;
    while (dsize < sizeof(cmd->data) && (CMD_STATUS & CMD_DATA_AV) != 0) {
        cmd->data[dsize++] = CMD_RESPONSE_DATA;
    }
    cmd->data_size = dsize;
    CMD_CONTROL = CMD_DATA_ACC;
    return true;
}
