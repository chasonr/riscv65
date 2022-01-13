/* 6502emu.h */

#ifndef X_6502EMU_H
#define X_6502EMU_H

#include <stdbool.h>
#include <stdint.h>

void emu_init(void);
void emu_write_byte(uint16_t address, uint8_t byte);
int emu_read_byte(uint16_t address);
void emu_set_regs(
        uint8_t a,
        uint8_t x,
        uint8_t y,
        uint8_t s,
        uint8_t flags,
        uint16_t pc);
bool emu_read_rom(uint16_t address, const char *name);
bool emu_written(uint16_t address);
bool emu_run(void);
extern unsigned long emu_cycles;
extern uint8_t emu_ext_mem[0x1000000]; /* REU and GeoRAM */

#endif
