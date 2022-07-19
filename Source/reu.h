// reu.h -- functions to access the RAM Expansion Unit

#ifndef REU_H
#define REU_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

// Detect the presence of a RAM Expansion Unit
extern bool reu_present(void);

// Write to REU
extern uint8_t reu_write(uint32_t reu_address, void const *data, size_t size);

// Fill REU
extern uint8_t reu_fill(uint32_t reu_address, uint8_t byte, uint32_t size);

// Read from REU
extern uint8_t reu_read(uint32_t reu_address, void *data, size_t size);

// Determine the size of the REU
extern uint32_t reu_size(void);

#endif
