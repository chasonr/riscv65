// reu.c -- functions to access the RAM Expansion Unit

#include <string.h>
#include "reu.h"

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
bool
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
uint8_t
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
uint8_t
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
uint8_t
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
uint32_t
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
