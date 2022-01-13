/* riscv-vm.h */

#ifndef RISCV_VM_H
#define RISCV_VM_H

#include <stdbool.h>
#include <stdint.h>

bool VM_init(void);
bool VM_load(const char *filename);
void VM_run(void);
bool VM_kernal(uint16_t addr, uint8_t *a, uint8_t *x, uint8_t *y);

#endif
