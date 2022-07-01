/* kernal.h -- system calls to access the Kernal */

#ifndef KERNAL_H
#define KERNAL_H

int CBM_acptr(void);
int CBM_chkin(unsigned char filenum);
int CBM_chkout(unsigned char filenum);
int CBM_chrin(void);
int CBM_chrout(unsigned char chr);
int CBM_cint(void);
int CBM_ciout(unsigned char chr);
int CBM_clall(void);
int CBM_close(unsigned char filenum);
int CBM_clrchn(void);
int CBM_getin(void);
int CBM_ioinit(void);
int CBM_listen(unsigned char device);
int CBM_open(const char *name, unsigned char filenum, unsigned char device,
             unsigned char sec_addr);
int CBM_get_cursor(unsigned char *row, unsigned char *col);
int CBM_set_cursor(unsigned char row, unsigned char col);
int CBM_rdtim(void);
int CBM_readst(void);
int CBM_second(unsigned char sec_addr);
int CBM_settim(int time);
int CBM_stop(void);
int CBM_talk(unsigned char device);
int CBM_tksa(unsigned char sec_addr);
int CBM_unlsn(void);
int CBM_untlk(void);

/* errors */
// 3: file not open
// 5: device not present
// 6: file not an input file
// 7: file not an output file

/* CBM_readst return */
// 0x01: time out write (serial)
// 0x02: time out read (serial)
// 0x04: short block (tape)
// 0x08: long block (tape)
// 0x10: read error (tape)
// 0x20: checksum error (tape)
// 0x40: end of file (tape), EOI (serial)
// 0x80: end of tape (tape), device not present (serial)

#endif
