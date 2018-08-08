// -*-Mode: C -*-



#ifndef KERNEL_H
#define KERNEL_H

int lam_main(int argc, char *argv[]);
void FB_dump(int vn);
int put_rx_ring(int vn,unsigned char ch);
int put_mouse_rx_ring(int vn,unsigned char ch);
void write_nvram();
void write_rtc_nvram();

#endif // KERNEL_H
