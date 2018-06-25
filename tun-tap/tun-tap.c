

#include <stdint.h>

#include "sdu_hw.h"

i8259 PIC[3];

// open("/dev/net/tun", O_RDWR);
// ioctl(fd, TUNSETIFF, (void *) &ifr);
// fcntl(fd,F_GETFL,0);
// fcntl(fd, F_SETFL, flags | O_NONBLOCK);


void multibus_interrupt(int irq){
  uint8_t irbit = 0x01;
  irbit <<= irq;
  /*
  if(PIC[2].IRQ & irbit){
    trace_log("SDU: FIRE MULTIBUS INTERRUPT %d (ALREADY SET!)\n",irq);
  }
  */
  PIC[2].IRQ |= irbit;
  // PIC[2].Last_IRQ &= ~irbit; // Ensure edge detector fires
  /*
  if(PIC[2].IMR & irbit){
    trace_log("SDU: FIRE MULTIBUS INTERRUPT %d (MASKED!)\n",irq);
  }
  */
}


int main(int argc, char ** argv) {
}
