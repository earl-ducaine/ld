

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <net/if.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <linux/if_tun.h>

#include "3com.h"

// Linux tuntap interface
char ether_iface[30] = "ldtap";

int enet_init() {
  struct ifreq ifr;
  int fd,
    err,
    flags;
  char *tundev = "/dev/net/tun";
  // Open device
  fd = open(tundev, O_RDWR);
  if (fd < 0) {
    return fd;
  }
  // Clobber ifr
  memset(&ifr, 0, sizeof(ifr));
  // Set flags for TAP device
  ifr.ifr_flags = IFF_TAP;
  strncpy(ifr.ifr_name, ether_iface, IFNAMSIZ);
  // Create queue
  err = ioctl(fd, TUNSETIFF, (void*) &ifr);
  if (err < 0) {
    close(fd);
    return err;
  }
  // Become nonblocking
  flags = fcntl(fd,F_GETFL,0);
  if (flags < 0) {
    flags = 0;
  }
  fcntl(fd, F_SETFL, flags | O_NONBLOCK);
  // All done!
  return fd;
}

void ether_tx_pkt(uint8_t* data, uint32_t len) {
  ssize_t res = 0;
  if (ether_fd < 0) {
    perror("ether: ether_fd not valid");
    return;
  }
  printf("Ether: Sending %d bytes\n",len+4);
  res = write(ether_fd,data-4,len+4);
  if (res < 0) {
    perror("ether:write()");
  }
}

uint32_t enet_rx_pkt() {
  ssize_t res = 0;
  if (ether_fd < 0) {
    return 0;
  }
  res = read(ether_fd, ether_rx_buf, (0x800-2));
  if (res < 0) {
    if (errno != EAGAIN && errno != EWOULDBLOCK) {
      perror("ether:read()");
    }
    return 0;
  }
  // printf("Done! Got %d bytes\n",(int)res);
  return res;
}
