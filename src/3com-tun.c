

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <net/if.h>
#include <errno.h>
#include <error.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <linux/if_tun.h>

#include "3com.h"

// Linux tuntap interface
char ether_iface[30] = "ldtap";

char* set_ether_iface(char* tok) {
  return strncpy(ether_iface, tok, 30);
}

char* get_ether_iface() {
  return ether_iface;
}

intmax_t enet_init() {
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

intmax_t  ether_tx_pkt(intmax_t ether_fd, uint8_t* data, uint32_t len) {
  intmax_t  res = write(ether_fd, data-4, len + 4);
  if (res < 0) {
    int error_number = errno;
    error(0,0, "%s", strerror(error_number));
    error(0, 0, "read() generated error code(%d); res (%li)", error_number, res);
  }
  return res;
}

intmax_t enet_rx_pkt(intmax_t ether_fd, uint8_t* data, uint32_t len) {
  // ssize_t res = read(ether_fd, ether_rx_buf, (0x800 - 2));
  intmax_t res = read(ether_fd, data, len);
  if (res < 0) {
    if ((errno != EAGAIN) && (errno != EWOULDBLOCK)) {
      error(0, 0, "read() generated error code(%d); res (%li)", errno, res);
    } else {
      // generates too many messages
      // error(0,
      // 	    0,
      // 	    "read() generated error code(%d): EAGAIN or EWOULDBLOCK; res (%d)",
      // 	    errno, res);
    }
  }
  return res;
}
