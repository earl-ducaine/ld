


char ether_bpfn[64];

int enet_init(){
  struct ifreq ifr;
  int fd, err, flags;
  int x=0;
  uint32_t y;

  while(x < 16){
    sprintf(ether_bpfn,"/dev/bpf%d",x);
    // Open device
    fd = open(ether_bpfn, O_RDWR);
    if(fd < 0){
      if(errno == EBUSY){ x++; continue; }
      if(errno == ENOENT){
	printf("BPF: Unable to find available BPF device (tried %d)\n",x);
	return(0);
      }
      // Some other error happened, bail
      return(-1);
    }else{
      // Done!
      break;
    }
    x++;
  }

  // Set max packet length
  y = 0x800;
  err = ioctl(fd, BIOCSBLEN,(void *)&y);
  if(err < 0){
    close(fd);
    return(err);
  }

  // Clobber ifr
  memset(&ifr, 0, sizeof(ifr));

  // Set header-complete mode to one, since we will be providing complete packets
  y = 1;
  err = ioctl(fd, BIOCSHDRCMPLT,(void *)&y);
  if(err < 0){
    close(fd);
    return(err);
  }

  // Don't echo my own packets back to me
  y = 0;
  err = ioctl(fd, BIOCSSEESENT,(void *)&y);
  if(err < 0){
    close(fd);
    return(err);
  }

  // Operate in Immediate Mode
  y = 1;
  err = ioctl(fd, BIOCIMMEDIATE,(void *)&y);
  if(err < 0){
    close(fd);
    return(err);
  }

  // Attach to interface
  strncpy(ifr.ifr_name,ether_iface,IFNAMSIZ);
  err = ioctl(fd, BIOCSETIF,(void *)&ifr);
  if(err < 0){
    close(fd);
    return(err);
  }

  // Operate in Promiscuous Mode
  y = 1;
  err = ioctl(fd, BIOCPROMISC,(void *)&y);
  if(err < 0){
    close(fd);
    return(err);
  }

  // Become nonblocking
  flags = fcntl(fd,F_GETFL,0);
  if(flags < 0){ flags = 0; }
  fcntl(fd,F_SETFL,flags|O_NONBLOCK);

  printf("BPF: Operating\n");
  return(fd);
}

void ether_tx_pkt(uint8_t *data,uint32_t len){
  ssize_t res = 0;
  if(ether_fd < 0){
    perror("ether:ether_tx_pkt() ether_fd invalid");
    return;
  }
  printf("Ether: Sending %d bytes\n",len);
  res = write(ether_fd,data,len);
  if(res < 0){
    perror("ether:write()");
  }
  return;
}

unsigned int bpf_buf_offset = 0;
unsigned int bpf_buf_length = 0;
uint8_t ether_bpf_buf[0x800];

uint32_t enet_rx_pkt() {
  ssize_t res = 0;
  struct bpf_hdr *bpf_header;
  if(ether_fd < 0){
    perror("ether:enet_rx_pkt() ether_fd invalid");
    return 0;
  }
  if(bpf_buf_offset == 0){
    // Get more packets
    res = read(ether_fd,ether_bpf_buf,0x800);
    if(res < 0){
      if(errno != EAGAIN && errno != EWOULDBLOCK){
	perror("ether:read()");
      }
      return(0);
    }
    printf("Ether: read got %d bytes\n",(int)res);
    bpf_buf_length = res;
  }
  // There is a BPF header that must be dealt with
  printf("BPF: Header @ %d\n",bpf_buf_offset);
  bpf_header = (struct bpf_hdr *)(ether_bpf_buf+bpf_buf_offset);
  // Extract packet into ether_rx_buf+4 (to fake the 4-byte header that linux prepends)
  printf("BPF: Actual packet %d bytes @ offset %d\n",bpf_header->bh_caplen,bpf_header->bh_hdrlen);
  memcpy(ether_rx_buf+4,(uint8_t *)(ether_bpf_buf+bpf_buf_offset+bpf_header->bh_hdrlen),bpf_header->bh_caplen);
  printf("PKT: DST %.2X:%.2X:%.2X:%.2X:%.2X:%.2X SRC %.2X:%.2X:%.2X:%.2X:%.2X:%.2X PTYPE %.2X %.2X\n",
	 ether_rx_buf[4],ether_rx_buf[5],ether_rx_buf[6],ether_rx_buf[7],ether_rx_buf[8],ether_rx_buf[9],
	 ether_rx_buf[10],ether_rx_buf[11],ether_rx_buf[12],ether_rx_buf[13],ether_rx_buf[14],ether_rx_buf[15],
	 ether_rx_buf[16],ether_rx_buf[17]);
  // Do we have more packets?
  if((bpf_buf_offset+bpf_header->bh_hdrlen+bpf_header->bh_caplen) < bpf_buf_length) {
    printf("BPF: At %d, want %d, continuing buffer processing\n",
	   (bpf_buf_offset+bpf_header->bh_hdrlen+bpf_header->bh_caplen),bpf_buf_length);
    bpf_buf_offset += BPF_WORDALIGN(bpf_header->bh_hdrlen+bpf_header->bh_caplen);
  } else {
    // printf("BPF: All done!\n");
    bpf_buf_offset = 0;
  }
  if(bpf_header->bh_caplen != bpf_header->bh_datalen) {
    printf("BPF: LENGTH MISMATCH: Captured %d of %d\n",
	   bpf_header->bh_caplen,bpf_header->bh_datalen);
    // Throw away packet
    return 0;
  }
  return (bpf_header->bh_caplen + 4);
}
