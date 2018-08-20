

#define USES_ETHER_CODE "null"

int enet_init(){
  return(0);
}

void ether_tx_pkt(uint8_t *data __attribute__ ((unused)),uint32_t len __attribute__ ((unused))){
  return;
}

uint32_t enet_rx_pkt() {
  return(0);
}
