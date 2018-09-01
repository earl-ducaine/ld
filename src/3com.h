/* Copyright 2016-2017
   Daniel Seagraves <dseagrav@lunar-tokyo.net>
   Barry Silverman <barry@disus.com>

   This file is part of LambdaDelta.

   LambdaDelta is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 2 of the License, or
   (at your option) any later version.

   LambdaDelta is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with LambdaDelta.  If not, see <http://www.gnu.org/licenses/>.
*/

/* 3Com 3C400 Multibus Ethernet */

#ifndef THREE_COM_H
#define THREE_COM_H

extern uint8_t ether_rx_buf[];

void enet_clock_pulse();
void enet_reset();
uint8_t enet_read(uint16_t addr);
void enet_write(uint16_t addr,uint8_t data);
char* set_ether_iface(char* tok);

intmax_t enet_init();
intmax_t ether_tx_pkt(intmax_t ether_fd, uint8_t* data, uint32_t len);
intmax_t enet_rx_pkt(intmax_t ether_fd, uint8_t* data, uint32_t len);

#endif // THREE_COM_H
