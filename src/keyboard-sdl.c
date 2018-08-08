

#include <stdint.h>

#include "keyboard-sdl.h"

// Timer callback
uint32_t sdl_timer_callback(uint32_t interval,
			    void *param __attribute__ ((unused))) {
  // Real time passed
  real_time++;
  // Also increment status update counter
  stat_time++;
  // Return next interval
  return(interval);
}
