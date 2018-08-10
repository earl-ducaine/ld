

#include <stdint.h>

#include "keyboard-sdl.h"

void lam_callback() {
    // Real time passed
  real_time++;
  // Also increment status update counter
  stat_time++;
}
