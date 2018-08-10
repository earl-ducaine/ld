
#include <stdbool.h>
#include <SDL.h>
#include <SDL_keysym.h>
#include <sys/time.h>
#include <signal.h>
#include <unistd.h>

#include "ld.h"
#include "keyboard-sdl.h"
#include "lambda_cpu.h"
#include "tapemaster.h"
#include "kernel.h"
#include "vcmem.h"

extern DisplayState* ds;

// Processor states
extern struct lambdaState pS[];

// SDL1 state
SDL_Surface *screen;
SDL_TimerID SDLTimer;
int video_width = VIDEO_WIDTH;
int video_height = VIDEO_HEIGHT;

void init_sdl_to_keysym_map(void) {
  int x = 0;
  while(x < 512) {
    map[x] = 0;
    modmap[x] = 0;
    x++;
  }
  // Default key map
  map['1'] = 0121; // 1
  map['2'] = 0061; // 2
  map['3'] = 0161; // 3
  map['4'] = 0011; // 4
  map['5'] = 0111; // 5
  map['6'] = 0051; // 6
  map['7'] = 0151; // 7
  map['8'] = 0031; // 8
  map['9'] = 0071; // 9
  map['0'] = 0171; // 0
  map['-'] = 0131; // -
  //  map['+'] = 0x2f; // + is shifted =
  map['='] = 0126;              /* not keypad-equal, real equals */

  map['Q'] = 0122; // Q
  map['W'] = 0062; // W
  map['E'] = 0162; // E
  map['R'] = 0012; // R
  map['T'] = 0112; // T
  map['Y'] = 0052; // Y
  map['U'] = 0152; // U
  map['I'] = 0032; // I
  map['O'] = 0072; // O
  map['P'] = 0172; // P

  map['A'] = 0123; // A
  map['S'] = 0063; // S
  map['D'] = 0163; // D
  map['F'] = 0013; // F
  map['G'] = 0113; // G
  map['H'] = 0053; // H
  map['J'] = 0153; // J
  map['K'] = 0033; // K
  map['L'] = 0073; // L

  map['Z'] = 0124; // Z
  map['X'] = 0064; // X
  map['C'] = 0164; // C
  map['V'] = 0014; // V
  map['B'] = 0114; // B
  map['N'] = 0054; // N
  map['M'] = 0154; // M

  // map['['] = 0132; // Unshifted (, [ is shiftstate
  // map[']'] = 0137; // Unshifted ), ] is shiftstate

  map['('] = 0132; // Unshifted (, [ is shiftstate
  map[')'] = 0137; // Unshifted ), ] is shiftstate

  map[';'] = 0173;
  map['`'] = 0077;
  // map['~'] = 061;               /* special treatment (this is too useful to remap) */
  map['\\'] = 0037;
  map[' '] = 0134;

  map[','] = 0034;
  map['.'] = 0074;
  map['/'] = 0174;
  map[';'] = 0173;
  map['\''] = 0133;

  map[SDLK_RETURN] = 0136; // ENTER
  map[SDLK_BACKSPACE] = 0023; // BACKSPACE (MAPS TO RUBOUT)
  map[SDLK_TAB] = 0022; // TAB
  map[SDLK_ESCAPE] = 0023; // ESC

  // LMI arrows were shift states?
  // Or maybe it's the HAND keys?
  map[SDLK_UP] = 0106; // hand up
  map[SDLK_DOWN] = 0176; // hand down
  map[SDLK_RIGHT] = 0017; // hand right
  map[SDLK_LEFT] = 0117; // hand left

  // No home or insert, no F1 or F2
  map[SDLK_HOME] = 0x15; /* HOME - F1 */
  map[SDLK_INSERT] = 0x14; /* INSERT - F2 */

  // No page up or down
  map[SDLK_PAGEUP] = 0067; /* PAGEUP - ABORT */
  map[SDLK_PAGEDOWN] = 0047; /* PAGEDOWN - RESUME */

  map[SDLK_END] = 0156; /* END - END */

  map[SDLK_F1] = 0141; /* F1 - SYSTEM */
  map[SDLK_F2] = 0042; /* F2 - NETWORK */
  map[SDLK_F3] = 0046; /* F3 - STATUS */
  map[SDLK_F4] = 0040; /* F4 - TERMINAL */
  map[SDLK_F5] = 0116; /* F5 - HELP */
  map[SDLK_F6] = 0110; /* F6 - CLEAR */
  map[SDLK_F7] = 0167; /* F7 - BREAK */
  // SDLK_F9 = CONSOLE SWITCH
  // SDLK_F10 = MOUSE CAPTURE/RELEASE
  // SDLK_F11 = RETURN TO NEWBOOT
  // SDLK_F12 = DUMP STATE

  map[SDLK_CAPSLOCK] = 0x03; // CAPSLOCK

  map[SDLK_RSHIFT] = 0025; // RIGHT SHIFT
  map[SDLK_LSHIFT] = 0024; // LEFT SHIFT

  map[SDLK_RCTRL] = 0044; // RIGHT CTRL is 0026, but we want LEFT GREEK which is 0044
  map[SDLK_LCTRL] = 0020; // LEFT CTRL
  map[SDLK_RALT] = 0005; // Mapped to LEFT SUPER // 0165; // RIGHT ALT (META)
  map[SDLK_LALT] = 0045; // LEFT ALT (META)

  map[SDLK_RSUPER] = 0065; // RIGHT windows (SUPER)
  map[SDLK_LSUPER] = 0005; // LEFT windows (SUPER)

  // Default modifier map
  modmap[SDLK_LSHIFT] = KB_BB_LSHIFT;
  modmap[SDLK_RSHIFT] = KB_BB_RSHIFT;
  modmap[SDLK_LCTRL] = KB_BB_LCTL;
  modmap[SDLK_RCTRL] = KB_BB_GREEK;
  modmap[SDLK_LALT] = KB_BB_LMETA;
  modmap[SDLK_RALT] = KB_BB_LSUPER;
  modmap[SDLK_MENU] = KB_BB_RHYPER;
}


// Timer callback
uint32_t sdl_timer_callback(uint32_t interval,
			    void *param __attribute__ ((unused))) {
  lam_callback();
  // Real time passed
  return(interval);
}

SDL_Scancode LAM_CODE_F12 = SDLK_F12;
SDL_Scancode LAM_CODE_F11 = SDLK_F11;

void kbd_handle_char(int code, int down) {
  int sdlchar = code;
  unsigned char outchar = 0;
  // Check for debug
  if (sdlchar == LAM_CODE_F12) {
    if (down) {
      if (((kb_buckybits & KB_BB_LSHIFT) |
	   (kb_buckybits & KB_BB_RSHIFT)) !=
	  0) {
        printf("DEBUG: DUMP REQUESTED FROM CONSOLE\n");
        lambda_dump(DUMP_ALL);
        FB_dump(0);
        FB_dump(1);
      } else {
        tapemaster_open_next();
      }
    }
    return;
  }
  // Check for return-to-newboot key
  if (sdlchar == LAM_CODE_F11) {
    // Keystroke is control-meta-control-meta-<LINE>
    // 0020 0045 0026 0165 0036
    if (down) {
      // I don't know how this translates to that, but it works.  This
      // is the sequence of bytes the ucode looks for.
      put_rx_ring(active_console, 0x60);
      put_rx_ring(active_console, 0x9F);
    }
    return;
  }
  // Check for decapture/pointer-hide-show key
  if (sdlchar == SDLK_F10) {
    if (down) {
      if (mouse_capture != 0) {
	mouse_capture = 0;
	if (mouse_op_mode == 0) {
	  SDL_WM_GrabInput(SDL_GRAB_OFF);
	}
	SDL_ShowCursor(SDL_ENABLE);
      } else {
	mouse_capture = 1;
	if (mouse_op_mode == 0) {
	  SDL_WM_GrabInput(SDL_GRAB_ON);
	}
	SDL_ShowCursor(SDL_DISABLE);
      }
    }
    return;
  }
  // Check for console switch key
#ifdef CONFIG_2X2
  if (sdlchar == SDLK_F9) {
    if (down) {
      // Switch active console
      active_console ^= 1;
      printf("CONSW: %d\n",active_console);
      // Update window title
      stat_time = 20;
      // Refresh display bitmap from stored image
      uint32_t *p = screen->pixels;
      uint32_t *s = FB_Image[active_console];
      int i,j;
      for (i = 0; i < video_width; i++) {
	for (j = 0; j < video_height; j++) {
	  *(p++) = *(s++);
	}
      }
      // Redraw it
      SDL_UpdateRect(screen, 0, 0, video_width, video_height);
      // Reset accumulation
      u_minh = 0x7fffffff;
      u_maxh = 0;
      u_minv = 0x7fffffff;
      u_maxv = 0;
      // If we are in shared mouse mode, move the pointer to where the
      // new console thinks it should be.
      if ((mouse_op_mode == 1) && (cp_state[active_console] == 3)) {
        warp_mouse_callback(active_console);
      }
    }
    return;
  }
#endif
  // For now, fold lower case to upper case (because we're ignoring
  // modifiers)
  if (sdlchar >= 'a' && sdlchar <= 'z') {
    sdlchar -= ' ';
  }
  // Obtain keymap entry
  outchar = map[sdlchar];
  // We send 3 characters. First is keycode, second is key state +
  // bucky bits, third is source ID.
  //
  // Keycode
  put_rx_ring(active_console,outchar);
  // Next is key up/down state and bucky bits
  outchar = 0x80; // This is the "second byte" flag
  if (down) {
    // Key Down (flag)
    outchar |= 0x40;
    if (modmap[sdlchar] != 0) {
      kb_buckybits |= modmap[sdlchar];
    }
    // Take "down" bucky bits
    if (((kb_buckybits&KB_BB_LSHIFT)|(kb_buckybits&KB_BB_RSHIFT)) != 0) {
      outchar |= 0x20;
    }
    if (((kb_buckybits&KB_BB_LCTL)|(kb_buckybits&KB_BB_RCTL)) != 0) {
      outchar |= 0x10;
    }
    if (((kb_buckybits&KB_BB_LMETA)|(kb_buckybits&KB_BB_RMETA)) != 0) {
      outchar |= 0x08;
    }
    if (((kb_buckybits&KB_BB_LSUPER)|(kb_buckybits&KB_BB_RSUPER)) != 0) {
      outchar |= 0x04;
    }
    if (((kb_buckybits&KB_BB_LHYPER)|(kb_buckybits&KB_BB_RHYPER)) != 0) {
      outchar |= 0x02;
    }
    if ((kb_buckybits&KB_BB_GREEK) != 0) {
      outchar |= 0x01;
    }
  } else {
    // Key Up
    if (modmap[sdlchar] != 0) {
      kb_buckybits &= ~modmap[sdlchar];
    }
    // Take "up" bucky bits
    if ((kb_buckybits&KB_BB_MODELOCK) != 0) {
      outchar |= 0x10;
    }
    if ((kb_buckybits&KB_BB_ALTLOCK) != 0) {
      outchar |= 0x08;
    }
    if ((kb_buckybits&KB_BB_CAPSLOCK) != 0) {
      outchar |= 0x04;
    }
    if ((kb_buckybits&KB_BB_REPEAT) != 0) {
      outchar |= 0x02;
    }
    if (((kb_buckybits&KB_BB_LTOP)|(kb_buckybits&KB_BB_RTOP)) != 0) {
      outchar |= 0x01;
    }
  }
  // Send result
  put_rx_ring(active_console, outchar);
  // Third byte is Source ID. (Newer Keyboard)
  put_rx_ring(active_console, 0x02);
  // printf("KB: Key event sent\n");
  vcmem_kb_int(active_console);
}

void sdl_system_shutdown_request(void) {
  exit(0);
}

static void sdl_process_key(SDL_KeyboardEvent* ev, int updown) {
  kbd_handle_char(ev->keysym.sym, updown);
}

void sdl_send_mouse_event(void) {
  int state,
    xm,
    ym;
  uint8_t buttons = 0x07;
  if (mouse_op_mode == 0) {
    // Direct Mode
    state = SDL_GetRelativeMouseState(&xm, &ym);
    // Disregard mouse when not captured, unless we are recapturing it.
    if (mouse_capture == 0 && (state & SDL_BUTTON(SDL_BUTTON_LEFT))) {
      mouse_capture = 2;
      return;
    }
    if (mouse_capture == 2 && !(state & SDL_BUTTON(SDL_BUTTON_LEFT))) {
      mouse_capture = 1;
      SDL_WM_GrabInput(SDL_GRAB_ON);
      SDL_ShowCursor(SDL_DISABLE);
      return;
    }
    if (mouse_capture != 1) {
      return;
    }
    if (cp_state[active_console] != 3) { return; }
    // Proceed
    if (state & SDL_BUTTON(SDL_BUTTON_LEFT)) {
      buttons ^= 0x04;
    }
    if (state & SDL_BUTTON(SDL_BUTTON_MIDDLE)) {
      buttons ^= 0x02;
    }
    if (state & SDL_BUTTON(SDL_BUTTON_RIGHT)) {
      buttons ^= 0x01;
    }
    if ((mouse_phase == 1) && (buttons != mouse_last_buttons)) {
      put_mouse_rx_ring(active_console, 0);
      put_mouse_rx_ring(active_console, 0);
      mouse_phase ^= 1;
    }
    // Construct packet -- Y movement is reversed
    ym = -ym;
    // Scale movement
    xm /= 2;
    ym /= 2;
    if (xm == 0 && ym == 0 && buttons == mouse_last_buttons) {
      return;
    }
    // printf("MOUSE: Movement: %d/%d buttons 0x%.2x\n",xm,ym,buttons);
    // Construct mouse packet and send it
    if (mouse_phase == 0) {
      put_mouse_rx_ring(active_console,0x80|buttons); // Buttons
      put_mouse_rx_ring(active_console,xm&0xFF);
      put_mouse_rx_ring(active_console,ym&0xFF);
    } else {
      put_mouse_rx_ring(active_console,xm&0xFF);
      put_mouse_rx_ring(active_console,ym&0xFF);
    }
    mouse_phase ^= 1;
    mouse_last_buttons = buttons;
  }
  if (mouse_op_mode == 1) {
    // Shared Mode
    // If lisp is not running, return
    if (cp_state[active_console] != 3) { return; }
    state = SDL_GetMouseState(&xm, &ym);
    // If the inhibit counter is nonzero, throw away this update (it's fake)
    if (mouse_update_inhibit > 0) { mouse_update_inhibit--; return; }
    // Otherwise, proceed
    if (state & SDL_BUTTON(SDL_BUTTON_LEFT)) { buttons ^= 0x04; }
    if (state & SDL_BUTTON(SDL_BUTTON_MIDDLE)) { buttons ^= 0x02; }
    if (state & SDL_BUTTON(SDL_BUTTON_RIGHT)) { buttons ^= 0x01; }
    // printf("MOUSE: Movement: %d/%d buttons 0x%.2x\n",xm,ym,buttons);
    // Do we need to update buttons?
    if (buttons != mouse_last_buttons) {
      // Yes - Generate a mouse packet (no movement, just buttons)
      if (mouse_phase == 1) {
	put_mouse_rx_ring(active_console,0);
	put_mouse_rx_ring(active_console,0);
	mouse_phase ^= 1;
      }
      put_mouse_rx_ring(active_console,0x80|buttons); // Buttons
      put_mouse_rx_ring(active_console,0);
      put_mouse_rx_ring(active_console,0);
      mouse_phase ^= 1;
      mouse_last_buttons = buttons;
    } else {
      // No, update position
      pS[active_console].Amemory[mouse_x_loc[active_console]] = 0xA000000|xm;
      pS[active_console].Amemory[mouse_y_loc[active_console]] = 0xA000000|ym;
      pS[active_console].Amemory[mouse_wake_loc[active_console]] = 0x6000005; // T
    }
  }
}

// Lisp updated the mouse position
void warp_mouse_callback(int cp) {
  // Make sure we care first
  if ((mouse_op_mode != 1) ||
      (cp_state[cp] != 3) || (cp != active_console)) {
    return;
  }
  mouse_update_inhibit++;
  // printf("WARP MOUSE 0x%X,0x%X\n",pS[cp].Amemory[mouse_x_loc[cp]],pS[cp].Amemory[mouse_y_loc[cp]]);
  SDL_WarpMouse((pS[cp].Amemory[mouse_x_loc[cp]]&0xFFFF),
		(pS[cp].Amemory[mouse_y_loc[cp]]&0xFFFF));
}

void set_bow_mode(int vn, int mode) {
  int i,
    j;
  if (black_on_white[vn] == mode) {
    // printf("BLACK-ON-WHITE MODE unchanged\n");
    return;                   /* noop */
  }
  printf("VC %d BLACK-ON-WHITE MODE now %d\n",vn,mode);
  // update
  black_on_white[vn] = mode;
  // invert pixels
  uint32_t *p = FB_Image[vn];
  if (vn == active_console) {
    uint32_t *b = screen->pixels;
    for (i = 0; i < video_width; i++) {
      for (j = 0; j < video_height; j++) {
	*b = *p = (*p == pixel_off ? pixel_on : pixel_off);
	p++; b++;
      }
    }
    // Refresh display
    SDL_UpdateRect(screen, 0, 0, video_width, video_height);
  } else {
    for (i = 0; i < video_width; i++) {
      for (j = 0; j < video_height; j++) {
	*p = ((*p == pixel_off) ? pixel_on : pixel_off);
	p++;
      }
    }
  }
}

void accumulate_update(int h, int v, int hs, int vs) {
  if (h < u_minh) u_minh = h;
  if (h+hs > u_maxh) u_maxh = h+hs;
  if (v < u_minv) u_minv = v;
  if (v+vs > u_maxv) u_maxv = v+vs;
}

void send_accumulated_updates(void) {
  int hs, vs;

  hs = u_maxh - u_minh;
  vs = u_maxv - u_minv;
  if (u_minh != 0x7fffffff && u_minv != 0x7fffffff &&
      u_maxh && u_maxv)
    {
      SDL_UpdateRect(screen, u_minh, u_minv, hs, vs);
    }

  u_minh = 0x7fffffff;
  u_maxh = 0;
  u_minv = 0x7fffffff;
  u_maxv = 0;
}

void sdl_refresh() {
  SDL_Event ev1;
  SDL_Event* ev = &ev1;
  send_accumulated_updates();
  while (SDL_PollEvent(ev)) {
    switch (ev->type) {
    case SDL_VIDEOEXPOSE:
      SDL_UpdateRect(screen, 0, 0, screen->w, screen->h);
      break;
    case SDL_KEYDOWN:
      sdl_process_key(&ev->key, 1);
      break;
    case SDL_KEYUP:
      sdl_process_key(&ev->key, 0);
      break;

    case SDL_MOUSEMOTION:
    case SDL_MOUSEBUTTONDOWN:
    case SDL_MOUSEBUTTONUP:
    case SDL_APPMOUSEFOCUS:
      sdl_send_mouse_event();
      break;

    case SDL_QUIT:
      if (quit_on_sdl_quit != 0) {
	sdl_system_shutdown_request();
      }
      break;

    default:
      break;
    }
  }
}

void sdl_cleanup(void) {
  if (mouse_op_mode == 0) {
    SDL_WM_GrabInput(SDL_GRAB_OFF);
  }
  SDL_ShowCursor(SDL_ENABLE);
  if (sdu_conn_fd > 0) {
    close(sdu_conn_fd);
  }
  if (sdu_fd > 0) {
    close(sdu_fd);
  }
  write_nvram();
  write_rtc_nvram();
  SDL_Quit();
}

int sdl_init(int width, int height) {
  int i,
    j;
  video_width = width;
  video_height = height;
  if (SDL_Init(SDL_INIT_VIDEO|SDL_INIT_TIMER|SDL_INIT_NOPARACHUTE)) {
    fprintf(stderr, "SDL initialization failed\n");
    exit(-1);
  }
  /* NOTE: we still want Ctrl-C to work - undo the SDL redirections*/
  signal(SIGINT, SIG_DFL);
  signal(SIGQUIT, SIG_DFL);
  // Obtain icon. It must be a 32x32 pixel 256-color BMP image. RGB
  // 255,0,255 is used for transparency.
  SDL_Surface* icon = SDL_LoadBMP("icon.bmp");
  if (icon != NULL) {
    SDL_SetColorKey(icon,
		    SDL_SRCCOLORKEY,
		    SDL_MapRGB(icon->format,
			       255,
			       0,
			       255));
    SDL_WM_SetIcon(icon, 0);
  } else {
    printf("Failed to open icon.bmp");
  }
  // SDL was burning a bunch of time forcing 8bpp, so use 32bpp instead.
  screen = SDL_SetVideoMode(width, height, 32,
			    SDL_HWSURFACE | SDL_ASYNCBLIT | SDL_HWACCEL);
  if (!screen) {
    fprintf(stderr, "Could not open SDL display\n");
    exit(-1);
  }
  ds->data = screen->pixels;
  ds->linesize = screen->pitch;
  ds->depth = screen->format->BitsPerPixel;
  ds->width = width;
  ds->height = height;
  // Window title, then icon title (?)
  SDL_WM_SetCaption("LambdaDelta", "LambdaDelta");
  SDL_EnableKeyRepeat(250, 50);
  // Clear display and stored bitmaps
  uint32_t *p = screen->pixels;
  for (i = 0; i < video_width; i++) {
    for (j = 0; j < video_height; j++)
      *p++ = pixel_off;
  }
  p = FB_Image[0];
  for (i = 0; i < video_width; i++) {
    for (j = 0; j < video_height; j++) {
      *p++ = pixel_off;
    }
  }
  p = FB_Image[1];
  for (i = 0; i < video_width; i++) {
    for (j = 0; j < video_height; j++) {
      *p++ = pixel_off;
    }
  }
  // Redraw it
  SDL_UpdateRect(screen, 0, 0, video_width, video_height);
  // Grab the mouse if we are in direct mode
  if (mouse_op_mode == 0) {
    SDL_WM_GrabInput(SDL_GRAB_ON);
  }
  SDL_ShowCursor(SDL_DISABLE);
  atexit(sdl_cleanup);
  // Kick interval timer
  SDLTimer = SDL_AddTimer(100, sdl_timer_callback, NULL);
  if (SDLTimer == NULL) {
    fprintf(stderr,"Unable to start interval timer\n");
    exit(-1);
  }
  return 0;
}

// Framebuffer management -- Given 1BPP data and a vcmem framebuffer
// address, translate to 32BPP and write to host.
void framebuffer_update_word(int vn,uint32_t addr,uint32_t data) {
  // Row and column of guest write
  uint32_t row,col;
  // Actual host FB offset
  uint32_t outpos;
  // Mask for pixel state
  uint64_t mask = 1;
  // Address of framebuffer
  uint32_t* FrameBuffer;
  // This many pixels in
  col = addr * 8;
  // Obtain row
  row = (col / 1024);
  col -= (row*1024); // Remove row pixels
  FrameBuffer = (uint32_t *)screen->pixels;
  outpos = col+((screen->pitch/4)*row);
  if (outpos >= (uint32_t)(video_width*video_height)) {
    return;
  }
  if (active_console == vn) {
    while (mask < 0x100000000LL) {
      if ((((black_on_white[vn] == 0) &&
	    (data & mask) != mask)) ||
	  ((black_on_white[vn] == 1) && ((data & mask) == mask))) {
	FB_Image[vn][outpos] = FrameBuffer[outpos] = pixel_on;
      } else {
	FB_Image[vn][outpos] = FrameBuffer[outpos] = pixel_off;
      }
      outpos++;
      mask <<= 1;
    }
    accumulate_update(col, row, 32, 1);
  } else {
    while(mask < 0x100000000LL) {
      if (((black_on_white[vn] == 0) &&
	   (data&mask) != mask) ||
	  ((black_on_white[vn] == 1) &&
	   ((data&mask) == mask))) {
	FB_Image[vn][outpos] = pixel_on;
      } else {
	FB_Image[vn][outpos] = pixel_off;
      }
      outpos++;
      mask <<= 1;
    }
  }
}

void framebuffer_update_hword(int vn, uint32_t addr, uint16_t data) {
  // Row and column of guest write
  uint32_t row,
    col;
  // Actual host FB offset
  uint32_t outpos;
  // Mask for pixel state
  uint64_t mask = 1;
  // Address of framebuffer
  uint32_t* FrameBuffer;
  // This many pixels in
  col = addr * 8;
  // Obtain row
  row = (col / 1024);
  // Remove row pixels
  col -= (row * 1024);
  FrameBuffer = (uint32_t*) screen->pixels;
  outpos = col + ((screen->pitch / 4) * row);
  if (outpos >= (uint32_t)(video_width*video_height)) {
    return;
  }
  if (active_console == vn) {
    while (mask < 0x10000LL) {
      if ((((black_on_white[vn] == 0) &&
	    (data & mask) != mask)) ||
	  ((black_on_white[vn] == 1) && ((data & mask) == mask))) {
	FB_Image[vn][outpos] = FrameBuffer[outpos] = pixel_on;
      } else {
	FB_Image[vn][outpos] = FrameBuffer[outpos] = pixel_off;
      }
      outpos++;
      mask <<= 1;
    }
    accumulate_update(col, row, 16, 1);
  } else {
    while (mask < 0x10000LL) {
      if (((black_on_white[vn] == 0) &&
	   ((data & mask) != mask)) ||
	  ((black_on_white[vn] == 1) && ((data & mask) == mask))) {
	FB_Image[vn][outpos] = pixel_on;
      } else {
	FB_Image[vn][outpos] = pixel_off;
      }
      outpos++;
      mask <<= 1;
    }
  }
}

void framebuffer_update_byte(int vn, uint32_t addr, uint8_t data) {
  // Given 1BPP data and a vcmem framebuffer address, translate to
  // 32BPP and write to host
  //
  // Row and column of guest write
  uint32_t row,
    col;
  // Actual host FB offset
  uint32_t outpos;
  // Mask for pixel state
  uint32_t mask = 1;
  // Address of framebuffer
  uint32_t *FrameBuffer;
  // This many pixels in
  col = addr * 8;
  // Obtain row
  row = (col / 1024);
  // Remove row pixels
  col -= (row * 1024);
  FrameBuffer = (uint32_t *)screen->pixels;
  outpos = col + ((screen->pitch / 4) * row);
  if (outpos >= (uint32_t) (video_width * video_height)) {
    return;
  }
  if (active_console == vn) {
    while(mask < 0x100) {
      if (((black_on_white[vn] == 0) &&
	   ((data & mask) != mask)) ||
	  (((black_on_white[vn] == 1) &&
	    ((data & mask) == mask)))) {
	FB_Image[vn][outpos] = FrameBuffer[outpos] = pixel_on;
      } else {
	FB_Image[vn][outpos] = FrameBuffer[outpos] = pixel_off;
      }
      outpos++;
      mask <<= 1;
    }
    accumulate_update(col, row, 8, 1);
  } else {
    while (mask < 0x100) {
      if (((black_on_white[vn] == 0) && ((data & mask) != mask)) ||
	  ((black_on_white[vn] == 1) && (data&mask) == mask)) {
        FB_Image[vn][outpos] = pixel_on;
      } else {
        FB_Image[vn][outpos] = pixel_off;
      }
      outpos++;
      mask <<= 1;
    }
  }
}

void init_sdl_keyboard() {
  // Read default keymap
  init_sdl_to_keysym_map();
}

void set_caption(char statbuf[]) {
  SDL_WM_SetCaption(statbuf, "LambdaDelta");
}
