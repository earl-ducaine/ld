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

#include "config.h"

// Unix things
#include <stdio.h>
#include <unistd.h>
#include <stdint.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <stdbool.h>
#include <signal.h>
#include <time.h>
#include <errno.h>

// TCP socket
#include <netinet/in.h>
#include <netdb.h> 

// SDL
#ifdef SDL1
#include <SDL.h>
#include <SDL_keysym.h>
#endif

#ifdef SDL2
#include <SDL.h>
#include <SDL_keycode.h>
#endif

// LambdaDelta things
#include "ld.h"
#include "lambda_cpu.h"
#include "nubus.h"
#include "mem.h"
#include "vcmem.h"
#include "sdu.h"
#include "sdu_hw.h"
#include "smd.h"
#include "3com.h"
#include "tapemaster.h"
#include "syms.h"

// Processor states
extern struct lambdaState pS[2];

// RTC time
uint32_t rtc_sec,rtc_min,rtc_hour;
uint32_t rtc_dow,rtc_date,rtc_month;
uint32_t rtc_year;

/* DISK GEOMETRY
   This will be read by the SDU.

   The "default" disk is a Fujitsu Eagle.
   M2351A is the standard drive, M2351A/F is the optional fixed-head version.
   The /F version has an extra area with fixed heads, presumably for swapping.

   28160 bytes per track unformatted
   (25 1024 byte sectors, 25600 bytes)

   20 tracks per cylinder
   842 cylinders per disk
   (3 cylinders fixed area on /F)   
*/
// int disk_geometry_sph = 25;  // Sectors per head
// int disk_geometry_spc = 500; // Sectors per cylinder

// Mouse state A-memory locations
uint32_t mouse_x_loc[2] = { 0156,0156 };
uint32_t mouse_y_loc[2] = { 0157,0157 };
uint32_t mouse_wake_loc[2] = { 0170,0170 };
// SDU rotary switch position
uint8_t sdu_rotary_switch = 0;

// Globals
int ld_die_rq;                  // Reason for emulator to die
int cp_state[2] = { 0,0 };      // 0 = cold, 1 = bootstrap, 2 = lisp, 3 = lisp + mouse initialized
int debug_log_enable = 0;
int debug_log_trigger = 0;
int dump_seq = 0;

// Whether to honour SDL_QUIT event (generated e.g. by Command-Q on a Mac)
int quit_on_sdl_quit = 1;

// Throttle timers
volatile uint64_t real_time = 0;
volatile uint64_t emu_time = 0;
volatile uint32_t stat_time = 20;

// Framebuffer size
#define VIDEO_HEIGHT 800
#define VIDEO_WIDTH 1024

// Pixels
uint32_t pixel_on = 0xFFFFFFFF;
uint32_t pixel_off = 0x00000000;

// FrameBuffer backup image
uint32_t FB_Image[2][VIDEO_WIDTH*VIDEO_HEIGHT];
// FrameBuffer Update accumulation
int u_minh = 0x7fffffff, u_maxh = 0, u_minv = 0x7fffffff, u_maxv = 0;
// Console switching
int active_console = 0;
// Reverse video mode (think <Terminal> C)
int black_on_white[2] = { 1,1 };               // 1 => white-on-black, 0 => black-on-white

#ifdef SDL1
// SDL1 state
SDL_Surface *screen;
SDL_TimerID SDLTimer;
int video_width = VIDEO_WIDTH;
int video_height = VIDEO_HEIGHT;
#endif

#ifdef SDL2
// SDL2 state
SDL_Window *SDLWindow;
SDL_Renderer *SDLRenderer;
SDL_Texture *SDLTexture;
SDL_TimerID SDLTimer;
uint32_t FrameBuffer[VIDEO_HEIGHT*VIDEO_WIDTH];
#endif

#ifdef BURR_BROWN
// Debug state and interface
uint8_t debug_target_mode = 0;
uint8_t debug_master_mode = 0;
char debug_target_host[128] = "localhost";
uint8_t debug_io_state = 0;
uint8_t debug_rx_buf[64];
uint32_t debug_last_addr = 0;
int debug_fd = -1;
int debug_conn_fd = -1;
#endif 

// SDU CONSOLE SOCKET STUFF
uint8_t sdu_rx_ptr = 0;
uint8_t sdu_rx_bot = 0;
uint8_t sdu_rx_buf[64];
uint32_t sdu_last_addr = 0;
int sdu_fd = -1;
int sdu_conn_fd = -1;
int sdu_tcmd_state = 0;

typedef struct DisplayState {
  unsigned char *data;
  int linesize;
  int depth;
  int width;
  int height;
} DisplayState;
DisplayState display_state;
DisplayState *ds = &display_state;

// SDL keymap and associated machinery
uint16_t map[512];
uint32_t modmap[512];
uint32_t kb_buckybits;
#define KB_BB_LSHIFT 0x00001
#define KB_BB_RSHIFT 0x00002
#define KB_BB_LCTL 0x00004
#define KB_BB_RCTL 0x00008
#define KB_BB_LMETA 0x00010
#define KB_BB_RMETA 0x00020
#define KB_BB_LSUPER 0x00040
#define KB_BB_RSUPER 0x00080
#define KB_BB_LHYPER 0x00100
#define KB_BB_RHYPER 0x00200
#define KB_BB_GREEK 0x00400
#define KB_BB_LTOP 0x00800
#define KB_BB_RTOP 0x01000
#define KB_BB_REPEAT 0x02000
#define KB_BB_CAPSLOCK 0x04000
#define KB_BB_ALTLOCK 0x08000
#define KB_BB_MODELOCK 0x10000

#ifdef SDL1
void init_sdl_to_keysym_map(void){
  int x = 0;
  while(x < 512){
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

  map['['] = 0132; // Unshifted (, [ is shiftstate
  map[']'] = 0137; // Unshifted ), ] is shiftstate
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
#endif /* SDL1 keymap */

#ifdef SDL2
void init_sdl_to_scancode_map(void){
  map[0x12] = 1;
  map[SDL_SCANCODE_1] = 0121; // 1
  map[SDL_SCANCODE_2] = 0061; // 2
  map[SDL_SCANCODE_3] = 0161; // 3
  map[SDL_SCANCODE_4] = 0011; // 4
  map[SDL_SCANCODE_5] = 0111; // 5
  map[SDL_SCANCODE_6] = 0051; // 6
  map[SDL_SCANCODE_7] = 0151; // 7
  map[SDL_SCANCODE_8] = 0031; // 8
  map[SDL_SCANCODE_9] = 0071; // 9
  map[SDL_SCANCODE_0] = 0171; // 0
  map[SDL_SCANCODE_MINUS] = 0131; // -
  //  map['+'] = 0x2f; // + is shifted =
  map[SDL_SCANCODE_EQUALS] = 0126;              /* not keypad-equal, real equals */

  map[SDL_SCANCODE_Q] = 0122; // Q
  map[SDL_SCANCODE_W] = 0062; // W
  map[SDL_SCANCODE_E] = 0162; // E
  map[SDL_SCANCODE_R] = 0012; // R
  map[SDL_SCANCODE_T] = 0112; // T
  map[SDL_SCANCODE_Y] = 0052; // Y
  map[SDL_SCANCODE_U] = 0152; // U
  map[SDL_SCANCODE_I] = 0032; // I
  map[SDL_SCANCODE_O] = 0072; // O
  map[SDL_SCANCODE_P] = 0172; // P

  map[SDL_SCANCODE_A] = 0123; // A
  map[SDL_SCANCODE_S] = 0063; // S
  map[SDL_SCANCODE_D] = 0163; // D
  map[SDL_SCANCODE_F] = 0013; // F
  map[SDL_SCANCODE_G] = 0113; // G
  map[SDL_SCANCODE_H] = 0053; // H
  map[SDL_SCANCODE_J] = 0153; // J
  map[SDL_SCANCODE_K] = 0033; // K
  map[SDL_SCANCODE_L] = 0073; // L

  map[SDL_SCANCODE_Z] = 0124; // Z
  map[SDL_SCANCODE_X] = 0064; // X
  map[SDL_SCANCODE_C] = 0164; // C
  map[SDL_SCANCODE_V] = 0014; // V
  map[SDL_SCANCODE_B] = 0114; // B
  map[SDL_SCANCODE_N] = 0054; // N
  map[SDL_SCANCODE_M] = 0154; // M

  map[SDL_SCANCODE_LEFTBRACKET] = 0132; // Unshifted (, [ is shiftstate
  map[SDL_SCANCODE_RIGHTBRACKET] = 0137; // Unshifted ), ] is shiftstate
  map[SDL_SCANCODE_SEMICOLON] = 0173;
  map[SDL_SCANCODE_GRAVE] = 0077;
  // map['~'] = 061;               /* special treatment (this is too useful to remap) */
  map[SDL_SCANCODE_BACKSLASH] = 0037;
  map[SDL_SCANCODE_SPACE] = 0134;

  map[SDL_SCANCODE_COMMA] = 0034;
  map[SDL_SCANCODE_PERIOD] = 0074;
  map[SDL_SCANCODE_SLASH] = 0174;
  map[SDL_SCANCODE_APOSTROPHE] = 0133;

  map[SDL_SCANCODE_RETURN] = 0136; // ENTER
  map[SDL_SCANCODE_BACKSPACE] = 0023; // BACKSPACE (MAPS TO RUBOUT)
  map[SDL_SCANCODE_TAB] = 0022; // TAB
  map[SDL_SCANCODE_ESCAPE] = 0023; // ESC

  // LMI arrows were shift states?
  // Or maybe it's the HAND keys?
  map[SDL_SCANCODE_UP] = 0106; // hand up
  map[SDL_SCANCODE_DOWN] = 0176; // hand down
  map[SDL_SCANCODE_RIGHT] = 0017; // hand right
  map[SDL_SCANCODE_LEFT] = 0117; // hand left

  // No home or insert, no F1 or F2
  map[SDL_SCANCODE_HOME] = 0x15; /* HOME - F1 */
  map[SDL_SCANCODE_INSERT] = 0x14; /* INSERT - F2 */

  // No page up or down
  map[SDL_SCANCODE_PAGEUP] = 0067; /* PAGEUP - ABORT */
  map[SDL_SCANCODE_PAGEDOWN] = 0047; /* PAGEDOWN - RESUME */

  map[SDL_SCANCODE_END] = 0156; /* END - END */

  map[SDL_SCANCODE_F1] = 0141; /* F1 - SYSTEM */
  map[SDL_SCANCODE_F2] = 0042; /* F2 - NETWORK */
  map[SDL_SCANCODE_F3] = 0046; /* F3 - STATUS */
  map[SDL_SCANCODE_F4] = 0040; /* F4 - TERMINAL */
  map[SDL_SCANCODE_F5] = 0116; /* F5 - HELP */
  map[SDL_SCANCODE_F6] = 0110; /* F6 - CLEAR */

  // SDL_SCANCODE_F9 = CONSOLE SWITCH
  // SDL_SCANCODE_F10 = MOUSE MODE SWITCH
  // SDL_SCANCODE_F11 = RETURN TO NEWBOOT
  // SDL_SCANCODE_F12 = SWITCH TAPE / DUMP

  map[SDL_SCANCODE_CAPSLOCK] = 0x03; // CAPSLOCK

  map[SDL_SCANCODE_RSHIFT] = 0025; // RIGHT SHIFT
  map[SDL_SCANCODE_LSHIFT] = 0024; // LEFT SHIFT

  map[SDL_SCANCODE_RCTRL] = 0044; // RIGHT CTRL is 0026, but we want LEFT GREEK which is 0044
  map[SDL_SCANCODE_LCTRL] = 0020; // LEFT CTRL
  map[SDL_SCANCODE_RALT] = 0005; // LEFT SUPER, was 0165 = RIGHT ALT (META)
  map[SDL_SCANCODE_LALT] = 0045; // LEFT ALT (META)

  map[SDL_SCANCODE_RGUI] = 0065; // RIGHT windows (SUPER)
  map[SDL_SCANCODE_LGUI] = 0005; // LEFT windows (SUPER)

  // Default modifier map
  modmap[SDL_SCANCODE_LSHIFT] = KB_BB_LSHIFT;
  modmap[SDL_SCANCODE_RSHIFT] = KB_BB_RSHIFT;
  modmap[SDL_SCANCODE_LCTRL] = KB_BB_LCTL;
  modmap[SDL_SCANCODE_RCTRL] = KB_BB_GREEK;
  modmap[SDL_SCANCODE_LALT] = KB_BB_LMETA;
  modmap[SDL_SCANCODE_RALT] = KB_BB_LSUPER;
  modmap[SDL_SCANCODE_APPLICATION] = KB_BB_RHYPER;
}
#endif /* SDL2 keymap */

// Utility functions
void FB_dump(int vn);
void write_nvram();
void write_rtc_nvram();

// SDL items
// Keyboard buffer
uint8_t keyboard_io_ring[2][0x100];
uint8_t keyboard_io_ring_top[2],keyboard_io_ring_bottom[2];
// Mouse
int mouse_op_mode = 1; // 0 = direct, 1 = shared
int mouse_update_inhibit = 0; // Inihibit the next SDL mouse event when Lisp warps the mouse
uint8_t mouse_io_ring[2][0x100];
uint8_t mouse_io_ring_top[2],mouse_io_ring_bottom[2];
uint8_t mouse_phase=0;
uint8_t mouse_capture=1; // Pointer capture state in mode 0, pointer hide/show state in mode 1
uint8_t mouse_last_buttons=0x07;

// Keyboard TX ring
int put_rx_ring(int vn,unsigned char ch){
  // printf("put_rx_ring: code %o @ %d\n",ch,keyboard_io_ring_top);
  keyboard_io_ring[vn][keyboard_io_ring_top[vn]] = ch;
  keyboard_io_ring_top[vn]++;
  // pS[0].microtrace = true;
  // pS[0].macrotrace = true;
  // NUbus_trace = 1;
  return(0);
}

// Mouse TX ring
int put_mouse_rx_ring(int vn,unsigned char ch){
  // printf("put_rx_ring: code %o @ %d\n",ch,keyboard_io_ring_top);
  mouse_io_ring[vn][mouse_io_ring_top[vn]] = ch;
  mouse_io_ring_top[vn]++;
  // pS[0].microtrace = true;
  // pS[0].macrotrace = true;
  // NUbus_trace = 1;
  return(0);
}

#ifdef SDL1
void kbd_handle_char(int symcode, int down){
  int sdlchar = symcode;
  unsigned char outchar=0;

  // Check for debug
  if(sdlchar == SDLK_F12){
    if(down){
      if(((kb_buckybits&KB_BB_LSHIFT)|(kb_buckybits&KB_BB_RSHIFT)) != 0){
	printf("DEBUG: DUMP REQUESTED FROM CONSOLE\n");
	lambda_dump(DUMP_ALL);
	FB_dump(0);
	FB_dump(1);
      }else{
	tapemaster_open_next();
      }
    }
    return;
  }

  // Check for return-to-newboot key
  if(sdlchar == SDLK_F11){
    // Keystroke is control-meta-control-meta-<LINE>
    // 0020 0045 0026 0165 0036
    if(down){
      // I don't know how this translates to that, but it works.
      // This is the sequence of bytes the ucode looks for.
      put_rx_ring(active_console,0x60);
      put_rx_ring(active_console,0x9F);
    }
    return;
  }

  // Check for decapture/pointer-hide-show key
  if(sdlchar == SDLK_F10){
    if(down){
      if(mouse_capture != 0){
	mouse_capture = 0;
	if(mouse_op_mode == 0){
	  SDL_WM_GrabInput(SDL_GRAB_OFF);
	}
	SDL_ShowCursor(SDL_ENABLE);
      }else{
	mouse_capture = 1;
	if(mouse_op_mode == 0){
	  SDL_WM_GrabInput(SDL_GRAB_ON);
	}
	SDL_ShowCursor(SDL_DISABLE);
      }
    }
    return;
  }

  // Check for console switch key
#ifdef CONFIG_2X2
  if(sdlchar == SDLK_F9){
    if(down){      
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
	for (j = 0; j < video_height; j++)
	  *p++ = *s++;
      }
      // Redraw it
      SDL_UpdateRect(screen, 0, 0, video_width, video_height);
      // Reset accumulation
      u_minh = 0x7fffffff; u_maxh = 0; u_minv = 0x7fffffff; u_maxv = 0;
      // If we are in shared mouse mode, move the pointer to where the new console thinks it should be
      if(mouse_op_mode == 1 && cp_state[active_console] == 3){
	warp_mouse_callback(active_console);
      }
    }
    return;
  }
#endif

  /* for now, fold lower case to upper case */
  /* (because we're ignoring modifiers) */
  if (sdlchar >= 'a' && sdlchar <= 'z'){
    sdlchar -= ' ';
  }
  
  // Obtain keymap entry
  outchar = map[sdlchar];

  // We send 3 characters. First is keycode, second is key state + bucky bits, third is source ID.
  put_rx_ring(active_console,outchar); // Keycode
  // Next is key up/down state and bucky bits
  outchar = 0x80; // This is the "second byte" flag  
  if(down){
    // Key Down
    outchar |= 0x40; // Key Down Flag
    if(modmap[sdlchar] != 0){
      kb_buckybits |= modmap[sdlchar];
    }
    // Take "down" bucky bits
    if(((kb_buckybits&KB_BB_LSHIFT)|(kb_buckybits&KB_BB_RSHIFT)) != 0){ outchar |= 0x20; }
    if(((kb_buckybits&KB_BB_LCTL)|(kb_buckybits&KB_BB_RCTL)) != 0){ outchar |= 0x10; }
    if(((kb_buckybits&KB_BB_LMETA)|(kb_buckybits&KB_BB_RMETA)) != 0){ outchar |= 0x08; }
    if(((kb_buckybits&KB_BB_LSUPER)|(kb_buckybits&KB_BB_RSUPER)) != 0){ outchar |= 0x04; }
    if(((kb_buckybits&KB_BB_LHYPER)|(kb_buckybits&KB_BB_RHYPER)) != 0){ outchar |= 0x02; }
    if((kb_buckybits&KB_BB_GREEK) != 0){ outchar |= 0x01; }
  }else{
    // Key Up
    if(modmap[sdlchar] != 0){
      kb_buckybits &= ~modmap[sdlchar];
    }
    // Take "up" bucky bits
    if((kb_buckybits&KB_BB_MODELOCK) != 0){ outchar |= 0x10; }
    if((kb_buckybits&KB_BB_ALTLOCK) != 0){ outchar |= 0x08; }
    if((kb_buckybits&KB_BB_CAPSLOCK) != 0){ outchar |= 0x04; }
    if((kb_buckybits&KB_BB_REPEAT) != 0){ outchar |= 0x02; }
    if(((kb_buckybits&KB_BB_LTOP)|(kb_buckybits&KB_BB_RTOP)) != 0){ outchar |= 0x01; }
  }
  // Send result
  put_rx_ring(active_console,outchar);
  // Third byte is Source ID.
  put_rx_ring(active_console,0x02); // (Newer Keyboard)

  // printf("KB: Key event sent\n");
  vcmem_kb_int(active_console);
}

void sdl_system_shutdown_request(void){
  exit(0);
}

void sdl_process_key(SDL_KeyboardEvent *ev, int updown){
  kbd_handle_char(ev->keysym.sym, updown);
}

void sdl_send_mouse_event(void){
  int state,xm,ym;
  uint8_t buttons=0x07;
  if(mouse_op_mode == 0){
    // Direct Mode
    state = SDL_GetRelativeMouseState(&xm, &ym);
    // Disregard mouse when not captured, unless we are recapturing it.
    if(mouse_capture == 0 && (state & SDL_BUTTON(SDL_BUTTON_LEFT))){
      mouse_capture = 2;
      return;
    }
    if(mouse_capture == 2 && !(state & SDL_BUTTON(SDL_BUTTON_LEFT))){
      mouse_capture = 1;
      SDL_WM_GrabInput(SDL_GRAB_ON);
      SDL_ShowCursor(SDL_DISABLE);
      return;
    }
    if(mouse_capture != 1){
      return;
    }
    if(cp_state[active_console] != 3){ return; }
    // Proceed
    if (state & SDL_BUTTON(SDL_BUTTON_LEFT)){ buttons ^= 0x04; }
    if (state & SDL_BUTTON(SDL_BUTTON_MIDDLE)){ buttons ^= 0x02; }
    if (state & SDL_BUTTON(SDL_BUTTON_RIGHT)){ buttons ^= 0x01; }
    
    if(mouse_phase == 1 && buttons != mouse_last_buttons){
      put_mouse_rx_ring(active_console,0);
      put_mouse_rx_ring(active_console,0);
      mouse_phase ^= 1;
    }
    // Construct packet
    ym = -ym; // Y movement is reversed
    // Scale movement
    xm /= 2;
    ym /= 2;
    if(xm == 0 && ym == 0 && buttons == mouse_last_buttons){ return; }
    // printf("MOUSE: Movement: %d/%d buttons 0x%.2x\n",xm,ym,buttons);
    // Construct mouse packet and send it
    if(mouse_phase == 0){    
      put_mouse_rx_ring(active_console,0x80|buttons); // Buttons
      put_mouse_rx_ring(active_console,xm&0xFF);
      put_mouse_rx_ring(active_console,ym&0xFF);
    }else{
      put_mouse_rx_ring(active_console,xm&0xFF);
      put_mouse_rx_ring(active_console,ym&0xFF);
    }
    mouse_phase ^= 1;
    mouse_last_buttons = buttons;
  }
  if(mouse_op_mode == 1){
    // Shared Mode
    // If lisp is not running, return
    if(cp_state[active_console] != 3){ return; }    
    state = SDL_GetMouseState(&xm, &ym);
    // If the inhibit counter is nonzero, throw away this update (it's fake)
    if(mouse_update_inhibit > 0){ mouse_update_inhibit--; return; }
    // Otherwise, proceed
    if (state & SDL_BUTTON(SDL_BUTTON_LEFT)){ buttons ^= 0x04; }
    if (state & SDL_BUTTON(SDL_BUTTON_MIDDLE)){ buttons ^= 0x02; }
    if (state & SDL_BUTTON(SDL_BUTTON_RIGHT)){ buttons ^= 0x01; }
    // printf("MOUSE: Movement: %d/%d buttons 0x%.2x\n",xm,ym,buttons);
    // Do we need to update buttons?
    if(buttons != mouse_last_buttons){
      // Yes - Generate a mouse packet (no movement, just buttons)
      if(mouse_phase == 1){
	put_mouse_rx_ring(active_console,0);
	put_mouse_rx_ring(active_console,0);
	mouse_phase ^= 1;
      }
      put_mouse_rx_ring(active_console,0x80|buttons); // Buttons
      put_mouse_rx_ring(active_console,0);
      put_mouse_rx_ring(active_console,0);
      mouse_phase ^= 1;      
      mouse_last_buttons = buttons;
    }else{
      // No, update position
      pS[active_console].Amemory[mouse_x_loc[active_console]] = 0xA000000|xm;
      pS[active_console].Amemory[mouse_y_loc[active_console]] = 0xA000000|ym;
      pS[active_console].Amemory[mouse_wake_loc[active_console]] = 0x6000005; // T
    }
  }
}

// Lisp updated the mouse position
void warp_mouse_callback(int cp){
  // Make sure we care first
  if(mouse_op_mode != 1 || cp_state[cp] != 3 || cp != active_console){ return; }
  mouse_update_inhibit++;
  // printf("WARP MOUSE 0x%X,0x%X\n",pS[cp].Amemory[mouse_x_loc[cp]],pS[cp].Amemory[mouse_y_loc[cp]]);
  SDL_WarpMouse((pS[cp].Amemory[mouse_x_loc[cp]]&0xFFFF),(pS[cp].Amemory[mouse_y_loc[cp]]&0xFFFF));
}

void set_bow_mode(int vn,int mode){
  int i,j;

  if(black_on_white[vn] == mode){
    // printf("BLACK-ON-WHITE MODE unchanged\n");
    return;                   /* noop */
  }
  printf("VC %d BLACK-ON-WHITE MODE now %d\n",vn,mode);
  black_on_white[vn] = mode;  /* update */

  // invert pixels
  uint32_t *p = FB_Image[vn];
  if(vn == active_console){
    uint32_t *b = screen->pixels;
    for (i = 0; i < video_width; i++) {
      for (j = 0; j < video_height; j++) {
	*b = *p = (*p == pixel_off ? pixel_on : pixel_off);
	p++; b++;
      }
    }
    // Refresh display
    SDL_UpdateRect(screen, 0, 0, video_width, video_height);
  }else{
    for (i = 0; i < video_width; i++) {
      for (j = 0; j < video_height; j++) {
	*p = (*p == pixel_off ? pixel_on : pixel_off);
	p++;
      }
    }
  }
}

void accumulate_update(int h, int v, int hs, int vs){
  if (h < u_minh) u_minh = h;
  if (h+hs > u_maxh) u_maxh = h+hs;
  if (v < u_minv) u_minv = v;
  if (v+vs > u_maxv) u_maxv = v+vs;
}

void send_accumulated_updates(void){
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

void sdl_refresh(void){
  SDL_Event ev1, *ev = &ev1;

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
      if(quit_on_sdl_quit != 0){
	sdl_system_shutdown_request();
      }
      break;
      
    default:
      break;
    }
  }
}

void sdl_cleanup(void){
  if(mouse_op_mode == 0){
    SDL_WM_GrabInput(SDL_GRAB_OFF);
  }
  SDL_ShowCursor(SDL_ENABLE);    
  if(sdu_conn_fd > 0){
    close(sdu_conn_fd);
  }
  if(sdu_fd > 0){
    close(sdu_fd);
  }
  write_nvram();
  write_rtc_nvram();
  SDL_Quit();
}

// Timer callback
uint32_t sdl_timer_callback(uint32_t interval, void *param __attribute__ ((unused))){
  // Real time passed
  real_time++;
  // Also increment status update counter
  stat_time++;
  // Return next interval
  return(interval);
}

int sdl_init(int width, int height){
  int i, j;

  video_width = width;
  video_height = height;

  if(SDL_Init(SDL_INIT_VIDEO|SDL_INIT_TIMER|SDL_INIT_NOPARACHUTE)){
    fprintf(stderr, "SDL initialization failed\n");
    exit(-1);
  }

  /* NOTE: we still want Ctrl-C to work - undo the SDL redirections*/
  signal(SIGINT, SIG_DFL);
  signal(SIGQUIT, SIG_DFL);

  // Obtain icon. It must be a 32x32 pixel 256-color BMP image. RGB 255,0,255 is used for transparency.
  SDL_Surface* icon = SDL_LoadBMP("icon.bmp");
  if(icon != NULL){
    SDL_SetColorKey(icon, SDL_SRCCOLORKEY, SDL_MapRGB(icon->format, 255, 0, 255));
    SDL_WM_SetIcon(icon, 0);
  }else{
    printf("Failed to open icon.bmp");
  }

  // SDL was burning a bunch of time forcing 8bpp, so use 32bpp instead.
  screen = SDL_SetVideoMode(width, height, 32, SDL_HWSURFACE|SDL_ASYNCBLIT|SDL_HWACCEL);

  if(!screen){
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
    for (j = 0; j < video_height; j++)
      *p++ = pixel_off;
  }
  p = FB_Image[1];
  for (i = 0; i < video_width; i++) {
    for (j = 0; j < video_height; j++)
      *p++ = pixel_off;
  }
  // Redraw it
  SDL_UpdateRect(screen, 0, 0, video_width, video_height);

  // Grab the mouse if we are in direct mode
  if(mouse_op_mode == 0){
    SDL_WM_GrabInput(SDL_GRAB_ON);
  }
  SDL_ShowCursor(SDL_DISABLE);

  atexit(sdl_cleanup);

  // Kick interval timer
  SDLTimer = SDL_AddTimer(100,sdl_timer_callback,NULL);
  if(SDLTimer == NULL){
    fprintf(stderr,"Unable to start interval timer\n");
    exit(-1);
  }

  return(0);
}

// Framebuffer management
void framebuffer_update_word(int vn,uint32_t addr,uint32_t data){
  // Given 1BPP data and a vcmem framebuffer address, translate to 32BPP and write to host
  uint32_t row,col;  // Row and column of guest write
  uint32_t outpos;   // Actual host FB offset
  uint64_t mask = 1; // Mask for pixel state
  uint32_t *FrameBuffer; // Address of framebuffer

  col = addr*8;      // This many pixels in
  row = (col/1024);  // Obtain row
  col -= (row*1024); // Remove row pixels
  FrameBuffer = (uint32_t *)screen->pixels;

  outpos = col+((screen->pitch/4)*row);

  if(outpos >= (uint32_t)(video_width*video_height)){
    return;
  }

  if(active_console == vn){
    while(mask < 0x100000000LL){
      if((black_on_white[vn] == 0 && (data&mask) != mask) || (black_on_white[vn] == 1 && (data&mask) == mask)){
	FB_Image[vn][outpos] = FrameBuffer[outpos] = pixel_on;
      }else{
	FB_Image[vn][outpos] = FrameBuffer[outpos] = pixel_off;
      }  
      outpos++;
      mask <<= 1;
    }
    accumulate_update(col, row, 32, 1);
  }else{
    while(mask < 0x100000000LL){
      if((black_on_white[vn] == 0 && (data&mask) != mask) || (black_on_white[vn] == 1 && (data&mask) == mask)){
	FB_Image[vn][outpos] = pixel_on;
      }else{
	FB_Image[vn][outpos] = pixel_off;
      }  
      outpos++;
      mask <<= 1;
    }
  }
}

void framebuffer_update_hword(int vn,uint32_t addr,uint16_t data){
  uint32_t row,col;  // Row and column of guest write
  uint32_t outpos;   // Actual host FB offset
  uint64_t mask = 1; // Mask for pixel state
  uint32_t *FrameBuffer; // Address of framebuffer

  col = addr*8;      // This many pixels in
  row = (col/1024);  // Obtain row
  col -= (row*1024); // Remove row pixels
  FrameBuffer = (uint32_t *)screen->pixels;

  outpos = col+((screen->pitch/4)*row);

  if(outpos >= (uint32_t)(video_width*video_height)){
    return;
  }

  if(active_console == vn){
    while(mask < 0x10000LL){
      if((black_on_white[vn] == 0 && (data&mask) != mask) || (black_on_white[vn] == 1 && (data&mask) == mask)){
	FB_Image[vn][outpos] = FrameBuffer[outpos] = pixel_on;
      }else{
	FB_Image[vn][outpos] = FrameBuffer[outpos] = pixel_off;
      }  
      outpos++;
      mask <<= 1;
    }
    accumulate_update(col, row, 16, 1);
  }else{
    while(mask < 0x10000LL){
      if((black_on_white[vn] == 0 && (data&mask) != mask) || (black_on_white[vn] == 1 && (data&mask) == mask)){
	FB_Image[vn][outpos] = pixel_on;
      }else{
	FB_Image[vn][outpos] = pixel_off;
      }  
      outpos++;
      mask <<= 1;
    }
  }
}

void framebuffer_update_byte(int vn,uint32_t addr,uint8_t data){
  // Given 1BPP data and a vcmem framebuffer address, translate to 32BPP and write to host
  uint32_t row,col;  // Row and column of guest write
  uint32_t outpos;   // Actual host FB offset
  uint32_t mask = 1; // Mask for pixel state
  uint32_t *FrameBuffer; // Address of framebuffer

  col = addr*8;      // This many pixels in
  row = (col/1024);  // Obtain row
  col -= (row*1024); // Remove row pixels
  FrameBuffer = (uint32_t *)screen->pixels;

  outpos = col+((screen->pitch/4)*row);

  if(outpos >= (uint32_t)(video_width*video_height)){
    return;
  }

  if(active_console == vn){
    while(mask < 0x100){
      if((black_on_white[vn] == 0 && (data&mask) != mask) || (black_on_white[vn] == 1 && (data&mask) == mask)){
	FB_Image[vn][outpos] = FrameBuffer[outpos] = pixel_on;
      }else{
	FB_Image[vn][outpos] = FrameBuffer[outpos] = pixel_off;
      }    
      outpos++;
      mask <<= 1;
    }
    accumulate_update(col, row, 8, 1);
  }else{
    while(mask < 0x100){
      if((black_on_white[vn] == 0 && (data&mask) != mask) || (black_on_white[vn] == 1 && (data&mask) == mask)){
        FB_Image[vn][outpos] = pixel_on;
      }else{
        FB_Image[vn][outpos] = pixel_off;
      }
      outpos++;
      mask <<= 1;
    }
  }
}

#endif /* SDL1 code */

#ifdef SDL2
void kbd_handle_char(int scancode, int down){
  int sdlchar = scancode;
  unsigned char outchar=0;

  // Check for debug
  if(sdlchar == SDL_SCANCODE_F12){
    if(down){
      if(((kb_buckybits&KB_BB_LSHIFT)|(kb_buckybits&KB_BB_RSHIFT)) != 0){
        printf("DEBUG: DUMP REQUESTED FROM CONSOLE\n");
        lambda_dump(DUMP_ALL);
        FB_dump(0);
        FB_dump(1);
      }else{
        tapemaster_open_next();
      }
    }
    return;
  }

  // Check for return-to-newboot key
  if(sdlchar == SDL_SCANCODE_F11){
    // Keystroke is control-meta-control-meta-<LINE>
    // 0020 0045 0026 0165 0036
    if(down){
      // I don't know how this translates to that, but it works.
      // This is the sequence of bytes the ucode looks for.
      put_rx_ring(active_console,0x60);
      put_rx_ring(active_console,0x9F);
    }
    return;
  }

  // Check for decapture/pointer-hide-show key
  if(sdlchar == SDL_SCANCODE_F10){
    if(down){
      if(mouse_capture != 0){
        mouse_capture = 0;
	if(mouse_op_mode == 0){
	  SDL_SetRelativeMouseMode(SDL_FALSE);
	}
	SDL_ShowCursor(SDL_ENABLE);
      }else{
        mouse_capture = 1;
	if(mouse_op_mode == 0){
	  SDL_SetRelativeMouseMode(SDL_TRUE);
	}
	SDL_ShowCursor(SDL_DISABLE);
      }
    }
  }

  // Check for console switch key
#ifdef CONFIG_2X2
  if(sdlchar == SDL_SCANCODE_F9){
    if(down){
      // Switch active console
      active_console ^= 1;
      printf("CONSW: %d\n",active_console);
      // Update window title
      stat_time = 20;
      // Refresh display bitmap from stored image
      uint32_t *p = FrameBuffer;
      uint32_t *s = FB_Image[active_console];
      int i,j;
      for (i = 0; i < VIDEO_WIDTH; i++) {
        for (j = 0; j < VIDEO_HEIGHT; j++)
          *p++ = *s++;
      }
      // Redraw it
      SDL_UpdateTexture(SDLTexture, NULL, FrameBuffer, (VIDEO_WIDTH*4));
      SDL_RenderClear(SDLRenderer);
      SDL_RenderCopy(SDLRenderer, SDLTexture, NULL, NULL);
      SDL_RenderPresent(SDLRenderer);

      // Reset accumulation
      u_minh = 0x7fffffff; u_maxh = 0; u_minv = 0x7fffffff; u_maxv = 0;

      // If we are in shared mouse mode, move the pointer to where the new console thinks it should be
      if(mouse_op_mode == 1 && cp_state[active_console] == 3){
        warp_mouse_callback(active_console);
      }
    }
    return;
  }
#endif

  /* for now, fold lower case to upper case */
  /* (because we're ignoring modifiers) */
  if (sdlchar >= 'a' && sdlchar <= 'z'){
    sdlchar -= ' ';
  }

  // Obtain keymap entry
  outchar = map[sdlchar];

  // We send 3 characters. First is keycode, second is key state + bucky bits, third is source ID.
  put_rx_ring(active_console,outchar); // Keycode
  // Next is key up/down state and bucky bits
  outchar = 0x80; // This is the "second byte" flag
  if(down){
    // Key Down
    outchar |= 0x40; // Key Down Flag
    if(modmap[sdlchar] != 0){
      kb_buckybits |= modmap[sdlchar];
    }
    // Take "down" bucky bits
    if(((kb_buckybits&KB_BB_LSHIFT)|(kb_buckybits&KB_BB_RSHIFT)) != 0){ outchar |= 0x20; }
    if(((kb_buckybits&KB_BB_LCTL)|(kb_buckybits&KB_BB_RCTL)) != 0){ outchar |= 0x10; }
    if(((kb_buckybits&KB_BB_LMETA)|(kb_buckybits&KB_BB_RMETA)) != 0){ outchar |= 0x08; }
    if(((kb_buckybits&KB_BB_LSUPER)|(kb_buckybits&KB_BB_RSUPER)) != 0){ outchar |= 0x04; }
    if(((kb_buckybits&KB_BB_LHYPER)|(kb_buckybits&KB_BB_RHYPER)) != 0){ outchar |= 0x02; }
    if((kb_buckybits&KB_BB_GREEK) != 0){ outchar |= 0x01; }
  }else{
    // Key Up
    if(modmap[sdlchar] != 0){
      kb_buckybits &= ~modmap[sdlchar];
    }
    // Take "up" bucky bits
    if((kb_buckybits&KB_BB_MODELOCK) != 0){ outchar |= 0x10; }
    if((kb_buckybits&KB_BB_ALTLOCK) != 0){ outchar |= 0x08; }
    if((kb_buckybits&KB_BB_CAPSLOCK) != 0){ outchar |= 0x04; }
    if((kb_buckybits&KB_BB_REPEAT) != 0){ outchar |= 0x02; }
    if(((kb_buckybits&KB_BB_LTOP)|(kb_buckybits&KB_BB_RTOP)) != 0){ outchar |= 0x01; }
  }
  // Send result
  put_rx_ring(active_console,outchar);
  // Third byte is Source ID.
  put_rx_ring(active_console,0x02); // (Newer Keyboard)
}

void sdl_system_shutdown_request(void){
  exit(0);
}

static void sdl_process_key(SDL_KeyboardEvent *ev, int updown){
  kbd_handle_char(ev->keysym.scancode, updown);
}

static void sdl_send_mouse_event(void){
  int state,xm,ym;
  uint8_t buttons=0x07;
  if(mouse_op_mode == 0){
    // Direct Mode
    state = SDL_GetRelativeMouseState(&xm, &ym);
    // Disregard mouse when not captured, unless we are recapturing it.
    if(mouse_capture == 0 && (state & SDL_BUTTON(SDL_BUTTON_LEFT))){
      mouse_capture = 2;
      return;
    }
    if(mouse_capture == 2 && !(state & SDL_BUTTON(SDL_BUTTON_LEFT))){
      mouse_capture = 1;
      SDL_SetRelativeMouseMode(SDL_TRUE);
      return;
    }
    if(mouse_capture != 1){
      return;
    }
    // if(!mouse_init){ return; }
    if(cp_state[active_console] != 3){ return; }
    // Proceed
    if (state & SDL_BUTTON(SDL_BUTTON_LEFT)){ buttons ^= 0x04; }
    if (state & SDL_BUTTON(SDL_BUTTON_MIDDLE)){ buttons ^= 0x02; }
    if (state & SDL_BUTTON(SDL_BUTTON_RIGHT)){ buttons ^= 0x01; }
    
    if(mouse_phase == 1 && buttons != mouse_last_buttons){
      put_mouse_rx_ring(active_console,0);
      put_mouse_rx_ring(active_console,0);
      mouse_phase ^= 1;
    }
    // Construct packet
    ym = -ym; // Y movement is reversed
    // Scale movement
    xm /= 2;
    ym /= 2;
    if(xm == 0 && ym == 0 && buttons == mouse_last_buttons){ return; }
    // printf("MOUSE: Movement: %d/%d buttons 0x%.2x\n",xm,ym,buttons);
    // Construct mouse packet and send it
    if(mouse_phase == 0){
      put_mouse_rx_ring(active_console,0x80|buttons); // Buttons
      put_mouse_rx_ring(active_console,xm&0xFF);
      put_mouse_rx_ring(active_console,ym&0xFF);
    }else{
      put_mouse_rx_ring(active_console,xm&0xFF);
      put_mouse_rx_ring(active_console,ym&0xFF);
    }
    mouse_phase ^= 1;
    mouse_last_buttons = buttons;
  }
  if(mouse_op_mode == 1){
    // Shared Mode
    // If lisp is not running, return
    if(cp_state[active_console] != 3){ return; }
    state = SDL_GetMouseState(&xm, &ym);
    // If the inhibit counter is nonzero, throw away this update (it's fake)
    if(mouse_update_inhibit > 0){ mouse_update_inhibit--; return; }
    // Otherwise, proceed
    if (state & SDL_BUTTON(SDL_BUTTON_LEFT)){ buttons ^= 0x04; }
    if (state & SDL_BUTTON(SDL_BUTTON_MIDDLE)){ buttons ^= 0x02; }
    if (state & SDL_BUTTON(SDL_BUTTON_RIGHT)){ buttons ^= 0x01; }
    // printf("MOUSE: Movement: %d/%d buttons 0x%.2x\n",xm,ym,buttons);
    // Do we need to update buttons?
    if(buttons != mouse_last_buttons){
      // Yes - Generate a mouse packet (no movement, just buttons)
      if(mouse_phase == 1){
        put_mouse_rx_ring(active_console,0);
        put_mouse_rx_ring(active_console,0);
        mouse_phase ^= 1;
      }
      put_mouse_rx_ring(active_console,0x80|buttons); // Buttons
      put_mouse_rx_ring(active_console,0);
      put_mouse_rx_ring(active_console,0);
      mouse_phase ^= 1;
      mouse_last_buttons = buttons;
    }else{
      // No, update position
      pS[active_console].Amemory[mouse_x_loc[active_console]] = 0xA000000|xm;
      pS[active_console].Amemory[mouse_y_loc[active_console]] = 0xA000000|ym;
      pS[active_console].Amemory[mouse_wake_loc[active_console]] = 0x6000005; // T
    }
  }
}

// Lisp updated the mouse position
void warp_mouse_callback(int cp){
  // Make sure we care first
  if(mouse_op_mode != 1 || cp_state[cp] != 3 || cp != active_console){ return; }
  // Are we the active window?
  if((SDL_GetWindowFlags(SDLWindow)&SDL_WINDOW_MOUSE_FOCUS) == 0){ return; }
  // Otherwise proceed
  mouse_update_inhibit++;
  // printf("WARP MOUSE 0x%X,0x%X\n",pS[cp].Amemory[mouse_x_loc[cp]],pS[cp].Amemory[mouse_y_loc[cp]]);
  SDL_WarpMouseInWindow(SDLWindow,(pS[cp].Amemory[mouse_x_loc[cp]]&0xFFFF),(pS[cp].Amemory[mouse_y_loc[cp]]&0xFFFF));
}

void set_bow_mode(int vn,int mode){
  int i,j;

  if (black_on_white[vn] == mode) {
    // printf("BLACK-ON-WHITE MODE unchanged\n");
    return;                   /* noop */
  }
  printf("VC %d BLACK-ON-WHITE MODE now %d\n",vn,mode);
  black_on_white[vn] = mode;  /* update */
  
  // invert pixels
  uint32_t *p = FB_Image[active_console];
  if(vn == active_console){
    uint32_t *b = FrameBuffer;
    for (i = 0; i < VIDEO_WIDTH; i++) {
      for (j = 0; j < VIDEO_HEIGHT; j++) {
	*b = *p = (*p == pixel_off ? pixel_on : pixel_off);
	p++; b++;
      }
    }
    // Refresh display
    SDL_UpdateTexture(SDLTexture, NULL, FrameBuffer, (VIDEO_WIDTH*4));
    SDL_RenderClear(SDLRenderer);
    SDL_RenderCopy(SDLRenderer, SDLTexture, NULL, NULL);
    SDL_RenderPresent(SDLRenderer);
  }else{
    for (i = 0; i < VIDEO_WIDTH; i++) {
      for (j = 0; j < VIDEO_HEIGHT; j++) {
	*p = (*p == pixel_off ? pixel_on : pixel_off);
	p++;
      }
    }
  }  
}

void accumulate_update(int h, int v, int hs, int vs){
  if (h < u_minh) u_minh = h;
  if (h+hs > u_maxh) u_maxh = h+hs;
  if (v < u_minv) u_minv = v;
  if (v+vs > u_maxv) u_maxv = v+vs;
}

void sdl_refresh(void){
  SDL_Event ev1, *ev = &ev1;

  // send_accumulated_updates();

  // Refresh display
  SDL_UpdateTexture(SDLTexture, NULL, FrameBuffer, (VIDEO_WIDTH*4));
  SDL_RenderClear(SDLRenderer);
  SDL_RenderCopy(SDLRenderer, SDLTexture, NULL, NULL);
  SDL_RenderPresent(SDLRenderer);

  // Handle input
  while (SDL_PollEvent(ev)) {
    switch (ev->type) {
    case SDL_WINDOWEVENT_EXPOSED:
      // SDL_UpdateRect(screen, 0, 0, screen->w, screen->h);
      // Instead of this cause the entire screen to update regardless of the accumulated coordinates.
      break;

    case SDL_KEYDOWN:
      sdl_process_key(&ev->key, 1);
      break;
    case SDL_KEYUP:
      sdl_process_key(&ev->key, 0);
      break;

    case SDL_QUIT:
      if(quit_on_sdl_quit != 0){
	sdl_system_shutdown_request();
      }
      break;
    case SDL_MOUSEMOTION:
    case SDL_MOUSEBUTTONDOWN:
    case SDL_MOUSEBUTTONUP:
    case SDL_WINDOWEVENT_ENTER:
    case SDL_WINDOWEVENT_LEAVE:
      sdl_send_mouse_event();
      break;

    default:
      break;
    }
  }
}

static void sdl_cleanup(void){
  if(mouse_op_mode == 0){
    SDL_SetRelativeMouseMode(SDL_FALSE);
  }
  SDL_ShowCursor(SDL_ENABLE);
  if(sdu_conn_fd > 0){
    close(sdu_conn_fd);
  }
  if(sdu_fd > 0){
    close(sdu_fd);
  }
  write_nvram();
  write_rtc_nvram();
  SDL_Quit();
}

// Timer callback
uint32_t sdl_timer_callback(uint32_t interval, void *param __attribute__ ((unused))){
  // Real time passed
  real_time++;
  // Also increment status update counter
  stat_time++;
  // Return next interval
  return(interval);
}

// New timer callback because SDL2's interval timer sucks
static void itimer_callback(int signum __attribute__ ((unused))){
  // Real time passed
  real_time++;
  // Also increment status update counter
  stat_time++;
}

int sdl_init(int width, int height){
  int flags;
  int i,j;
  struct sigaction sigact;

  flags = SDL_INIT_VIDEO;

  if (SDL_Init(flags)) {
    fprintf(stderr, "SDL initialization failed\n");
    exit(1);
  }

  /* NOTE: we still want Ctrl-C to work - undo the SDL redirections */
  signal(SIGINT, SIG_DFL);
  signal(SIGQUIT, SIG_DFL);

  // Capture SIGALRM for our callback
  sigact.sa_handler = itimer_callback;
  sigemptyset(&sigact.sa_mask);
  sigact.sa_flags = SA_RESTART; // Attempt to restart syscalls interrupted by this signal
  sigaction(SIGALRM,&sigact,NULL);

  // Create window
  SDLWindow = SDL_CreateWindow("LambdaDelta",
                               SDL_WINDOWPOS_CENTERED,
                               SDL_WINDOWPOS_CENTERED,
                               width, height,
                               0);
  if(SDLWindow == NULL){
    printf("SDL_CreateWindow(): %s\n",SDL_GetError());
    return(-1);
  }
  // And renderer
  SDLRenderer = SDL_CreateRenderer(SDLWindow, -1, 0);
  if(SDLRenderer == NULL){
    printf("SDL_CreateRenderer(): %s\n",SDL_GetError());
    return(-1);
  }

  // Obtain icon. It must be a 32x32 pixel 256-color BMP image. RGB 255,0,255 is used for transparency.
  SDL_Surface* icon = SDL_LoadBMP("icon.bmp");
  if(icon != NULL){
    SDL_SetColorKey(icon, SDL_TRUE, SDL_MapRGB(icon->format, 255, 0, 255));
    SDL_SetWindowIcon(SDLWindow, icon);
    SDL_FreeSurface(icon);
  }else{
    printf("Failed to open icon.bmp");
  }

  // Do some setting
  SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "linear");  // make the scaled rendering look smoother.
  SDL_RenderSetLogicalSize(SDLRenderer, width, height);

  printf("XXX wid %d high %d\n", width, height);

  // And texture
  SDLTexture = SDL_CreateTexture(SDLRenderer,
                                 SDL_PIXELFORMAT_ARGB8888,
                                 SDL_TEXTUREACCESS_STREAMING,
                                 width, height);
  if(SDLTexture == NULL){
    printf("SDL_CreateTexture(): %s\n",SDL_GetError());
    return(-1);
  }

  // Clean up if we die
  atexit(sdl_cleanup);

  // Clear stored bitmaps
  uint32_t *p = FB_Image[0];
  for (i = 0; i < VIDEO_WIDTH; i++) {
    for (j = 0; j < VIDEO_HEIGHT; j++)
      *p++ = pixel_off;
  }
  p = FB_Image[1];
  for (i = 0; i < VIDEO_WIDTH; i++) {
    for (j = 0; j < VIDEO_HEIGHT; j++)
      *p++ = pixel_off;
  }

  // Grab the mouse if we are in direct mode
  if(mouse_op_mode == 0){
    SDL_SetRelativeMouseMode(SDL_TRUE);
  }
  SDL_ShowCursor(SDL_DISABLE);

  // Kick interval timer
  /*
  SDLTimer = SDL_AddTimer(100,sdl_timer_callback,NULL);
  if(SDLTimer == 0){
    fprintf(stderr,"Unable to start interval timer\n");
    exit(-1);
  }
  */
  struct itimerval itv;
  bzero((uint8_t *)&itv,sizeof(struct itimerval));
  itv.it_interval.tv_usec = 100000;
  itv.it_value.tv_usec = 100000;
  setitimer(ITIMER_REAL,&itv,NULL);

  // Done
  return 0;
}

// Framebuffer management
void framebuffer_update_word(int vn,uint32_t addr,uint32_t data){
  // Given 1BPP data and a vcmem framebuffer address, translate to 32BPP and write to host
  uint32_t row,col;  // Row and column of guest write
  uint32_t outpos;   // Actual host FB offset
  uint64_t mask = 1; // Mask for pixel state

  col = addr*8;      // This many pixels in
  row = (col/1024);  // Obtain row
  col -= (row*1024); // Remove row pixels

  outpos = col+(VIDEO_WIDTH*row);

  if(outpos >= (uint32_t)(VIDEO_WIDTH*VIDEO_HEIGHT)){
    return;
  }

  if(active_console == vn){
    while(mask < 0x100000000LL){
      if((black_on_white[vn] == 0 && (data&mask) != mask) || (black_on_white[vn] == 1 && (data&mask) == mask)){
	FB_Image[vn][outpos] = FrameBuffer[outpos] = pixel_on;
      }else{
	FB_Image[vn][outpos] = FrameBuffer[outpos] = pixel_off;
      }
      outpos++;
      mask <<= 1;
    }
    accumulate_update(col, row, 32, 1);
  }else{
    while(mask < 0x100000000LL){
      if((black_on_white[vn] == 0 && (data&mask) != mask) || (black_on_white[vn] == 1 && (data&mask) == mask)){
        FB_Image[vn][outpos] = pixel_on;
      }else{
        FB_Image[vn][outpos] = pixel_off;
      }
      outpos++;
      mask <<= 1;
    }
  }
}

void framebuffer_update_hword(int vn,uint32_t addr,uint16_t data){
  uint32_t row,col;  // Row and column of guest write
  uint32_t outpos;   // Actual host FB offset
  uint64_t mask = 1; // Mask for pixel state

  col = addr*8;      // This many pixels in
  row = (col/1024);  // Obtain row
  col -= (row*1024); // Remove row pixels

  outpos = col+(VIDEO_WIDTH*row);

  if(outpos >= (uint32_t)(VIDEO_WIDTH*VIDEO_HEIGHT)){
    return;
  }

  if(active_console == vn){
    while(mask < 0x10000){
      if((black_on_white[vn] == 0 && (data&mask) != mask) || (black_on_white[vn] == 1 && (data&mask) == mask)){
	FB_Image[vn][outpos] = FrameBuffer[outpos] = pixel_on;
      }else{
	FB_Image[vn][outpos] = FrameBuffer[outpos] = pixel_off;
      }  
      outpos++;
      mask <<= 1;
    }
    accumulate_update(col, row, 16, 1);
  }else{
    while(mask < 0x10000){
      if((black_on_white[vn] == 0 && (data&mask) != mask) || (black_on_white[vn] == 1 && (data&mask) == mask)){
	FB_Image[vn][outpos] = pixel_on;
      }else{
	FB_Image[vn][outpos] = pixel_off;
      }  
      outpos++;
      mask <<= 1;
    }
  }
}

void framebuffer_update_byte(int vn,uint32_t addr,uint8_t data){
  // Given 1BPP data and a vcmem framebuffer address, translate to 32BPP and write to host
  uint32_t row,col;  // Row and column of guest write
  uint32_t outpos;   // Actual host FB offset
  uint32_t mask = 1; // Mask for pixel state

  col = addr*8;      // This many pixels in
  row = (col/1024);  // Obtain row
  col -= (row*1024); // Remove row pixels

  outpos = col+(VIDEO_WIDTH*row);

  if(outpos >= (uint32_t)(VIDEO_WIDTH*VIDEO_HEIGHT)){
    return;
  }

  if(active_console == vn){
    while(mask < 0x100){
      if((black_on_white[vn] == 0 && (data&mask) != mask) || (black_on_white[vn] == 1 && (data&mask) == mask)){
	FB_Image[vn][outpos] = FrameBuffer[outpos] = pixel_on;
      }else{
	FB_Image[vn][outpos] = FrameBuffer[outpos] = pixel_off;
      }
      outpos++;
      mask <<= 1;
    }
    accumulate_update(col, row, 8, 1);
  }else{
    while(mask < 0x100){
      if((black_on_white[vn] == 0 && (data&mask) != mask) || (black_on_white[vn] == 1 && (data&mask) == mask)){
        FB_Image[vn][outpos] = pixel_on;
      }else{
        FB_Image[vn][outpos] = pixel_off;
      }
      outpos++;
      mask <<= 1;
    }
  }
}

#endif /* SDL2 code */

void read_sdu_rom(){
  extern uint8_t SDU_ROM[];
  int rom_fd = open("roms/SDU.ROM",O_RDONLY);
  if(rom_fd < 0){
    perror("SDU:open");
    exit(-1);
  }else{
    ssize_t rv=0;
    rv = read(rom_fd,SDU_ROM,64*1024);
    if(rv != (64*1024)){
      perror("SDU:read");
      exit(-1);
    }
    close(rom_fd);
  }
}

void read_vcmem_rom(){
  extern uint8_t VCMEM_ROM[];
  int rom_fd = open("roms/VCMEM.ROM",O_RDONLY);
  if(rom_fd < 0){
    perror("VCMEM:open");
    exit(-1);
  }else{
    ssize_t rv=0;
    rv = read(rom_fd,VCMEM_ROM,2048);
    if(rv != 2048){
      perror("VCMEM:read");
      exit(-1);
    }
    close(rom_fd);
  }
}

void read_mem_rom(){
  extern uint8_t MEM_ROM[];
  int rom_fd = open("roms/MEM.ROM",O_RDONLY);
  if(rom_fd < 0){
    perror("MEM:open");
    exit(-1);
  }else{
    ssize_t rv=0;
    rv = read(rom_fd,MEM_ROM,2048);
    if(rv != 2048){
      perror("MEM:read");
      exit(-1);
    }
    close(rom_fd);
  }
}

void read_nvram(){
  extern uint8_t CMOS_RAM[];
  int cmos_fd = open("CMOS.RAM",O_RDONLY);
  if(cmos_fd < 0){
    perror("CMOS:open");
  }else{
    ssize_t rv=0;
    rv = read(cmos_fd,CMOS_RAM,2048);
    if(rv != 2048){
      perror("CMOS:read");
    }
    close(cmos_fd);
  }
}

void write_nvram(){
  extern uint8_t CMOS_RAM[];
  int cmos_fd = open("CMOS.RAM",O_RDWR|O_CREAT,0660);
  if(cmos_fd < 0){
    perror("CMOS:open");
  }else{
    ssize_t rv=0;
    rv = write(cmos_fd,CMOS_RAM,2048);
    if(rv != 2048){
      perror("CMOS:write");
    }
    close(cmos_fd);
  }
}

void read_rtc_nvram(){
  extern uint8_t RTC_RAM[];
  int rtc_fd = open("RTC.RAM",O_RDONLY);
  if(rtc_fd < 0){
    perror("RTC:open");
    // Initialize contents
    RTC_RAM[0] = 0x2C; // EST TZ Low
    RTC_RAM[1] = 0x01; // EST TZ Hi
    RTC_RAM[2] = 'C'; // Cookie
    RTC_RAM[3] = '\'';
    RTC_RAM[4] = 'e';
    RTC_RAM[5] = 's';
    RTC_RAM[6] = 't';
    RTC_RAM[7] = ' ';
    RTC_RAM[8] = 'v';
    RTC_RAM[9] = 'r';
    RTC_RAM[10] = 'a';
    RTC_RAM[11] = 'i';
    RTC_RAM[12] = '.';
    RTC_RAM[13] = 0;
  }else{
    ssize_t rv=0;
    rv = read(rtc_fd,RTC_RAM,50);
    if(rv != 50){
      perror("RTC:read");
    }
    close(rtc_fd);
  }
}

void write_rtc_nvram(){
  extern uint8_t RTC_RAM[];
  int rtc_fd = open("RTC.RAM",O_RDWR|O_CREAT,0660);
  if(rtc_fd < 0){
    perror("RTC:open");
  }else{
    ssize_t rv=0;
    rv = write(rtc_fd,RTC_RAM,50);
    if(rv != 50){
      perror("RTC:write");
    }
    close(rtc_fd);
  }
}

const char *dtp_str[040] = {
  "DTP_TRAP",
  "DTP_NULL",
  "DTP_UNRECONCILED",     // was DTP-FREE
  "DTP_SYMBOL",
  "DTP_SYMBOL_HEADER",
  "DTP_FIX",
  "DTP_EXTENDED_NUMBER",
  "DTP_HEADER",
  "DTP_GC_FORWARD",
  "DTP_EXTERNAL_VALUE_CELL_POINTER",
  "DTP_ONE_Q_FORWARD",
  "DTP_HEADER_FORWARD",
  "DTP_BODY_FORWARD",
  "DTP_LOCATIVE",
  "DTP_LIST",
  "DTP_U_ENTRY",
  "DTP_FEF_POINTER",
  "DTP_ARRAY_POINTER",
  "DTP_ARRAY_HEADER",
  "DTP_STACK_GROUP",
  "DTP_CLOSURE",
  "DTP_INDEXED_FORWARD", // was DTP-SMALL-FLONUM
  "DTP_SELECT_METHOD",
  "DTP_INSTANCE",
  "DTP_INSTANCE_HEADER",
  "DTP_ENTITY",
  "DTP_UNUSED_32",   // was DTP-STACK-CLOSURE
  "DTP_SELF_REF_POINTER",
  "DTP_CHARACTER",
  "DTP_RPLACD_FORWARD",
  "DTP_SPARE",
  "DTP_SMALL_FLONUM" // 37
};

// Dump
void FB_dump(int vn){
  FILE *output;
  if(vn == 0){
    output = fopen("VC0-SCREENSHOT.BMP","w+");
  }else{
    output = fopen("VC1-SCREENSHOT.BMP","w+");
  }
  if(!output){
    printf("Can't open SCREENSHOT.BMP\n");
    return;
  }
  {
    // BMP header
    const char bmpheader[14] = {
      0x42,0x4D,  // Constant 19778, 'BM'
      0x3E,0,2,0, // FILE SIZE - 62+131072 (131134)
      0,0,        // Must be zero
      0,0,        // Must be zero
      62,0,0,0  // Offset to start of data (Constant 0x436)
    };
    const char bmpinfohdr[40] = {
      0x28,0,0,0, // Constant 40, size of info header
      0,4,0,0,    // Width (1024 px)
      0,4,0,0,    // Height (1024 px)
      1,0,        // Number of bitplanes (mono)
      1,0,        // Bits per pixel (mono)
      0,0,0,0,    // Compression (none)
      0,0,0,0,    // Size of image data (0 for not compressed)
      0,0,0,0,    // pels per meter etc
      0,0,0,0,
      0,0,0,0,    // Colors used (mono)
      0,0,0,0     // Important colors (mono)
    };
    // Now at byte 54
    const char rgbinfo[8] = {
      0,0,0,0,       // Black
      255,255,255,0  // White
    };
    // Now at byte 62
    // The VRAM
    extern struct vcmemState vcS[2];

    int x=1024,y=128;
    // Write out header and such
    fwrite(bmpheader,14,1,output);
    fwrite(bmpinfohdr,40,1,output);
    fwrite(rgbinfo,8,1,output);

    // Write pixels
    while(x > 0){
      x--;
      y=0; // 128 bytes in a row
      while(y < 128){
        unsigned char dto;
        dto=vcS[vn].AMemory[(x*128)+y];
        // Reverse bits
        dto = ((dto >>  1) & 0x55) | ((dto <<  1) & 0xaa);
        dto = ((dto >>  2) & 0x33) | ((dto <<  2) & 0xcc);
        dto = ((dto >>  4) & 0x0f) | ((dto <<  4) & 0xf0);
        fwrite(&dto,1,1,output);
        y++;
      }
    }
  }
  fclose(output);
  printf("Dump completed.\r\n");
}


// Hardware initialization
void hw_init(){
  // RTC initialization
  // The Lambda expects the RTC to read in local time.
  time_t rt_sec = time(NULL);
  struct tm *real_tm = localtime(&rt_sec);

  // The RTC on the SDU is a Motorola MC146818.
  // The year is supposed to be 0-99, but lisp will use any 8 bits of data in there.
  // Day of week, date of month, and month are based at 1.
  // If DST is in effect, Lambda expects the clock to be offset one hour.
  if(real_tm != NULL && real_tm->tm_isdst > 0){
    rt_sec -= (60*60); // Back one hour
    real_tm = localtime(&rt_sec);
  }
  if(real_tm != NULL){
    rtc_sec = real_tm->tm_sec;
    rtc_min = real_tm->tm_min;
    rtc_hour = real_tm->tm_hour;
    rtc_dow = (real_tm->tm_wday+1);
    rtc_date = real_tm->tm_mday;
    rtc_month = (real_tm->tm_mon+1);
    rtc_year = real_tm->tm_year;
  }else{
    // Default safe date.
    // Not sure what the correct time was; 7 AM seems like a reasonable before-school time slot.
    rtc_sec = 0;
    rtc_min = 0;
    rtc_hour = 7;
    rtc_dow = 7;
    rtc_date = 7;
    rtc_month = 3;
    rtc_year = 92;
  }
  
  // Reset processor and all peripherals
  printf("Initializing...\r\n");
  lambda_initialize(0,0xF0);
  sdu_init();
  smd_reset();
  enet_reset();
  mem_init();
  vcmem_init(0,0xF8);
#ifdef CONFIG_2X2
  lambda_initialize(1,0xF4);
  vcmem_init(1,0xFC);
#endif
}

void lambda_dump(int opts){
  FILE *output;
  char ofn[32];
  uint32_t addr=0;
  int x=0;

  if(opts&DUMP_A_MEM){
    addr = 0;
    sprintf(ofn,"AMEM-%.2d.DUMP",dump_seq);
    printf("Dumping A-Memory to %s...\n",ofn);
    output = fopen(ofn,"w+");
    if(!output){
      printf("Can't open %s\n",ofn);
      return;
    }
    while(addr<(1024*4)){
      fprintf(output,"[A-%.5o] 0x%.8X\n",addr,pS[0].Amemory[addr]);
      addr++;
    }
    fclose(output);
  }
  if(opts&DUMP_M_MEM){
    addr = 0;
    sprintf(ofn,"MMEM-%.2d.DUMP",dump_seq);
    printf("Dumping M-Memory to %s...\n",ofn);
    output = fopen(ofn,"w+");
    if(!output){
      printf("Can't open %s\n",ofn);
      return;
    }
    fprintf(output,"PDL Index = 0%.11o, PDL Pointer = %.11o, PDL_Addr_Hi = %o\n",pS[0].pdl_index_reg,pS[0].pdl_ptr_reg,pS[0].DP_Mode.PDL_Addr_Hi);
    while(addr<(1024*4)){
      fprintf(output,"[M-%.5o] 0x%.8X\n",addr,pS[0].Mmemory[addr]);
      addr++;
    }
    fclose(output);
  }
  if(opts&DUMP_T_MEM){
    addr = 0;
    sprintf(ofn,"TRAM-%.2d.DUMP",dump_seq);
    printf("Dumping TRAM to %s...\n",ofn);
    output = fopen(ofn,"w+");
    if(!output){
      printf("Can't open %s\n",ofn);
      return;
    }
    while(addr < 0x0FFF){
      fprintf(output,"[T-%.4o] = 0x%.8X: next.select %o state %.3o source_cycle %o new_uinst %o\n",
	      addr,
	      pS[0].TRAM[addr].word,
	      pS[0].TRAM[addr].next_select,
	      pS[0].TRAM[addr].state,
	      pS[0].TRAM[addr].source_cycle,
	      pS[0].TRAM[addr].new_uinst);
      // fprintf(output,"  pS[I].TRAM[0%.4o].word = 0x%.8X;\n",addr,pS[0].TRAM[addr].word);
      addr++;
    }
    fclose(output);
  }
  if(opts&DUMP_U_STACK){
    printf("Micro-stack-pointer 0%o\n",pS[0].uPCS_ptr_reg);
    x = pS[0].uPCS_ptr_reg;
    while(x >= 0){
      int offset = 0;
      char *location = sym_find_last(1, pS[0].uPCS_stack[x]&0xFFFFF, &offset);
      char symloc[100];
      if(location != 0){
	if(offset != 0){
	  sprintf(symloc, "%s+%o", location, offset);
	}else{
	  sprintf(symloc, "%s", location);
	}
      }else{
	symloc[0] = 0;
      }
      printf("uStack[%o] = %s (%o)\n",x,symloc,pS[0].uPCS_stack[x]&0xFFFFF);
      x--;
    }
  }
  if(opts&DUMP_PDL){
    printf("PDL Index = 0%.11o, PDL Pointer = %.11o, PDL_Addr_Hi = %o\n",pS[0].pdl_index_reg,pS[0].pdl_ptr_reg,pS[0].DP_Mode.PDL_Addr_Hi);
    if(pS[0].DP_Mode.PDL_Addr_Hi != 0){ addr = 04000; }else{ addr = 0; }
    x = addr+pS[0].pdl_ptr_reg;
    while(x >= (int)addr){
      Q tmp;
      tmp.raw = pS[0].Mmemory[x];
      printf("PDL[%o] = CDR %o DTP %.2o (%s) PTR 0x%.7x/0%.9o\n",
	     (x-addr),tmp.cdr,tmp.dtp,dtp_str[tmp.dtp],tmp.ptr,tmp.ptr);
      x--;
    }
  }
  if(opts&DUMP_P_MEM){
    addr = 0;
    sprintf(ofn,"PMEM-%.2d.DUMP",dump_seq);
    printf("Dumping Physical Memory to %s...\n",ofn);
    output = fopen(ofn,"w+");
    if(!output){
      printf("Can't open %s\n",ofn);
      return;
    }
    // while(addr<0x1000000){ // 16MB
    while(addr<0x0800000){ // 8MB
      uint32_t data = debug_mem_read(addr+3); data <<= 8;
      data |= debug_mem_read(addr+2); data <<= 8;
      data |= debug_mem_read(addr+1); data <<= 8;
      data |= debug_mem_read(addr);
      fprintf(output,"[0x%.6x] 0x%.8X\n",addr,data);
      addr += 4;
    }
    fclose(output);
  }
  if(opts&DUMP_MID_MEM){
    addr = 0;
    sprintf(ofn,"MIDMEM-%.2d.DUMP",dump_seq);
    printf("Dumping MID Memory to %s...\n",ofn);
    output = fopen(ofn,"w+");
    if(!output){
      printf("Can't open %s\n",ofn);
      return;
    }
    while(addr<(1024*4)){
      DispatchWord disp_word;
      disp_word.raw = pS[0].MIDmemory[addr];
      fprintf(output,"[MID-%.5o] 0x%.8X",addr,disp_word.raw);
      if(disp_word.raw != 0){
	char *location;
	char symloc[100];
	int offset;
	location = "";
	offset = 0;
	location = sym_find_last(1, disp_word.PC, &offset);
	if(offset != 0){
	  sprintf(symloc, "%s+%o", location, offset);
	}else{
	  sprintf(symloc, "%s", location);
	}
      fprintf(output," (Op %o S-M-R %o Dest %s (%o))",disp_word.Operation,disp_word.StartRead,symloc,disp_word.PC);
      }
      fprintf(output,"\n");
      addr++;
    }
    fclose(output);
  }
#ifdef SHADOW
  if(opts&DUMP_SHADOW_MEM){
    addr = 0;
    sprintf(ofn,"VMEM-%.2d.DUMP",dump_seq);
    printf("Dumping Shadow Memory to %s...\n",ofn);
    output = fopen(ofn,"w+");
    if(!output){
      printf("Can't open %s\n",ofn);
      return;
    }
    while(addr<0x20000){
      extern ShadowMemoryPageEnt ShadowMemoryPageMap[0x20000];
      extern Q ShadowMemory[0x2000000];
      Q Page;
      lv2_idx lv2_index;
      lv2_index.raw = 0;
      // Begin
      Page.raw = addr<<8;
      fprintf(output,"PAGE 0x%.7x (0%.9o): Resident %d Written %d Paged %d\n",Page.raw,Page.raw,
	      ShadowMemoryPageMap[addr].Resident,ShadowMemoryPageMap[addr].Written,ShadowMemoryPageMap[addr].Paged);
      // Print LV1 data
      fprintf(output,"  LV1: Meta %o Validity %o L2Block %.3o\n",
	      pS[0].vm_lv1_map[Page.VM.VPage_Block].MB,pS[0].vm_lv1_map[Page.VM.VPage_Block].MB_Valid,pS[0].vm_lv1_map[Page.VM.VPage_Block].LV2_Block);
      // Obtain LV2 index
      lv2_index.VPage_Offset = Page.VM.VPage_Offset;
      lv2_index.LV2_Block = pS[0].vm_lv1_map[Page.VM.VPage_Block].LV2_Block;
      // Print LV2 data
      fprintf(output,"  LV2: Access %o Status %o Meta %.2o Force-Allowed %o Byte-Code %o PPN %.8o\n",
	      pS[0].vm_lv2_ctl[lv2_index.raw].Access,pS[0].vm_lv2_ctl[lv2_index.raw].Status,pS[0].vm_lv2_ctl[lv2_index.raw].Meta,
	      pS[0].vm_lv2_ctl[lv2_index.raw].Force_Allowed,pS[0].vm_lv2_adr[lv2_index.raw].Byte_Code,pS[0].vm_lv2_adr[lv2_index.raw].PPN);
      if(ShadowMemoryPageMap[addr].Written != 0 || ShadowMemoryPageMap[addr].Paged != 0){
	int x = 0;
	while(x < 0x100){
	  fprintf(output,"[0x%.7x/0%.9o] CDR %o DTP %.2o PTR 0x%.7x/0%.9o\n",
		  Page.raw+x,Page.raw+x,ShadowMemory[Page.raw+x].cdr,ShadowMemory[Page.raw+x].dtp,
		  ShadowMemory[Page.raw+x].ptr,ShadowMemory[Page.raw+x].ptr);
	  x++;
	}
      }
      addr++;
    }
    fclose(output);
  }
#endif
  if(!(opts&DUMP_NO_INC_SEQ)){ dump_seq++; }
}

// Make a point where we can stop gdb without relying on a gdb conditional breakpoint, which involves a huge performance hit
void foo_hit(){
  printf("*** STOP GDB ***\n");
}

void sdu_cons_init(){
  struct sockaddr_in sdu_addr;

  // Open socket
  sdu_fd = socket(AF_INET, SOCK_STREAM, 0);
  if(sdu_fd < 0){
    perror("sdu_cons_init(): socket()");
    sdu_fd = -1;
    return;
  }

  int flags;
  // Become reusable
  flags = 1;
  if(setsockopt(sdu_fd,SOL_SOCKET,SO_REUSEADDR,&flags,sizeof(flags)) < 0){
    perror("sdu_fd:setsockopt()");
  }

  // Become nonblocking
  flags = fcntl(sdu_fd,F_GETFL,0);
  if(flags < 0){ flags = 0; }
  fcntl(sdu_fd,F_SETFL,flags|O_NONBLOCK);
  
  // Clobber and setup socket
  bzero((char *)&sdu_addr,sizeof(sdu_addr));
  sdu_addr.sin_family = AF_INET;
  sdu_addr.sin_addr.s_addr = INADDR_ANY;
  sdu_addr.sin_port = htons(3637);
  
  // Bind
  if(bind(sdu_fd,(struct sockaddr *)&sdu_addr,sizeof(sdu_addr)) < 0){
    perror("sdu_cons_init(): bind()");
    close(sdu_fd);
    sdu_fd = -1;
    exit(-1);
    return;
  }
  
  // Listen
  listen(sdu_fd,5);

  // All done!
  return;
}

// SDU console plumbing
void sdu_cons_clockpulse(){
  ssize_t res = 0;
  struct sockaddr_in other_addr;
  socklen_t socklen = sizeof(other_addr);
  if(sdu_fd < 0){ return; }
  if(sdu_conn_fd < 0){
    int flags;
    // Is anybody out there?
    sdu_conn_fd = accept(sdu_fd,(struct sockaddr *)&other_addr,&socklen);
    if(sdu_conn_fd < 0){
      if(errno != EAGAIN && errno != EWOULDBLOCK){
	perror("debug:accept()");
	close(sdu_fd);
	sdu_fd = -1;
      }
      sdu_conn_fd = -1;
      return;
    }
    printf("SDUCONS: Connection accepted\n");
    // Got one!
    // Negotiate character mode
    sdu_rx_buf[0] = 255; // IAC
    sdu_rx_buf[1] = 251; // WILL
    sdu_rx_buf[2] = 1;   // ECHO
    sdu_rx_buf[3] = 255; // IAC
    sdu_rx_buf[4] = 251; // WILL
    sdu_rx_buf[5] = 3;   // SGA
    sdu_rx_buf[6] = 255; // IAC
    sdu_rx_buf[7] = 252; // WON'T
    sdu_rx_buf[8] = 34;  // LINEMODE
    res = write(sdu_conn_fd,sdu_rx_buf,9);
    if(res < 0){
      perror("sdu_cons_clockpulse:write()");
    }
    bzero(sdu_rx_buf,9);
    // Become nonblocking
    flags = fcntl(sdu_conn_fd,F_GETFL,0);
    if(flags < 0){ flags = 0; }
    fcntl(sdu_conn_fd,F_SETFL,flags|O_NONBLOCK);    
    return;
  }else{
    res = read(sdu_conn_fd,sdu_rx_buf+sdu_rx_ptr,1);
    if(res < 0){
      if(errno != EAGAIN && errno != EWOULDBLOCK){
        perror("sducons:read()");
        close(sdu_conn_fd);
        sdu_conn_fd = -1;
      }
      return;
    }
    if(res == 0){ return; } // What?
    if(res != 1){
      printf("SDUCONS: BAD PACKET? Got %d bytes\n",(int)res);
      return;
    }else{
      // Got a byte      
      printf("SDUCONS: IO: 0x%x\n",sdu_rx_buf[sdu_rx_ptr]);
      if(sdu_tcmd_state > 0){
	printf("SDUCONS: TCMD %d\n",sdu_rx_buf[sdu_rx_ptr]);
	if(sdu_rx_buf[sdu_rx_ptr] == 0xFF){
	  // Quoted IAC
	  sdu_rx_ptr++;
          sducons_rx_int();
	  sdu_tcmd_state = 0;
	}else{
	  // Something else, eat the next two bytes
	  sdu_rx_buf[sdu_rx_ptr] = 0;	
	  sdu_tcmd_state++;
	  if(sdu_tcmd_state == 3){
	    sdu_tcmd_state = 0;
	  }
	}
      }else{
	if(sdu_rx_buf[sdu_rx_ptr] == 0xFF){ // Did we get IAC?
	  // Yes
	  sdu_tcmd_state = 1; sdu_rx_buf[sdu_rx_ptr] = 0;
	  printf("SDUCONS: IAC\n");
	}else{
	  // Is it a CR?
	  if(sdu_rx_buf[sdu_rx_ptr] == 0x0D){
	    sdu_rx_buf[sdu_rx_ptr] = 0x0A; // Make it a LF instead
	  }
	  // Is it a null?
	  if(sdu_rx_buf[sdu_rx_ptr] != 0x00){
	    // No, tell the SDU
	    sdu_rx_ptr++;
	    sducons_rx_int();	    
	  }
	}
      }
    }
  }
}

void sducons_write(char data){
  ssize_t res = 0;
  uint8_t tx_buf[2] = { 0,0 };
  if(sdu_conn_fd < 0){ return; }
  tx_buf[0] = data;
  res = write(sdu_conn_fd,tx_buf,1);
  if(res < 0){
    perror("sducons_write:write()");
  }
}

#ifdef BURR_BROWN
// Debug interface initialization
void debug_init(){
  struct sockaddr_in debug_addr;
  
  // Open debug socket
  debug_fd = socket(AF_INET, SOCK_STREAM, 0);
  if(debug_fd < 0){
    perror("debug_init(): socket()");
    debug_fd = -1;
    return;
  }
    
  if(debug_target_mode != 0){
    int flags;
    // Become nonblocking
    flags = fcntl(debug_fd,F_GETFL,0);
    if(flags < 0){ flags = 0; }
    fcntl(debug_fd,F_SETFL,flags|O_NONBLOCK);

    // Clobber and setup socket
    bzero((char *)&debug_addr,sizeof(debug_addr));
    debug_addr.sin_family = AF_INET;
    debug_addr.sin_addr.s_addr = INADDR_ANY;
    debug_addr.sin_port = htons(3636);
    
    // Bind
    if(bind(debug_fd,(struct sockaddr *)&debug_addr,sizeof(debug_addr)) < 0){
      perror("debug_init(): bind()");
      close(debug_fd);
      debug_fd = -1;
      return;
    }
    
    // Listen
    listen(debug_fd,5);
  }

  // All done!
  return;
}

// Debug interface: Connect to other side
void debug_connect(){
  struct hostent *target;
  struct sockaddr_in target_addr;
  if(debug_fd < 0){ return; }
  if(debug_conn_fd > 0){ return; } // Already connected!

  target = gethostbyname(debug_target_host);
  if(target == NULL){
    perror("debug_connect(): gethostbyname()");
    return;
  }
  bzero((char *)&target_addr, sizeof(target_addr));
  target_addr.sin_family = AF_INET;
  bcopy((char *)target->h_addr, 
	(char *)&target_addr.sin_addr.s_addr,
	target->h_length);
  target_addr.sin_port = htons(3636);
  if(connect(debug_fd,(struct sockaddr *)&target_addr,sizeof(target_addr)) < 0){
    perror("debug_connect()");    
  }else{
    int flags;
    debug_conn_fd = debug_fd;
    // Become nonblocking
    flags = fcntl(debug_fd,F_GETFL,0);
    if(flags < 0){ flags = 0; }
    fcntl(debug_fd,F_SETFL,flags|O_NONBLOCK);
  }
}

// Debug interface: send request
void debug_tx_rq(uint8_t rq,uint32_t addr,uint32_t data){
  extern uint8_t BB_Remote_Result;
  ssize_t res = 0;
  nuData tmp;
  uint8_t tx_buf[64];
  if(debug_fd < 0){ return; }
  if(debug_conn_fd < 0){ return; }
  // printf("DEBUG: Sending 0x%.2X Addr 0x%.8X Data 0x%.8X\n",rq,addr,data);
  BB_Remote_Result = 1; // Hold for result on read
  debug_last_addr = addr;
  // Assemble packet
  tmp.word = addr;
  tx_buf[0] = rq;
  tx_buf[1] = tmp.byte[0];
  tx_buf[2] = tmp.byte[1];
  tx_buf[3] = tmp.byte[2];
  tx_buf[4] = tmp.byte[3];
  tmp.word = data;
  tx_buf[5] = tmp.byte[0];
  tx_buf[6] = tmp.byte[1];
  tx_buf[7] = tmp.byte[2];
  tx_buf[8] = tmp.byte[3];  
  res = write(debug_conn_fd,tx_buf,9);  
  if(res < 0){
    perror("debug:write()");
  }
}

// Debug interface maintenance
void debug_clockpulse(){
  extern uint8_t BB_Remote_Result;
  ssize_t res = 0;
  struct sockaddr_in other_addr;
  socklen_t socklen = sizeof(other_addr);
  if(debug_fd < 0){ return; }  
  if(debug_conn_fd < 0){
    if(debug_target_mode != 0){
      // Is anybody out there?
      debug_conn_fd = accept(debug_fd,(struct sockaddr *)&other_addr,&socklen);
      if(debug_conn_fd < 0){
	if(errno != EAGAIN && errno != EWOULDBLOCK){
	  perror("debug:accept()");
	  close(debug_fd);
	  debug_fd = -1;
	}
	debug_conn_fd = -1;
	BB_Remote_Result = 0;
	return;
      }
      printf("DEBUG: Connection accepted\n");
      // Got one!      
    }  
    return; 
  }  
  switch(debug_io_state){
  case 0: // Nothing
    res = read(debug_conn_fd,debug_rx_buf,64);
    if(res < 0){
      if(errno != EAGAIN && errno != EWOULDBLOCK){
	perror("debug:read()");
	close(debug_conn_fd);
	debug_conn_fd = -1;
	BB_Remote_Result = 0;
      }
      return;
    }
    if(res == 0){ return; } // What?
    if(res != 9){
      printf("DEBUG: BAD PACKET? Got %d bytes\n",(int)res);
      // printf("BAD PACKET?\n");
      return;
    }
    // If we are the master, this is our ack/result
    if(debug_master_mode == 1){
      // Take it
      extern nuAddr BB_Remote_Addr;
      extern nuData BB_Remote_Data;
      uint32_t ack_addr = debug_rx_buf[4];
      ack_addr <<= 8; ack_addr |= debug_rx_buf[3];
      ack_addr <<= 8; ack_addr |= debug_rx_buf[2];
      ack_addr <<= 8; ack_addr |= debug_rx_buf[1];
      if(ack_addr == debug_last_addr){
	BB_Remote_Addr.raw = ack_addr;
	BB_Remote_Data.byte[0] = debug_rx_buf[5];
	BB_Remote_Data.byte[1] = debug_rx_buf[6];
	BB_Remote_Data.byte[2] = debug_rx_buf[7];
	BB_Remote_Data.byte[3] = debug_rx_buf[8];
	if(debug_rx_buf[0] == 0xFF){ BB_Remote_Result = 3; }else{ BB_Remote_Result = 2; }	
	// printf("DEBUG: Got ack 0x%.2X Addr 0x%.8X Data 0x%.8X\n",debug_rx_buf[0],BB_Remote_Addr.raw,BB_Remote_Data.word);
      }else{
	printf("DEBUG: Got out-of-order ack 0x%.2X Addr 0x%.8X Data 0x%.8X\n",debug_rx_buf[0],ack_addr,BB_Remote_Data.word);
      }
      debug_io_state = 0;
      return;
    }
    // Otherwise get the request
    if(debug_rx_buf[0]&0x08){
      uint8_t Command = debug_rx_buf[0]&0x07;
      switch(Command){
      case 0: // DIE
	printf("DEBUG: Remote requested termination\n");
	ld_die_rq = 1;
	lambda_dump(DUMP_T_MEM);
	return;
	break;
      }
    }
    debug_io_state = 1;    
    // Fall into...
  case 1: // New incoming request
    // Await bus
    if(NUbus_Busy != 0){ return; }
    debug_io_state++;
  case 2: // Issue request
    {
      nuAddr Remote_Addr;
      nuData Remote_Data;
      Remote_Addr.raw = debug_rx_buf[4];
      Remote_Addr.raw <<= 8; Remote_Addr.raw |= debug_rx_buf[3];
      Remote_Addr.raw <<= 8; Remote_Addr.raw |= debug_rx_buf[2];
      Remote_Addr.raw <<= 8; Remote_Addr.raw |= debug_rx_buf[1];
      Remote_Data.byte[0] = debug_rx_buf[5];
      Remote_Data.byte[1] = debug_rx_buf[6];
      Remote_Data.byte[2] = debug_rx_buf[7];
      Remote_Data.byte[3] = debug_rx_buf[8];
      // printf("DEBUG: Got RQ 0x%.2X Addr 0x%.8X Data 0x%.8X\n",debug_rx_buf[0],Remote_Addr.raw,Remote_Data.word);
      nubus_io_request(debug_rx_buf[0],0xF0,Remote_Addr.raw,Remote_Data.word);
    }
    debug_io_state++;
  case 3: // Await completion
    if(NUbus_error != 0){
      // Error
      debug_rx_buf[0] = 0xFF; // Error marker
    }
    if(NUbus_master == 0xF0 && NUbus_acknowledge == 1){
      // This is our answer, tell the master
      debug_tx_rq(debug_rx_buf[0],NUbus_Address.raw,NUbus_Data.word);
      debug_io_state = 0; // Ready for next packet
    }else{
      // Bus still busy?
      if(NUbus_Busy == 0){ 
	// No, our transaction failed
	printf("DEBUG: BUS TIMEOUT while handing RQ 0x%.2X Addr 0x%.8X Data 0x%.8X\n",NUbus_Request,NUbus_Address.raw,NUbus_Data.word);
	// printf("DEBUG: BUS TIMEOUT?\n");
	debug_tx_rq(0xFF,NUbus_Address.raw,0);
	debug_io_state = 0; // Ready for next packet
      }
      return;
    }    
  }  
  return;
}
#endif

// Handle writes to keyboard control reg #5 to click/not (cf vcmem.c)
void audio_control(int onoff) {
  static int state = 0;
  static uint64_t toggle_time = 0;

  if(onoff == state){
    return;
  }
  // this value seems to "work" for single beeps, and multiple beeps (around 4)
  // with default values for TV:BEEP-WAVELENGTH and TV:BEEP-DURATION,
  if(onoff && (real_time > (toggle_time + 1))){
    printf("\aBEEP\n");
    toggle_time = real_time;
  }
  state = onoff;
}

void parse_config_line(char *line){
  char *tok = NULL;
  tok = strtok(line," \t\r\n");
  if(tok == NULL){ return; }
  if(tok[0] == '#' || tok[0] == ';'){ return; } // Comment
  if(strcasecmp(tok,"ether_addr") == 0){
    // 3Com Ethernet address
    int x = 0;
    extern unsigned char ether_addr[6];
    while(x < 6){
      long int val = 0;
      tok = strtok(NULL," :\t\r\n");
      if(tok != NULL){
	val = strtol(tok,NULL,16);
      }
      ether_addr[x] = val;
      x++;
    }
    printf("Using 3Com Ethernet address %.2X:%.2X:%.2X:%.2X:%.2X:%.2X\n",
	   ether_addr[0],ether_addr[1],ether_addr[2],ether_addr[3],ether_addr[4],ether_addr[5]);
  }
  if(strcasecmp(tok,"ether_iface") == 0){
    extern char ether_iface[30];
    // 3Com Ethernet interface
    tok = strtok(NULL," \t\r\n");
    if(tok != NULL){
      strncpy(ether_iface,tok,30);
      printf("Using 3Com Ethernet interface %s\n",ether_iface);
    }
  }
  if(strcasecmp(tok,"disk") == 0){
    // Disk FN
    int dsk = 0;
    tok = strtok(NULL," \t\r\n");
    if(tok != NULL){
      int val = atoi(tok);
      dsk = val;
      tok = strtok(NULL," \t\r\n");
      if(tok != NULL){
	extern char disk_fn[4][64];
	strncpy(disk_fn[dsk],tok,64);
	printf("Using disk image %s for unit %d\n",tok,dsk);
      }
    }    
  }
  /* 
  if(strcasecmp(tok,"disk_sph") == 0){
    // Disk geometry - sectors per head
    tok = strtok(NULL," \t\r\n");
    if(tok != NULL){
      int val = atoi(tok);
      disk_geometry_sph = val;
      printf("Using %d sectors per head\n",disk_geometry_sph);
    }
  }
  if(strcasecmp(tok,"disk_spc") == 0){
    // Disk geometry - sectors per cylinder
    tok = strtok(NULL," \t\r\n");
    if(tok != NULL){
      int val = atoi(tok);
      disk_geometry_spc = val;
      printf("Using %d sectors per cylinder\n",disk_geometry_spc);
    }
  }
  */
  if(strcasecmp(tok,"sdu_switch") == 0){
    // SDU rotary switch position
    tok = strtok(NULL," \t\r\n");
    if(tok != NULL){
      int val = atoi(tok);
      sdu_rotary_switch = val;
      printf("SDU switch setting %d\n",sdu_rotary_switch);
    }
  }  
#ifdef BURR_BROWN
  if(strcasecmp(tok,"debug_target_mode") == 0){
    // Debug Target Mode
    tok = strtok(NULL," \t\r\n");
    if(tok != NULL){
      int val = atoi(tok);
      debug_target_mode = val;
      printf("Using Debug Target Mode %d\n",debug_target_mode);
    }
  }
  if(strcasecmp(tok,"debug_target_host") == 0){
    // Debug Target Hostname
    tok = strtok(NULL," \t\r\n");
    if(tok != NULL){
      strncpy(debug_target_host,tok,128);
      printf("Using Debug Target Hostname %s\n",debug_target_host);
    }    
  }
#endif
  if(strcasecmp(tok,"mouse_mode") == 0){
    // Mouse Operation Mode
    tok = strtok(NULL," \t\r\n");
    if(tok != NULL){
      int val = atoi(tok);
      switch(val){
      case 1: // Shared
	mouse_op_mode = 1;
	printf("Using Shared mode for mouse interface\r\n");
	break;
      case 0: // Direct
      default: // Direct
	mouse_op_mode = 0;
	printf("Using Direct mode for mouse interface\r\n");
	break;
      }
    }
  }
  // Mouse A-memory locations
  if(strcasecmp(tok,"mouse_x_loc_0") == 0){
    tok = strtok(NULL," \t\r\n");
    if(tok != NULL){
      int val = strtol(tok,NULL,8);
      mouse_x_loc[0] = val;
      printf("Using A-%o for CP 0 Mouse X\n",mouse_x_loc[0]);
    }
  }
  if(strcasecmp(tok,"mouse_x_loc_1") == 0){
    tok = strtok(NULL," \t\r\n");
    if(tok != NULL){
      int val = strtol(tok,NULL,8);
      mouse_x_loc[1] = val;
      printf("Using A-%o for CP 1 Mouse X\n",mouse_x_loc[1]);
    }
  }
  if(strcasecmp(tok,"mouse_y_loc_0") == 0){
    tok = strtok(NULL," \t\r\n");
    if(tok != NULL){
      int val = strtol(tok,NULL,8);
      mouse_y_loc[0] = val;
      printf("Using A-%o for CP 0 Mouse Y\n",mouse_y_loc[0]);
    }
  }
  if(strcasecmp(tok,"mouse_y_loc_1") == 0){
    tok = strtok(NULL," \t\r\n");
    if(tok != NULL){
      int val = strtol(tok,NULL,8);
      mouse_y_loc[1] = val;
      printf("Using A-%o for CP 1 Mouse Y\n",mouse_y_loc[1]);
    }
  }
  if(strcasecmp(tok,"mouse_wake_loc_0") == 0){
    tok = strtok(NULL," \t\r\n");
    if(tok != NULL){
      int val = strtol(tok,NULL,8);
      mouse_wake_loc[0] = val;
      printf("Using A-%o for CP 0 Mouse Wake\n",mouse_wake_loc[0]);
    }
  }
  if(strcasecmp(tok,"mouse_wake_loc_1") == 0){
    tok = strtok(NULL," \t\r\n");
    if(tok != NULL){
      int val = strtol(tok,NULL,8);
      mouse_wake_loc[1] = val;
      printf("Using A-%o for CP 1 Mouse Wake\n",mouse_wake_loc[1]);
    }
  }
  if(strcasecmp(tok,"pixel_on") == 0){
    tok = strtok(NULL," \t\r\n");
    if(tok != NULL){
      uint32_t sval = strtol(tok,(char **)NULL, 16);
      pixel_on = sval;
      printf("pixel_on set to 0x%X\n", pixel_on);
    }
  }
  if(strcasecmp(tok,"pixel_off") == 0){
    tok = strtok(NULL," \t\r\n");
    if(tok != NULL){
      uint32_t sval = strtol(tok,(char **)NULL, 16);
      pixel_off = sval;
      printf("pixel_off (%s) set to 0x%X\n", tok, pixel_off);
    }
  }
  if(strcasecmp(tok,"sdl_quit") == 0){
    // Turn off handling of SDL_QUIT event (caused by e.g. Command-Q on a Mac)
    tok = strtok(NULL," \t\r\n");
    if(tok != NULL){
      if((strcasecmp(tok,"on") == 0) || (strcasecmp(tok,"yes") == 0) || (strcasecmp(tok,"true") == 0)){
        quit_on_sdl_quit = 1;
      }else{
	if((strcasecmp(tok,"off") == 0) || (strcasecmp(tok,"no") == 0) || (strcasecmp(tok,"false") == 0)){
	  quit_on_sdl_quit = 0;
	}else{
	  printf("sdl_quit: unrecognized value '%s' (on/true/yes, off/false/no)\n", tok);
	}
      }
    }
    return;
  }

  if(strcasecmp(tok,"map_key") == 0){
    // Alter keyboard map
    tok = strtok(NULL," \t\r\n");
    if(tok != NULL){
      int sval = atoi(tok);
      tok = strtok(NULL," \t\r\n");
      if(tok != NULL){
	int dval = strtol(tok,NULL,8);
	map[sval] = dval;
	// Assign modifier bits
	switch(dval){
	case 0003: // Mode Lock
	  modmap[sval] = KB_BB_MODELOCK; break;
	case 0005: // Left Super
	  modmap[sval] = KB_BB_LSUPER; break;
	case 0015: // Alt Lock
	  modmap[sval] = KB_BB_ALTLOCK; break;
	case 0020: // Left Control
	  modmap[sval] = KB_BB_LCTL; break;
	case 0024: // Left Shift
	  modmap[sval] = KB_BB_LSHIFT; break;
	case 0025: // Right Shift
	  modmap[sval] = KB_BB_RSHIFT; break;
	case 0026: // Right Control
	  modmap[sval] = KB_BB_RCTL; break;
	case 0044: // Left Greek (there is no Right Greek)
	  modmap[sval] = KB_BB_GREEK; break;
	case 0045: // Left Meta
	  modmap[sval] = KB_BB_LMETA; break;
	case 0065: // Right Super
	  modmap[sval] = KB_BB_RSUPER; break;
	case 0104: // Left Top
	  modmap[sval] = KB_BB_LTOP; break;
	case 0115: // Repeat
	  modmap[sval] = KB_BB_REPEAT; break;
	case 0145: // Left Hyper
	  modmap[sval] = KB_BB_LHYPER; break;
	case 0155: // Right Top
	  modmap[sval] = KB_BB_RTOP; break;
	case 0165: // Right Meta
	  modmap[sval] = KB_BB_RMETA; break;
	case 0175: // Right Hyper
	  modmap[sval] = KB_BB_RHYPER; break;
	default:
	  modmap[sval] = 0; // Not a modifier
	  break;
	}
	printf("Mapped SDL keycode %d to Lambda keycode 0%o\n",sval,dval);
      }else{
	printf("key_map: Missing octal Lambda key code.\n");
      }
    }else{
      printf("key_map: Missing decimal SDL keycode value\n");
    }
  }

}

// One nubus clock cycle
// Can be driven by the SDU 8088 or not.
int bcount = 0; // Bus Cycle Counter
int icount=0; // Main cycle counter

// The Lambda and nubus are run at 5 MHz.
void nubus_cycle(int sdu){
  if(bcount == 5){
    // Update microsecond clock if that's enabled (NB: AUX stat only!)
    if(pS[0].RG_Mode.Aux_Stat_Count_Control == 6){
      pS[0].stat_counter_aux++;
    }
    if(pS[1].RG_Mode.Aux_Stat_Count_Control == 6){
      pS[1].stat_counter_aux++;
    }
    bcount = 0;
  }
  // Clock lambda
  lambda_clockpulse(0);
#ifdef CONFIG_2X2
  lambda_clockpulse(1);
#endif
  // Other devices go here  
  sdu_clock_pulse();
  if(sdu == 0){
    smd_clock_pulse();
    tapemaster_clock_pulse();
    enet_clock_pulse();
  }
  mem_clock_pulse();
  vcmem_clock_pulse(0);
#ifdef CONFIG_2X2
  vcmem_clock_pulse(1);
#endif
  // Nubus signal maintenance goes last
  nubus_clock_pulse();
  bcount++; // Count bus cycles  
  icount++; // Main cycle
}

// Main
int main(int argc, char *argv[]){
  FILE *config;
  keyboard_io_ring_top[0] = keyboard_io_ring_top[1] = 0;
  keyboard_io_ring_bottom[0] = keyboard_io_ring_bottom[1] = 0;
  mouse_io_ring_top[0] = mouse_io_ring_top[1] = 0;
  mouse_io_ring_bottom[0] = mouse_io_ring_bottom[1] = 0;

  printf("LambdaDelta\n");  

  // Read default keymap
#ifdef SDL1
  init_sdl_to_keysym_map();
#endif

#ifdef SDL2
  init_sdl_to_scancode_map();
#endif

  // Obtain configuration
  config = fopen("ld.conf","r");
  if(!config){
    printf("Can't open ld.conf\n");
  }else{
    while(!feof(config)){
      char buf[128];
      char *ret = NULL;
      ret = fgets(buf,128,config);
      if(ret != NULL){
	parse_config_line(buf);
      }
    }
  }
  
  // Handle command-line options
  if(argc > 1){
    int x = 1;
    while(x < argc){
#ifdef BURR_BROWN
      if(strcmp("-d",argv[x]) == 0){
	debug_target_mode = 10;
	printf("DEBUG TARGET MODE 10\n");
      }
#endif
      if(strcmp("-?",argv[x]) == 0){
        printf("\nUsage: ld [OPTIONS]\n");
        printf("Valid options:\n");
#ifdef BURR_BROWN
	printf("  -d  Enable debug target mode\n");
#endif
	printf("  -?  Print this text\n");
	exit(0);
      }
      x++;
    }
  }

  if(smd_init() < 0){
    exit(-1);
  }
  tapemaster_init();
  
  read_sym_files();

  if(sdu_rotary_switch != 1){
    sdu_cons_init();
  }

#ifdef BURR_BROWN
  // Debug init
  debug_init();

  // Ethernet initialization
  /* OBSOLETED WHEN 3COM SPLIT
  if(debug_target_mode < 10){
    ether_fd = ether_init();
    if(ether_fd < 0){
      perror("ether_init()");
      ether_fd = -1;
    }
  }
  */

  // SDL display initialization
  if(debug_target_mode < 10){
    sdl_init(VIDEO_WIDTH,VIDEO_HEIGHT);
  }

  hw_init();

  if(debug_target_mode > 9){
    extern int SDU_state;
    printf("CLOCK STOPPED\n");
    SDU_state = -1; // Stop SDU from IPLing machine
    pS[0].cpu_die_rq = 1; // Stop clock
    pS[1].cpu_die_rq = 1; // Stop clock
  }
#else
  // Ethernet initialization
  /*
  ether_fd = ether_init();
  if(ether_fd < 0){
    perror("ether_init()");
    ether_fd = -1;
  }
  */

  // SDL display initialization
  sdl_init(VIDEO_WIDTH,VIDEO_HEIGHT);

  hw_init();
#endif

  // Read in NVRAM
  read_nvram();
  // And RTC NVRAM
  read_rtc_nvram();

  // Read in ROMs
  read_sdu_rom();
  read_vcmem_rom();
  read_mem_rom();

  // If the debug switch is on debug/install mode
  if(sdu_rotary_switch == 0){
    // Wait here for telnet
    while(sdu_conn_fd < 0){
      sdu_cons_clockpulse();
      usleep(0);
    }
  }
  
  while(ld_die_rq == 0){
    // New loop
    icount -= 500000; // Don't clobber extra cycles if they happened
    // Run for 1/10th of a second, or 100000 cycles
    while(icount < 500000 && ld_die_rq == 0){
      int x=0;
      // Lambda runs at 5 MHz, so it gets 5 cycles
      while(x < 5 && ld_die_rq == 0){
#ifdef BURR_BROWN
	// Clock debug interface
        debug_clockpulse();
#endif
	if(x == 0){
	  // The 8088 does more than one cycle worth of work in one tick,
	  // so we clock it here. 1 MIPS was as fast as an 8088 got.
	  // NB: This will cause nubus cycles if the 8088 has to wait for the bus!
	  i8086_clockpulse();
	}
	// Step lambda and nubus (8088 might have already done this!)
	nubus_cycle(0);
	x++;
      }
      // icount++;
    }
    // Redraw display and process input
    sdl_refresh();
    // Plumb SDU console
    if(sdu_rotary_switch != 1){
      sdu_cons_clockpulse();
    }
    // Update status line
    if(stat_time > 9){
      char statbuf[512];
      // Update status line
      extern char tape_fn[];
      sprintf(statbuf,"LambdaDelta: VC %d | Tape: %s | ",active_console,tape_fn);
      switch(cp_state[active_console]){
      case 0: // Cold (or under 8088 control!)
	if(pS[active_console].cpu_die_rq){
	  sprintf(statbuf,"%sCold Halted",statbuf);
	}else{
	  sprintf(statbuf,"%sCold Running",statbuf);
	}
	break;
      case 1: // Bootstrapping
	if(pS[active_console].cpu_die_rq){
	  sprintf(statbuf,"%sCold Halted",statbuf);
	}else{
	  sprintf(statbuf,"%sCold Booting",statbuf);
	}
	break;
      case 2: // Lisp Booting
	if(pS[active_console].cpu_die_rq){
	  sprintf(statbuf,"%sLisp Boot Halted",statbuf);
	}else{
	  sprintf(statbuf,"%sLisp Booting",statbuf);
	}
	break;
      case 3: // Lisp Running
	if(pS[active_console].cpu_die_rq){
	  sprintf(statbuf,"%sHalted",statbuf);
	}else{
	  sprintf(statbuf,"%sRunning",statbuf);
	}
	break;
      default: // ???
	sprintf(statbuf,"%sUnknown State %d",statbuf,cp_state[active_console]);
	break;
      }
      sprintf(statbuf,"%s | DT %ld",statbuf,(emu_time-real_time));
#ifdef SDL1
      SDL_WM_SetCaption(statbuf, "LambdaDelta");
#endif
#ifdef SDL2
      SDL_SetWindowTitle(SDLWindow, statbuf);
#endif
      stat_time = 0;
    }
    // Emulated time passed
    emu_time++;
    // Timer won't wrap for many years, so we don't have to care about that
    // Are we ahead of real time?
    if(emu_time > real_time){
      // Yes, wait.
      while(emu_time > real_time){
	usleep(0); // Allow real time to pass
      }
    }
    // Otherwise loop
  }

  // Save framebuffer image
#ifdef BURR_BROWN
  if(debug_target_mode < 10){
    FB_dump(0);
    FB_dump(1);
  }

  // Is there a debug target?
  if(debug_conn_fd > 0){
    // Kill target, sorry target
    debug_tx_rq(0x08,0,0);
    usleep(0);
  }
#else
  FB_dump(0);
  FB_dump(1);
#endif

  printf("Run completed\n");
  // sdl_cleanup() will write out NVRAM and terminate the process.
  // It will not return.
  sdl_cleanup();
  return(0);
}
