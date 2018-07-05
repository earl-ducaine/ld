

// Guard
#ifndef KEYBOARD_SDL_H
#define KEYBOARD_SDL_H

// Framebuffer size
#define VIDEO_HEIGHT 800
#define VIDEO_WIDTH 1024

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

typedef struct DisplayState {
  unsigned char *data;
  int linesize;
  int depth;
  int width;
  int height;
} DisplayState;

extern uint16_t map[];
extern uint32_t modmap[];
extern uint32_t kb_buckybits;
extern int active_console;

// Pointer capture state in mode 0, pointer hide/show state in mode 1
extern uint8_t mouse_capture;

// 0 = direct, 1 = shared
extern int mouse_op_mode;

// 0 = cold, 1 = bootstrap, 2 = lisp, 3 = lisp + mouse initialized
extern int cp_state[];

// Inihibit the next SDL mouse event when Lisp warps the mouse
extern int mouse_update_inhibit;

extern uint8_t mouse_phase;
extern uint8_t mouse_last_buttons;
extern uint8_t sdu_rx_ptr;
extern int sdu_conn_fd;
extern int sdu_fd;

// Throttle timers
extern uint64_t volatile real_time;
extern uint64_t volatile emu_time;
extern uint32_t volatile stat_time;

// Pixels
extern uint32_t pixel_on;
extern uint32_t pixel_off;

extern int u_minh,
  u_maxh,
  u_minv,
  u_maxv;
extern int quit_on_sdl_quit;
extern uint32_t FB_Image[2][VIDEO_WIDTH*VIDEO_HEIGHT];
extern uint32_t mouse_x_loc[];
extern uint32_t mouse_y_loc[];
extern uint32_t mouse_wake_loc[];

// 1 => white-on-black, 0 => black-on-white
extern int black_on_white[];

void init_sdl_keyboard();
void set_caption(char statbuf[]);
void sdl_cleanup(void);

#endif // KEYBOARD_SDL_H
