
#include <stdbool.h>
#include <SDL.h>
#include <SDL_keycode.h>
#include <sys/time.h>
#include <signal.h>

#include "ld.h"
#include "keyboard-sdl.h"
#include "lambda_cpu.h"

// Processor states
extern struct lambdaState pS[];

// SDL2 state
SDL_Window *SDLWindow;
SDL_Renderer *SDLRenderer;
SDL_Texture *SDLTexture;
SDL_TimerID SDLTimer;
uint32_t FrameBuffer[VIDEO_HEIGHT*VIDEO_WIDTH];

void init_sdl_to_scancode_map(void) {
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

void kbd_handle_char(int scancode, int down) {
  int sdlchar = scancode;
  unsigned char outchar = 0;
  // Check for debug
  if (sdlchar == SDL_SCANCODE_F12) {
    if (down) {
      if (((kb_buckybits & KB_BB_LSHIFT) |
	   (kb_buckybits & KB_BB_RSHIFT)) !=
	  0) {
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
  if (sdlchar == SDL_SCANCODE_F11) {
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
  if (sdlchar == SDL_SCANCODE_F10) {
    if (down) {
      if (mouse_capture != 0) {
        mouse_capture = 0;
	if (mouse_op_mode == 0) {
	  SDL_SetRelativeMouseMode(SDL_FALSE);
	}
	SDL_ShowCursor(SDL_ENABLE);
      } else {
        mouse_capture = 1;
	if (mouse_op_mode == 0) {
	  SDL_SetRelativeMouseMode(SDL_TRUE);
	}
	SDL_ShowCursor(SDL_DISABLE);
      }
    }
  }
  // Check for console switch key

#ifdef CONFIG_2X2
  if (sdlchar == SDL_SCANCODE_F9) {
    if (down) {
      // Switch active console
      active_console ^= 1;
      printf("CONSW: %d\n",active_console);
      // Update window title
      stat_time = 20;
      // Refresh display bitmap from stored image
      uint32_t *p = FrameBuffer;
      uint32_t *s = FB_Image[active_console];
      int i,
	j;
      for (i = 0; i < VIDEO_WIDTH; i++) {
        for (j = 0; j < VIDEO_HEIGHT; j++) {
          *(p++) = *(s++);
	}
      }
      // Redraw it
      SDL_UpdateTexture(SDLTexture, NULL, FrameBuffer, (VIDEO_WIDTH * 4));
      SDL_RenderClear(SDLRenderer);
      SDL_RenderCopy(SDLRenderer, SDLTexture, NULL, NULL);
      SDL_RenderPresent(SDLRenderer);
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
  if (sdlchar >= 'a' && sdlchar <= 'z'){
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
    if (((kb_buckybits&KB_BB_LCTL)|(kb_buckybits&KB_BB_RCTL)) != 0){
      outchar |= 0x10;
    }
    if (((kb_buckybits&KB_BB_LMETA)|(kb_buckybits&KB_BB_RMETA)) != 0){
      outchar |= 0x08;
    }
    if (((kb_buckybits&KB_BB_LSUPER)|(kb_buckybits&KB_BB_RSUPER)) != 0){
      outchar |= 0x04;
    }
    if (((kb_buckybits&KB_BB_LHYPER)|(kb_buckybits&KB_BB_RHYPER)) != 0){
      outchar |= 0x02;
    }
    if ((kb_buckybits&KB_BB_GREEK) != 0){
      outchar |= 0x01;
    }
  } else {
    // Key Up
    if (modmap[sdlchar] != 0) {
      kb_buckybits &= ~modmap[sdlchar];
    }
    // Take "up" bucky bits
    if((kb_buckybits&KB_BB_MODELOCK) != 0){
      outchar |= 0x10;
    }
    if((kb_buckybits&KB_BB_ALTLOCK) != 0){
      outchar |= 0x08;
    }
    if((kb_buckybits&KB_BB_CAPSLOCK) != 0){
      outchar |= 0x04;
    }
    if((kb_buckybits&KB_BB_REPEAT) != 0){
      outchar |= 0x02;
    }
    if(((kb_buckybits&KB_BB_LTOP)|(kb_buckybits&KB_BB_RTOP)) != 0){
      outchar |= 0x01;
    }
  }
  // Send result
  put_rx_ring(active_console, outchar);
  // Third byte is Source ID. (Newer Keyboard)
  put_rx_ring(active_console, 0x02);
}

void sdl_system_shutdown_request(void) {
  exit(0);
}

static void sdl_process_key(SDL_KeyboardEvent* ev, int updown) {
  kbd_handle_char(ev->keysym.scancode, updown);
}

static void sdl_send_mouse_event(void) {
  int state,
    xm,
    ym;
  uint8_t buttons = 0x07;
  if (mouse_op_mode == 0) {
    // Direct Mode
    state = SDL_GetRelativeMouseState(&xm, &ym);
    // Disregard mouse when not captured, unless we are recapturing it.
    if(mouse_capture == 0 && (state & SDL_BUTTON(SDL_BUTTON_LEFT))) {
      mouse_capture = 2;
      return;
    }
    if (mouse_capture == 2 && !(state & SDL_BUTTON(SDL_BUTTON_LEFT))) {
      mouse_capture = 1;
      SDL_SetRelativeMouseMode(SDL_TRUE);
      return;
    }
    if (mouse_capture != 1) {
      return;
    }
    // if(!mouse_init){ return; }
    if (cp_state[active_console] != 3){
      return;
    }
    // Proceed
    if (state & SDL_BUTTON(SDL_BUTTON_LEFT)){
      buttons ^= 0x04;
    }
    if (state & SDL_BUTTON(SDL_BUTTON_MIDDLE)){
      buttons ^= 0x02;
    }
    if (state & SDL_BUTTON(SDL_BUTTON_RIGHT)){
      buttons ^= 0x01;
    }
    if((mouse_phase == 1) && (buttons != mouse_last_buttons)) {
      put_mouse_rx_ring(active_console, 0);
      put_mouse_rx_ring(active_console, 0);
      mouse_phase ^= 1;
    }
    // Construct packet -- Y movement is reversed
    ym = -ym;
    // Scale movement
    xm /= 2;
    ym /= 2;
    if (xm == 0 && ym == 0 && buttons == mouse_last_buttons){
      return;
    }
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
void warp_mouse_callback(int cp) {
  // Make sure we care first
  if ((mouse_op_mode != 1) ||
      (cp_state[cp] != 3) || (cp != active_console)) {
    return;
  }
  // Are we the active window?
  if((SDL_GetWindowFlags(SDLWindow) & SDL_WINDOW_MOUSE_FOCUS) == 0){
    return;
  }
  // Otherwise proceed
  mouse_update_inhibit++;
  // printf("WARP MOUSE 0x%X,0x%X\n",pS[cp].Amemory[mouse_x_loc[cp]],pS[cp].Amemory[mouse_y_loc[cp]]);
  SDL_WarpMouseInWindow(SDLWindow,
			(pS[cp].Amemory[mouse_x_loc[cp]] & 0xFFFF),
			(pS[cp].Amemory[mouse_y_loc[cp]] & 0xFFFF));
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
    SDL_UpdateTexture(SDLTexture, NULL, FrameBuffer, (VIDEO_WIDTH * 4));
    SDL_RenderClear(SDLRenderer);
    SDL_RenderCopy(SDLRenderer, SDLTexture, NULL, NULL);
    SDL_RenderPresent(SDLRenderer);
  }else{
    for (i = 0; i < VIDEO_WIDTH; i++) {
      for (j = 0; j < VIDEO_HEIGHT; j++) {
	*p = ((*p == pixel_off) ? pixel_on : pixel_off);
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

void sdl_refresh(void) {
  SDL_Event ev1;
  SDL_Event* ev = &ev1;
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

void sdl_cleanup(void) {
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


void init_sdl_keyboard() {
  // Read default keymap
  init_sdl_to_scancode_map();
}


void set_caption(char statbuf[]) {
      SDL_SetWindowTitle(SDLWindow, statbuf);
}
