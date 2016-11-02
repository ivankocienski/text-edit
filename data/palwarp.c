
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <SDL/SDL.h>

typedef unsigned char BYTE;

enum {
  AT_LOOPED,
  AT_CLOSEST
};

SDL_Surface *screen;

SDL_Color *palette = NULL;

typedef struct _S_ANIMATOR {
  BYTE value;
  BYTE target;
  char dir;
  BYTE delay;
  BYTE delay_value;
} T_ANIMATOR, *PT_ANIMATOR;

typedef struct _S_COLORBAR {
  BYTE r;
  BYTE g;
  BYTE b;

  BYTE pos;
  BYTE size;

  T_ANIMATOR pos_animator;
  T_ANIMATOR size_animator;

  T_ANIMATOR r_anim;
  T_ANIMATOR g_anim;
  T_ANIMATOR b_anim;

} T_COLORBAR, *PT_COLORBAR;

T_COLORBAR color_bar;

void render_bar(PT_COLORBAR cb) {

  BYTE hp;
  BYTE p1;
  BYTE p2;
  int i;
  int j;
  int mr;
  int mg;
  int mb;

  int stepr;
  int stepg;
  int stepb;

  if(cb->size < 2) return;
  if(!(cb->r + cb->g + cb->b)) return;

  hp   = cb->size / 2;
  p1   = cb->pos - hp + 1;
  p2   = cb->pos + hp;

  stepr = (cb->r << 8) / hp;
  stepg = (cb->g << 8) / hp;
  stepb = (cb->b << 8) / hp;

  mr = 0;
  mg = 0;
  mb = 0;

  //printf( "step=%d\n", step );

  for(i = 0; i < hp; i++) {

    mr += stepr;
    mg += stepg;
    mb += stepb; 

    j = palette[p1].r + (mr >> 8); if(j > 255) j = 255; palette[p1].r = j;
    j = palette[p1].g + (mg >> 8); if(j > 255) j = 255; palette[p1].g = j;
    j = palette[p1].b + (mb >> 8); if(j > 255) j = 255; palette[p1].b = j;

    j = palette[p2].r + (mr >> 8); if(j > 255) j = 255; palette[p2].r = j;
    j = palette[p2].g + (mg >> 8); if(j > 255) j = 255; palette[p2].g = j;
    j = palette[p2].b + (mb >> 8); if(j > 255) j = 255; palette[p2].b = j;

    p1++; 
    p2--;
  } 
}

BYTE animate( PT_ANIMATOR an, int type ) {

  if(!an->dir) {

    /* initialize */
    an->dir         = (rand() & 1) ? 1 : -1;
    an->value       = rand();
    an->target      = rand();
    an->delay_value = rand() % 5;
    an->delay       = an->delay_value;

    return an->value;
  }

  if(an->delay) {
    an->delay--;
    return an->value;
  }

  if(an->value == an->target) {

    an->target      = rand();
    an->dir         = (rand() & 1) ? 1 : -1;
    an->delay_value = rand() % 5;
  }

  switch(type) {
    case AT_LOOPED:
      an->value += an->dir;
      break;

    case AT_CLOSEST:
      if(an->target < an->value) 
        an->value--;
      else
        an->value++;
      break;

  }

  an->delay = an->delay_value;

  return an->value;

}

void init_bars() {

  memset( &color_bar, 0, sizeof(color_bar));
}

void update() {

  memset( palette, 0, sizeof(SDL_Color) * 256 );

  color_bar.pos  = animate( &color_bar.pos_animator,  AT_LOOPED  );
  color_bar.size = animate( &color_bar.size_animator, AT_CLOSEST );

  color_bar.r = animate( &color_bar.r_anim, AT_CLOSEST );
  color_bar.g = animate( &color_bar.g_anim, AT_CLOSEST );
  color_bar.b = animate( &color_bar.b_anim, AT_CLOSEST );

  render_bar( &color_bar ); 

  SDL_SetColors( screen, palette, 1, 255 );
}

void cleanup() {

  if(palette) free(palette);
  SDL_Quit();

}

void init() {

  int x;
  int y;
  BYTE *p;


  srand(time(NULL));

  if( SDL_Init( SDL_INIT_VIDEO ) == -1) {

    fprintf(stderr, "could not init SDL\n");
    exit(-1); 
  }

  screen = SDL_SetVideoMode( 640, 480, 8, SDL_SWSURFACE | SDL_DOUBLEBUF );
  if(!screen) {
    fprintf(stderr, "could not set video mode\n");
    exit(-1);
  }

  SDL_WM_SetCaption( "teapot", NULL );

  /* set up initial palette */
  palette = (SDL_Color *)calloc( 256, sizeof(SDL_Color) );
  if(!palette) {
    perror("allocating palette space");
    exit(0);
  }

  /* draw pattern into screen buffer */
  SDL_LockSurface( screen );
  p = screen->pixels;
  
  for(y = 0; y < 100; y++) 
    for(x = 0; x < 256; x++) {

      p[(y * 640) + x] = x;
    }
  
  SDL_UnlockSurface( screen );
}

int main(int argc, char ** argv ) {

  SDL_Event ev;

  atexit(cleanup);

  init();
  init_bars();

  while(1) {

    /* Check for events */
    while(SDL_PollEvent(&ev)) {  

      switch(ev.type) { 
        case SDL_QUIT:
          exit(0);
          break;

        case SDL_KEYUP:
          if(ev.key.keysym.sym == SDLK_F12) 
            return 0;
          break;
      }
    }

    update();

    SDL_Flip(screen); // shouldn't really need to do this
    SDL_Delay(20);
  }


  return 0;
}


