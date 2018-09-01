#include <stdio.h>
#include <ctype.h>
#include <hosttab.h>
#include <sys/chaos.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include <utmp.h>
#ifdef BSD42
#include <sys/time.h>
#else
#include <time.h>
#endif
#include <pwd.h>
/*
 * SEND server from another user on the chaosnet
 */


/* $Log: SEND.c.x10,v $
/* Revision 1.1.1.1  1998/09/07 18:56:06  brad
/* initial checkin of initial release
/*
 * Revision 1.2  86/10/12  13:12:21  mbm
 * Use group write permission on tty for 4.3
 *  */

#define MAXLINES 64
#define MAXLINELEN 256

char *lines[MAXLINES] = {0};
char display[64] = {0};

#define SPOOL "/usr/spool/sends"

struct	utmp ubuf;
#define NMAX sizeof(ubuf.ut_name)
#define LMAX sizeof(ubuf.ut_line)
#define FALSE 0
#define TRUE 1
#define bool int

char of[sizeof(SPOOL)+1+NMAX+1];

char	*strcat();
char	*strcpy();
int	signum[] = {SIGHUP, SIGINT, SIGQUIT, 0};
char	me[10]	= "???";
char	*him;
char	*mytty;
char	histty[32];
char	*histtya;
char	*ttyname();
char	*rindex();
int	logcnt;
int	eof();
int	timout();

char 	*malloc(), *realloc(), free();;
char	*getenv();

main(argc, argv)
char *argv[];
{
  struct stat stbuf;
  register i;
  register FILE *uf;
  int c1, c2;
  bool brief = FALSE;	/* for CHAOS/NOTIFY like performance */
  long	clock = time( 0 );
  struct tm *localtime();
  struct tm *localclock = localtime( &clock );
  struct chstatus chst;
  int ofd;
  struct passwd *pwd, *getpwnam();

  /* stuff for buffer */
  int linectr, charctr;
  char *line;


  
  if(argc < 2)
    {
      chreject(0, "No user name supplied");
      exit(1);
    }
  
  /* get target username */
  him = argv[1];
  lcase(him);			/* lowercase users only! */

  /* target starts with '-' implies brief mode */
  if( him[0] == '-' )
    {
      him++;
      brief = TRUE;
    }

  if(argc > 2)
    histtya = argv[2];

  if ((uf = fopen("/etc/utmp", "r")) == NULL)
    {
      printf("cannot open /etc/utmp\n");
      goto cont;
    }

  if (histtya)
    {
      strcpy(histty, "/dev/");
      strcat(histty, histtya);
    }
  
  /* magic code to parse utmp
     I don't understand the '-' stuff
     */
  while (fread((char *)&ubuf, sizeof(ubuf), 1, uf) == 1)
    {
      if (ubuf.ut_name[0] == '\0')
	continue;
      if(him[0] != '-' || him[1] != 0)
	for(i=0; i<NMAX; i++)
	  {
	    c1 = him[i];
	    c2 = ubuf.ut_name[i];
	    if(c1 == 0)
	      if(c2 == 0 || c2 == ' ')
		break;
	    if(c1 != c2)
	      goto nomat;
	  }
      logcnt++;
      if (histty[0]==0)
	{
	  strcpy(histty, "/dev/");
	  strcat(histty, ubuf.ut_line);
	}
    nomat:
      ;
    }

 cont:
  if (logcnt==0 && histty[0]=='\0')
    {
      chreject(0, "User not logged in.");
      exit(1);
    }
  fclose(uf);
  if(histty[0] == 0)
    {
      chreject(0, "User not logged in out given tty.");
      exit(1);
    }
  if (access(histty, 0) < 0)
    {
      chreject(0, "User's tty non-existent.");
      exit(1);
    }

  signal(SIGALRM, timout);
  alarm(5);

  if (freopen(histty, "w", stdout) == NULL)
    goto perm;

  alarm(0);			/* no more worry about hanging */

  if (fstat(fileno(stdout), &stbuf) < 0)
    goto perm;
  if ((stbuf.st_mode&020) == 0)	/* group write permission to allow sends */
    goto perm;

  /* accept the connection */
  ioctl(0, CHIOCACCEPT, 0);
  ioctl(0, CHIOCGSTAT, &chst);

  /* read the data into a buffer */
  linectr = charctr = 0;
  line = NULL;

  /* loop on each character */
  while( (c1 = getchar()) != EOF )
    {
      switch(c1)
	{
	  /* finish line if currently on line, else punt */
	  /* multiple NL's are punted - this seems wrong */
	case CHNL:
	case '\n':
	case '\r':
	  if( line != NULL )
	    {
	      line[charctr] = '\0';
	      line = NULL;
	    }
	  break;
	 
	case CHTAB:
	  c1 = '\t';
	  /* fall through! */

	  /* reg char ==> start a new line if need be, and add it */
	default:
	  if( line == NULL )
	    {
	      if( linectr >= MAXLINES )
		linectr--;
	      line = lines[linectr++] = malloc(MAXLINELEN*sizeof(char));
	      if( line == NULL )
		die("Malloc failed");
	      charctr = 0;
	    }

	  if( charctr < MAXLINELEN )
	    line[charctr++] = c1;
	  break;
	}			/* switch */
    }				/* while */

  /* finish off last line if need be */
  if( line != NULL )
    {
      line[charctr] = '\0';
      line = NULL;
    }

  /* fix first line */
  line = malloc((MAXLINELEN+64)*sizeof(char));
  if( line == NULL )
    die("malloc failed");

  sprintf(line, "[%s%s]", brief ? "" : "Message from ", lines[0]);

  free(lines[0]);
  lines[0] = line;
  line = NULL;

  /* free chaos connection */
  {
    char randombuf[1024];
    read(0, randombuf, 1024);

    close(0);
  }

  /* output to spool directory file */
  if (brief == FALSE &&
      ((ofd = open(sprintf(of, "%s/%s", SPOOL, him), 1)) >= 0 || 
       (ofd = creat(of, 0622)) >= 0))
    {
      if ((pwd = getpwnam(him)) == NULL ||
	  chown(of, pwd->pw_uid, pwd->pw_gid) < 0)
	chmod(of, 0666);
      lseek(ofd, 0L, 2);
      for(linectr = 0; lines[linectr] != NULL; linectr++)
	{
	  write(ofd, lines[linectr], strlen(lines[linectr]));
	  write(ofd, "\n", 1);
	}
      write(ofd,"\004",1);
    }
  
  sprintf(display, "unix:%c", histty[9]);

#ifdef XWIND
  if( strncmp(histty, "/dev/ttyv", 9) != 0 ||
      Xnotify(him, display, lines, brief) != 0 )
    {
#endif
      /* if not X or X lost */
      printf("\7\n\r");
      for(linectr = 0; lines[linectr] != NULL; linectr++)
	{
	  write(fileno(stdout), lines[linectr], strlen(lines[linectr]));
	  write(fileno(stdout), "\n\r", 2);
	}

#ifdef XWIND
    }
#endif

  exit(0);
 perm:
  chreject(0, "Permission denied on his tty.");
  exit(1);
}

timout()
{

	chreject(0, "Timeout opening their tty.");
	exit(1);
}

lcase(s)
register char *s;
{
	while (*s)
		if (isupper(*s))
			*s++ = tolower(*s);
		else
			s++;
}

/* following ripped off shamelessly from X/comsat/comsat.c */

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */


#ifdef XWIND
#include <X/Xlib.h>
/*char *indexs(), *index();*/
#endif

/*
 * comsat
 */
extern	errno;

#ifdef XWIND

short cursor[] = {0x0000, 0x7ffe, 0x4fc2, 0x4ffe, 0x7ffe,
		  0x7ffe, 0x781e, 0x7ffe , 0x7ffe, 0x0000};


extern exit();

/*
  name: username
  dname: display name
  notice: array of lines (no newlines on them!)
  brief: 1 if should use sendbrief defaults
*/
Xnotify (name, dname, notice, brief)
     char *name;
     char *dname;
     char **notice;
     bool brief;
{
  int i, j;			/* random iteration variables */
  struct passwd *pwent;
  char *envbuf[2];
  char homebuf[280];
  Display *dpy;
  WindowInfo winfo;
  FontInfo finfo;
  Font font;
  int width, height;
  Window w;
  XEvent rep;
  int timeout = 0;
  int reverse = 0;
  int bwidth = 2;
  int inner = 2;
  int vertical = 2;
  int horizontal = -2;
  int volume = 0;
  int forepix = BlackPixel;
  int backpix = WhitePixel;
  int brdrpix = BlackPixel;
  int mouspix = BlackPixel;
  extern char **environ;
  char *option;
  char *font_name = "8x13";
  char *fore_color = NULL;
  char *back_color = NULL;
  char *brdr_color = NULL;
  char *mous_color = NULL;
  Color cdef;
  char *myname = "send";

  if( brief == TRUE )
    myname = "sendbrief";
  
  if (pwent = getpwnam(name)) {
    strcpy(homebuf, "HOME=");
    strcat(homebuf, pwent->pw_dir);
    envbuf[0] = homebuf;
    envbuf[1] = NULL;
    environ = envbuf;
    if (option = XGetDefault(myname, "Display")) dname = option;
    if (!XOpenDisplay(dname)) return(-1);
    if (option = XGetDefault(myname, "BodyFont"))
      font_name = option;
    fore_color = XGetDefault(myname, "Foreground");
    back_color = XGetDefault(myname, "Background");
    brdr_color = XGetDefault(myname, "Border");
    mous_color = XGetDefault(myname, "Mouse");
    if (option = XGetDefault(myname, "BorderWidth"))
      bwidth = atoi(option);
    if (option = XGetDefault(myname, "InternalBorder"))
      inner = atoi(option);
    if (option = XGetDefault(myname, "Timeout"))
      timeout = atoi(option);
    if (option = XGetDefault(myname, "Volume"))
      volume = atoi(option);
    if (option = XGetDefault(myname, "HOffset"))
      horizontal = atoi(option);
    if (option = XGetDefault(myname, "VOffset"))
      vertical = atoi(option);
    if ((option = XGetDefault(myname, "ReverseVideo")) &&
	strcmp(option, "on") == 0)
      reverse = 1;
  }
  else {
    if (!XOpenDisplay(dname)) return(-1);
  }
  
  if (reverse) {
    brdrpix = backpix;
    backpix = forepix;
    forepix = brdrpix;
    mouspix = forepix;
  }
  
  if ((font = XGetFont(font_name)) == NULL)
    return(-1);
  if (DisplayCells() > 2)
    {
      if (back_color && XParseColor(back_color, &cdef) &&
	  XGetHardwareColor(&cdef))
	backpix = cdef.pixel;
      if (fore_color && XParseColor(fore_color, &cdef) &&
	  XGetHardwareColor(&cdef))
	forepix = cdef.pixel;
      if (brdr_color && XParseColor(brdr_color, &cdef) &&
	  XGetHardwareColor(&cdef))
	brdrpix = cdef.pixel;
      if (mous_color && XParseColor(mous_color, &cdef) &&
	  XGetHardwareColor(&cdef))
	mouspix = cdef.pixel;
    }
  XQueryFont(font, &finfo);
  XQueryWindow (RootWindow, &winfo);
  
  /* get max width */
  for( i = 0, width = 0; notice[i] != NULL; i++ )
    {
      if( (j = XQueryWidth (notice[i], font)) > width )
	width = j;
    }
  width += (inner << 1);
  height = finfo.height * i + (inner << 1);
  
  if (vertical < 0)
    vertical += winfo.height - height - (bwidth << 1);
  if (horizontal < 0)
    horizontal += winfo.width - width - (bwidth << 1);
  
  w = XCreateWindow(RootWindow, horizontal,
		    vertical, width, height, bwidth,
		    XMakeTile(brdrpix), XMakeTile(backpix));
  XStoreName(w, notice[0]);
  XSelectInput(w, ButtonPressed|ButtonReleased|ExposeWindow);
  XDefineCursor(w, XCreateCursor(16, 10, cursor, NULL, 7, 5,
				 mouspix, backpix, GXcopy));
  XMapWindow(w);
  XFeep(volume);
  if (timeout > 0) {
    signal(SIGALRM, exit);
    alarm(timeout * 60);
  }
  if (inner) inner--;
  while (1)
    {
      for(i = 0; notice[i] != NULL; i++)
	XText(w, inner, inner + i*finfo.height, notice[i], strlen(notice[i]),
	      font, forepix, backpix);
      XNextEvent(&rep);
      if (rep.type == ButtonPressed)
	return(0);
    }
}
#endif

die(s)
char *s;
{
  exit(-1);
}
