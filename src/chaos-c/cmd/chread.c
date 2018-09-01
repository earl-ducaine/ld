#include <sys/chaos.h>
#include <hosttab.h>
#include <signal.h>
#include <stdio.h>
#include <ctype.h>

#ifdef linux
#include <bsd/sgtty.h>
#else
#include <sgtty.h>
#endif

int conn;
FILE *stream;
struct host_entry *host, *lookup();

int argc;
char **argv;

timeout()
{
  printf("timeout\n");
  fflush(stdout);
}

badusage()
{
  printf("usage: %s <host/hostnum> <socket>\n");
  exit(1);
}

main(ac,av)
char **av;
{
  int n, addr, argnum=1;
  char *hostname=NULL, *cname=NULL, contact[1000];
  struct chstatus chstat;
  static char junkbuf[CHMAXPKT];

  argc = ac, argv = av;		/* set up the global arg info */
  contact[0] = '\0';		/* initialize contact */
  setlinebuf(stdout);

  /* if program name is a host, use that */
  if(lookup(argv[0])) hostname = argv[0];
  while(argnum<argc && *argv[argnum]=='-')	/* switches 	*/
    {
    }
  /* if no hostname yet, grab the next arg */
  if(hostname==NULL)
    {
      if(argnum==argc) badusage();
      hostname = argv[argnum++];
    }
  if(cname==NULL)
    {
      char *index();
      
      if(argnum==argc) badusage();
      cname = argv[argnum];
      /* if there are spaces in the cname, split it into "cname\0contact\0" */
      if(index(' ', argv[argnum]))
	{
	  char *ptr=index(' ', argv[argnum]);
	  strcpy(contact, 1+ptr);
	  *ptr = '\0';
	}
      argnum++;
    }
  if(argnum<argc) strcat(contact, argv[argnum++]);
  while(argnum<argc)
    {
      strcat(contact, " ");
      strcat(contact, argv[argnum++]);
    }
  
  if(isdigit(*hostname))
    {
      addr = chaos_addr(hostname, 0);
    }
  else
    {
      if((host=lookup(hostname))==NULL)
	{
	  printf("can\'t find host %s\n", hostname);
	  exit(1);
	}
      
      addr = chaos_addr(host->host_name, 0);
      if(addr==0)
	{
	  printf("host %s is not on the chaos net\n", host->host_name);
	  exit(2);
	}
    }
  fprintf(stderr, "[%s]\n", hostname);
  fflush(stdout);
  if ((conn=chopen(addr, cname, 2, 1, contact[0]?contact:0, 0, 0)) < 0)
    {
      perror("chopen");
      printf("ncp error -- cannot open %s(%s %s)\n",
	     hostname, cname, contact);
      exit(2);
    }
  signal(SIGALRM, timeout);
  alarm(5);
  ioctl(conn, CHIOCSWAIT, CSRFCSENT);
  signal(SIGALRM, SIG_IGN);
  ioctl(conn, CHIOCGSTAT, &chstat);
  if (chstat.st_state == CSRFCSENT)
    {
    nogood:
      printf("host %s not responding (%s)\n",
	     hostname,contact);
      close(conn);
      conn = -1;
      exit(1);
    }
  if (chstat.st_state != CSOPEN)
    {
      if (chstat.st_state == CSCLOSED)
	{
	  printf(" Refused\n");
	  if ((chstat.st_plength) &&
	      ((chstat.st_ptype == ANSOP) ||
	       (chstat.st_ptype == CLSOP)))
	    {
	      ioctl(conn, CHIOCPREAD, junkbuf);
	      printf("%s\n", junkbuf);
	    }
	  close(conn);
	  conn = -1;
	  exit(1);
	}
      printf("host %s st_state %d\n",
	     hostname,chstat.st_state);
      close(conn);
      conn = -1;
      exit(1);
    }
  stream = fdopen(conn, "r");
  {
    int chr, wasnl=1, neednl=0;

    /* if there is a \r with no neighboring \n, then add a \n */
    while((chr=getc(stream))!=EOF)
      {
	if(chr==CHNL) putchar('\n'), wasnl=1, neednl=0;
	else if(chr=='\n') putchar('\n'), wasnl=1, neednl=0;
	else if(chr=='\r')
	  {
	    putchar('\r');
	    if(!wasnl) neednl=1;
	  }
	else
	  {
	    if(neednl) putchar('\n'), neednl=0;
	    putchar(chr), wasnl=0;
	  }
      }
  }
}
struct host_entry *
lookup(name) char* name;
{
	struct host_entry *host, *host_info();
	
	if ((host = host_info(name)) == 0)
	{
		return(NULL);
	}
	return(host);
}
