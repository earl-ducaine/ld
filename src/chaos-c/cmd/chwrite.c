#include <sys/chaos.h>
#include <hosttab.h>
#include <signal.h>

#ifdef linux
#include <bsd/sgtty.h>
#else
#include <sgtty.h>
#endif

#include <stdio.h>
#include <ctype.h>

int conn;
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
  char *hostname=NULL, *cname=NULL, arglist[1000];
  struct chstatus chstat;
  static char junkbuf[CHMAXPKT];

  argc = ac, argv = av;		/* set up the global arg info */
  arglist[0] = '\0';		/* initialize arglist */
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
      argnum++;
    }
  /* terminate each arglist word with \212 */
  while(argnum<argc)
    {
      strcat(arglist, argv[argnum++]);
      strcat(arglist, "\212");
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
/* chopen(address, arglist, mode, async, data, dlength, rwsize) */
  if ((conn=chopen(addr, cname, 1, 1, arglist[0]?arglist:0, 0, 0)) < 0)
    {
      perror("chopen");
      printf("ncp error -- cannot open %s(%s %s)\n",
	     hostname, cname, arglist);
      exit(2);
    }
  signal(SIGALRM, timeout);
  alarm(20);
  ioctl(conn, CHIOCSWAIT, CSRFCSENT);
  signal(SIGALRM, SIG_IGN);
  ioctl(conn, CHIOCGSTAT, &chstat);
  if (chstat.st_state == CSRFCSENT)
    {
    nogood:
      printf("host %s not responding (%s %s)\n", hostname, cname, arglist);
      close(conn);
      conn = -1;
      exit(1);
    }
  if(chstat.st_state != CSOPEN)
    {
      if(chstat.st_state == CSCLOSED)
	{
	  printf(" Refused\n");
	  if((chstat.st_plength) &&
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
  /* raw i/o */
  {
    char bfr[8192];
    int input_result, output_result;

    while((input_result=read(0, bfr, sizeof(bfr)))>0)
      {
	if((output_result=write(conn, bfr, input_result))!=input_result)
	  {
	    perror("writing to connection");
	    exit(2);
	  }
      }
    if(input_result<0)
      {
	perror("reading standard input");
	exit(1);
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
