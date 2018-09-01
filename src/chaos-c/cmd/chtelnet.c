/*
 *	User mode telnet program.
 *	Usable on the Chaosnet (or the Arpanet via a Chaos/Arpa gateway),
 *	although it should be easy to change for any other protocol supporting
 *	reliable bytestreams.
 *	Author: T. J. Teixeira <TJT@MIT-XX>
 *
 * 12/29/81 tony -- Modified to print refusal and message, if any.
 *		    Also, send CLS packet before closing connection.
 *
 * 1/4/82 tjt -- use CHRFCADEV instead of /dev/chaos
 *
 * 1/4/82 tjt -- added verbose & brief options
 *
 * 2/9/82 tjt -- incorporated Dove's rdcnt changes
 */

#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <signal.h>

#ifdef linux
#include <bsd/sgtty.h>
#else
#include <sgtty.h>
#endif

#include <sys/types.h>
#include <sys/chaos.h>
#include <hosttab.h>

#define ctrl(x) ((x)&037)

/*
 * telnet protocol command characters
 */
#define SE	240
#define NOP	241
#define DMARK	242
#define BRK	243
#define IP	244
#define AO	245
#define AYT	246
#define EC	247
#define EL	248
#define GA	249
#define SB	250
#define WILL	251
#define WONT	252
#define DO	253
#define DONT	254
#define IAC	255

/*
 * telnet option codes
 */
#define TN_TRANSMIT_BINARY 0
#define TN_ECHO 1
#define TN_RCP 2
#define TN_SUPPRESS_GO_AHEAD 3
#define TN_NAME 4
#define TN_STATUS 5
#define TN_TIMING_MARK 6
#define TN_RCTE 7
#define TN_NAOL 8
#define TN_NAOP 9
#define TN_NAOCRD 10
#define TN_NAOHTS 11
#define TN_NAHTD 12
#define TN_NAOFFD 13
#define TN_NAOVTS 14
#define TN_NAVTD 15
#define TN_NAOLFD 16
#define TN_EXTEND_ASCII 17
#define TN_EXOPL 255

char *optnames[] = {
	"TRANSMIT BINARY",
	"ECHO",
	"RCP",
	"SUPPRESS GO AHEAD",
	"NAME",
	"STATUS",
	"TIMING MARK",
	"RCTE",
	"NAOL",
	"NAOP",
	"NAOCRD",
	"NAOHTS",
	"NAHTD",
	"NAOFFD",
	"NAOVTS",
	"NAVTD",
	"NAOLFD",
	"EXTEND ASCII",
	"EXOPL",
};

char *optcmd[] = {
	"SE",
	"NOP",
	"DMARK",
	"BRK",
	"IP",
	"AO",
	"AYT",
	"EC",
	"EL",
	"GA",
	"SB",
	"WILL",
	"WONT",
	"DO",
	"DONT",
	"IAC",
};

char hisopts[256], myopts[256];

struct host_entry *host;
char contact[80];

int conn = -1;	/* connection */
int fin = -1;	/* input from other process */
int fout = -1;	/* output to other process */
int pid;	/* process id of write process */
int fk;		/* process id of read process */
int isopen;	/* connection open */
int echomode;
int goahead = 1;
int verbose = 0;
int monitor = 0;
short rdcnt = 0;		/* 2/8/82 dove */

/*
 * commands between processes
 */
#define IPC_MODE 1
#define IPC_CLOSED 2
#define IPC_GA 3
#define IPC_DUMMY 4
#define IPC_VERBOSE 5
#define IPC_XMIT_BIN 6
#define IPC_MONITOR 7

char *prompt;

char escape = ctrl('^');
char tkill, terase;

int connect();
int quit();
int pausecmd();
int bye();
int help();
int setescape();
int status();
int abrtout();
int attn();
int brkkey();
int wordy();
int monit();
int brief();

#define HELPINDENT (sizeof("disconnect"))

struct cmd {
	char *name;
	int (*handler)();
};

struct cmd maintab[] = {
	"open",		connect,
	"connect",	connect,
	"exit",		quit,
	"quit",		quit,
	"pause",	pausecmd,
	"escape",	setescape,
	"status",	status,
	"verbose",	wordy,
	"brief",	brief,
	"monitor",	monit,
	"help",		help,
	"?",		help,
	0
};

struct cmd talktab[] = {	/* legal in talk mode */
	"bye",		bye,
	"disconnect",	bye,
	"exit",		quit,
	"quit",		quit,
	"pause",	pausecmd,
	"escape",	setescape,
	"abort",	abrtout,
	"attn",		attn,
	"break",	brkkey,
	"status",	status,
	"verbose",	wordy,
	"monitor",	monit,
	"brief",	brief,
	"help",		help,
	"?",		help,
	0
};

struct cmd *cmdtab = maintab;

int margc;
char *margv[20];
char line[132];

/*
 * construct a control character sequence for a special character
 */
char *control(c)
register int c;
{
	static char buf[3];
	if (c == 0177)
		return("^?");
	if (c >= 040) {
		buf[0] = c;
		buf[1] = 0;
	} else {
		buf[0] = '^';
		buf[1] = '@'+c;
		buf[2] = 0;
	}
	return(buf);
}

struct cmd *getcmd(name)
register char *name;
{
	register char *p, *q;
	register struct cmd *c, *found;
	register int nmatches, longest;
	longest = 0;
	nmatches = 0;
	found = 0;
	for (c = cmdtab; p = c->name; c++) {
		for (q = name; *q == *p++; q++)
			if (*q == 0)	/* exact match? */
				return(c);
		if (!*q) {	/* the name was a prefix */
			if (q - name > longest) {
				longest = q - name;
				nmatches = 1;
				found = c;
			} else if (q - name == longest)
				nmatches++;
		}
	}
	if (nmatches > 1)
		return ((struct cmd *)-1);
	return (found);
}

main(argc, argv)
char *argv[];
{
	register struct cmd *c;
	prompt = argv[0];
	pid = getpid();
	if (argc >= 2)
		return(connect(argc, argv));
	while (1) {
		printf("%s>", prompt);
		fflush(stdout);
		if (gets(line) == NULL)
			break;
		if (line[0] == 0)
			continue;
		makeargv();
		c = getcmd(margv[0]);
		if (c == (struct cmd *)-1)
			printf("?Ambiguous command\n");
		else if (c == (struct cmd *)0)
			printf("?Invalid command\n");
		else
			(*c->handler)(margc, margv);
	}
	putchar('\n');
	return(0);
}

connect(argc, argv)
char *argv[];
{
	register int c;
#ifndef TERMIOS
	struct sgttyb stbuf;
#endif
	struct chstatus chstat;
	static char junkbuf[CHMAXPKT];
	char *via = "mit-mc";
	char *name;
	char *cname = NULL;
	int net = 0;	/* 0 => unspec, 1 => chaos, 2 => arpa */
	int ipc(), timeout();
	int tord[2], towr[2];

	if (argc <= 0) {	/* give help */
		printf("Connect to the given site.\n");
		return(0);
	}
	if (conn >= 0) {
		printf("?Already connected to %s!\n", host->host_name);
		return(1);
	}
	while (argc < 2) {
		printf("(to) ");
		fflush(stdout);
		strcpy(line, "connect ");
		gets(&line[strlen(name)]);
		makeargv();
		argc = margc;
		argv = margv;
	}
	argc--;	/* skip the zeroth arg */
	argv++;
	if (argc > 1) {
		if (strcmp(argv[0], "chaosnet")==0) {
			net = 1;
			argv++;
			argc--;
		} else if (strcmp(argv[0], "arpanet")==0) {
			net = 2;
			argv++;
			argc--;
		}
	}
	name = *argv++;
	argc--;
	if (argc > 0) {
		cname = *argv++;
		argc--;
	}
	if (argc > 0) {
		via = *argv++;
		argc--;
	}
	if ((host = host_info(name)) == 0) {
		printf("?No such host %s\n", name);
		return(1);
	}
	if (pipe(tord) < 0) {
		printf("?can't open pipe to read process\n");
		return(1);
	}
	if (pipe(towr) < 0) {
		printf("?can't open pipe to write process\n");
		close(tord[0]);
		close(towr[0]);
		return(1);
	}
	contact[0] = '\0';
	if (net != 2) {
		c = chaos_addr(host->host_name);
		if (c == 0) {
			if (net == 1) {
				printf("host %s is not on the chaos net\n",
					host->host_name);
		lose1:		close(tord[0]);
				close(tord[1]);
				close(towr[0]);
				close(towr[1]);
				return(1);
			}
			goto arpa;
		} else
			if (!cname)
				cname = "TELNET";
	} else {
	arpa:
		c = chaos_addr(via);
		if (c == 0) {
			printf("routing host %s unknown\n", via);
			goto lose1;
		}
		sprintf(contact, "%s %s",
			host->host_name, cname ? cname : "27");
		cname = "TCP";
		fixhostname(contact);
	}
	printf("Trying...");
	fflush(stdout);
	if ((conn = chopen(c, cname, 2, 1, contact[0] ? contact : 0, 0, 0)) < 0) {
		printf("ncp error -- cannot open %s\n", contact);
		return(1);
	}
	signal(SIGALRM, timeout);
	alarm(15);
	ioctl(conn, CHIOCSWAIT, CSRFCSENT);
	signal(SIGALRM, SIG_IGN);
	ioctl(conn, CHIOCGSTAT, &chstat);
	if (chstat.st_state == CSRFCSENT)
		{
nogood:
		printf("host %s not responding (%s)\n",host->host_name,contact);
		close(conn);
		conn = -1;
		return(1);
		}
	if (chstat.st_state != CSOPEN)
		{
		if (chstat.st_state == CSCLOSED)
			{
			printf(" Refused\n");
			if ((chstat.st_plength)
			    && ((chstat.st_ptype == ANSOP) || (chstat.st_ptype == CLSOP)))
				{
				ioctl(conn, CHIOCPREAD, junkbuf);
				printf("%s\n", junkbuf);
				}
			close(conn);
			conn = -1;
			return(1);
			}
		else goto nogood;
		}
	printf(" Open\n");
	fflush(stdout);
	mode(3);
	ioctl(conn, CHIOCSMODE, CHRECORD);
	isopen = 1;
#ifdef SIGEMT
	signal(SIGEMT, ipc);	/* ignore them for now */
#endif
	fk = fork();
	if (fk == 0) {
		fin = tord[0];
		fout = towr[1];
		close(tord[1]);
		close(towr[0]);
#ifdef SIGEMT
		signal(SIGEMT, ipc);	/* ignore them for now */
#endif
		wr_ipc(IPC_DUMMY);
		rd();
		fprintf(stderr, "Connection closed by foreign host\r\n");
		wr_ipc(IPC_CLOSED);
#ifdef SIGEMT
		kill(pid, SIGEMT);
#endif
		exit(3);
	}
	fin = towr[0];
	fout = tord[1];
	close(towr[1]);
	close(tord[0]);
	ipc();	/* reader always sends at least one */
	wr();
	call(bye, "bye", 0);
	return(0);
}

/*
 * print status about the connection
 */
status(argc, argv)
int argc;
char *argv[];
{
	if (argc <= 0) {
		printf("Print status information.\n");
		return(0);
	}
	if (conn >= 0) {
		printf("Connected to %s.\r\n", host->host_name);
		wrconn(IAC);
		wrconn(AYT);
		synch();
	} else
		printf("No connection.\r\n");
	printf("escape character is '%s'\r\n", control(escape));
	if (verbose)
		printf("verbose\r\n");
	if (monitor)
		printf("monitor\r\n");
	return(0);
}

/*
 * be verbose
 * e.g. print all option negotiations
 */
wordy(argc, argv)
int argc;
char *argv[];
{
	if (argc <= 0) {
		printf("Print information about option negotiations.\n");
		return(0);
	}
	verbose = 1;
	wr_ipc(IPC_VERBOSE);
	wr_ipc(verbose);
#ifdef SIGEMT
	if (fk)
		kill(fk, SIGEMT);
#endif
	return(0);
}
monit(argc, argv)
int argc;
char *argv[];
{
	if (argc <= 0) {
		printf("Show non-printing characters from the net.\n");
		return(0);
	}
	monitor = !monitor;
	wr_ipc(IPC_MONITOR);
	wr_ipc(monitor);
#ifdef SIGEMT
	if (fk)
		kill(fk, SIGEMT);
#endif
	return(0);
}
/*
 * be brief
 * e.g. don't print all option negotiations
 */
brief(argc, argv)
int argc;
char *argv[];
{
	if (argc <= 0) {
		printf("Do not print information about option negotiations.\n");
		return(0);
	}
	verbose = 0;
	wr_ipc(IPC_VERBOSE);
	wr_ipc(verbose);
#ifdef SIGEMT
	if (fk)
		kill(fk, SIGEMT);
#endif
	return(0);
}

/*
 * send the NVT ATTN (interrupt process) sequence
 */
attn(argc, argv)
int argc;
char *argv[];
{
	if (argc <= 0) {
		printf("Send the NVT interrupt process sequence.\n");
		return(0);
	}
	if (conn >= 0) {
		wrconn(IAC);
		wrconn(IP);
		synch();
	}
	return(0);
}

/*
 * send the NVT ABORT sequence
 */
abrtout(argc, argv)
int argc;
char *argv[];
{
	if (argc <= 0) {
		printf("Send the NVT abort output sequence.\n");
		return(0);
	}
	if (conn >= 0) {
		wrconn(IAC);
		wrconn(AO);
		synch();
	}
	return(0);
}

/*
 * send the NVT BREAK key sequence
 */
brkkey(argc, argv)
int argc;
char *argv[];
{
	if (argc <= 0) {
		printf("Send the NVT BREAK key sequence.\n");
		return(0);
	}
	if (conn >= 0) {
		wrconn(IAC);
		wrconn(BRK);
		synch();
	}
	return(0);
}

makeargv() {
	register char *cp;
	register char **argp = margv;

	margc = 0;
	for (cp = line; *cp;) {
		while (isspace(*cp))
			cp++;
		if (*cp == '\0')
			break;
		*argp++ = cp;
		margc += 1;
		while (*cp != '\0' && !isspace(*cp))
			cp++;
		if (*cp == '\0')
			break;
		*cp++ = '\0';	/* Null terminate that string */
	}
	*argp++ = NULL;
}

pausecmd(argc, argv)
int argc;
char *argv[];
{
	register int save;
	if (argc <= 0) {	/* give help */
		printf("Suspend job\r\n");
		return(0);
	}
	save = mode(0);
#ifdef	SIGTSTP
	kill(0, SIGTSTP);	/* get whole process group */
#endif
	mode(save);
	return(0);
}

bye(argc, argv)
int argc;
char *argv[];
{
	register int c;
	struct chpacket clspkt;

	if (argc <= 0) {	/* give help */
		printf("Disconnect from the current site.\n");
		return(0);
	}
	mode(0);
	if (conn >= 0) {
		ioctl(conn, CHIOCOWAIT, 1);
		if (fk) {
			kill(fk, SIGKILL);
			wait((int *)NULL);
		}
		clspkt.cp_op = CLSOP;
		write(conn, &clspkt, 1);
		close(conn);
		if (fin > 0) {
			close(fin);
			close(fout);
		}
		conn = -1;
		fin = -1;
		fout = -1;
		if (isopen)
			printf("Connection closed\n");
		isopen = 0;
		fk = 0;
	}
	return(0);
}

quit(argc, argv)
int argc;
char *argv[];
{
	if (argc <= 0) {	/* give help */
		printf("leave %s, closing any connection\n", prompt);
		return(0);
	}
	call(bye, "bye", 0);
	exit(0);
}

/*
 * help command -- call each command handler with argc == 0
 * and argv[0] == name
 */
help(argc, argv)
int argc;
char *argv[];
{
	register struct cmd *c;
	char *dargv[2];	/* can't use call because of screwy argc required */
	if (argc <= 0) {	/* give help! */
		printf("print help information\n");
		return(0);
	}
	dargv[1] = 0;
	if (argc == 1) {
		printf("Commands may be abbreviated.  Commands are:\n\n");
		for (c = cmdtab; c->name; c++) {
			printf("%-*s", HELPINDENT, c->name);
			dargv[0] = c->name;
			(*c->handler)(0, dargv);
		}
	} else {
		while (--argc > 0) {
			register char *arg;
			arg = *++argv;
			dargv[0] = arg;
			c = getcmd(arg);
			if (c == (struct cmd *)-1)
				printf("?Ambiguous help command %s\n", arg);
			else if (c == (struct cmd *)0)
				printf("?Invalid help command %s\n", arg);
			else
				(*c->handler)(-1, dargv);
		}
	}
	return(0);
}

/*
 * call routine with argc, argv set from args (terminated by 0).
 * VARARGS1
 */
call(routine, args)
int (*routine)();
int args;
{
	register int *argp;
	register int argc;
	for (argc = 0, argp = &args; *argp++ != 0; argc++);
	return((*routine)(argc,&args));
}

/*
 * read from the IPC pipe
 */
rd_ipc()
{
	long int waiting;
	char c;
	if (ioctl(fin, FIONREAD, &waiting) < 0
	 || waiting <= 0
	 || read(fin, &c, 1) != 1)
		return(EOF);
	return(c&0377);
}

/*
 * write to the IPC pipe
 */
wr_ipc(c)
char c;
{
	write(fout, &c, 1);
}

/*
 * handle IPC via the chaos connection between processes
 */
ipc()
{
	register int c;
#ifndef BSD42
	signal(SIGEMT, 1);
#endif
	while ((c = rd_ipc()) != EOF) {
		switch(c) {
		case IPC_MODE:
			mode(rd_ipc());
			break;
	
		case IPC_CLOSED:
			isopen = 0;
			call(bye,"bye",0);
			exit(0);
			break;
	
		case IPC_GA:
			goahead = rd_ipc();
			break;
	
		case IPC_DUMMY:
			break;

		case IPC_VERBOSE:
			verbose = rd_ipc();
			break;

		case IPC_XMIT_BIN:
			myopts[TN_TRANSMIT_BINARY] = rd_ipc();
			break;

		case IPC_MONITOR:
			monitor = rd_ipc();
			break;
		}
	}
#ifndef BSD42
	signal(SIGEMT, ipc);
#endif
}

mode(f)
register int f;
{
	register int old;
#ifndef TERMIOS
	struct sgttyb stbuf;
#endif
	if (fk == 0 && fout >= 0) {	/* child process */
		wr_ipc(IPC_MODE);
		wr_ipc(f);
#ifdef SIGEMT
		kill(pid, SIGEMT);
#endif
		return;
	}
#ifndef TERMIOS
	ioctl(fileno(stdin), TIOCGETP, &stbuf);
	tkill = stbuf.sg_kill;
	terase = stbuf.sg_erase;
	old = echomode;
	echomode = f;
	switch (f) {
	case 0:
		stbuf.sg_flags &= ~RAW;
		stbuf.sg_flags |= ECHO|CRMOD;
		break;

	case 1:
		stbuf.sg_flags |= RAW;
		stbuf.sg_flags &= ~(ECHO|CRMOD);
		break;

	case 2:
		stbuf.sg_flags &= ~RAW;
		stbuf.sg_flags &= ~(ECHO|CRMOD);
		break;

	case 3:
		stbuf.sg_flags |= RAW;
		stbuf.sg_flags |= ECHO|CRMOD;
	}
	ioctl(fileno(stdin), TIOCSETN, &stbuf);
#endif
	return(old);
}

/*
 * read from the user's tty and write to the network connection
 */
wr()
{
	register int c;
	register int escaped = 0;
	off_t waiting;
	extern int errno;

	opt(DO, TN_ECHO);
	opt(DO, TN_SUPPRESS_GO_AHEAD);
	opt(WILL, TN_SUPPRESS_GO_AHEAD);
	chflush(DATOP);

	while (isopen) {
		if ((c = getchar()) == EOF)
			if (errno == EINTR)
				continue;
			else
				break;
		if (!myopts[TN_TRANSMIT_BINARY])
			c &= 0177;	/* strip the parity bit */
		if (escaped) {
			escaped = 0;
			if (c != escape)
				if (doescape(c))
					return;
				else
					continue;
		}
		if (c == IAC)
			wrconn(c);
		else if (c == escape)	
			escaped = 1;
		else {
			wrconn(c);
			if (c == '\r')
				wrconn('\n');
		}
		if (ioctl(fileno(stdin), FIONREAD, &waiting) >= 0 &&
		    waiting == 0)
			chflush(DATOP);
	}
}
/*
 * Perform an escape command.
 */
doescape(c)
{
	register int retval = 0;

#ifdef	SIGSTOP
	kill(fk, SIGSTOP);
#endif
	switch(c) {
	case '?':
		printf("\r\n");
		printf("?\tLists the command characters\r\n");
		printf("a\tEquivalent to the attn command\r\n");
		printf("b\tEquivalent to the break command\r\n");
		printf("c\tEquivalent to the bye command\r\n");
		printf("o\tEquivalent to the abort command\r\n");
		printf("m\tEquivalent to the monitor command\r\n");
		printf("p\tEquivalent to the pause command\r\n");
		printf("q\tEquivalent to the exit command\r\n");
		printf("s\tEquivalent to the status command\r\n");
		printf("x\tEnter extended command mode\r\n");
		break;
	case 'd': case 'D':
#ifdef	SIGCONT
		kill(fk, SIGCONT);
#endif
		kill(fk, SIGIOT);
		mode(0);
		exit(1);
	case 'A': case 'a':
		call(attn, "attn", 0);
		break;
	case 'B': case 'b':
		call(brkkey, "break", 0);
		break;
	case 'C': case 'c':
		retval = 1;
		break;
	case 'M': case 'm':
		call(monit, "monitor", 0);
		break;
	case 'O': case 'o':
		call(abrtout, "abort", 0);
		break;
	case 'p': case 'P':
		call(pausecmd, "pause", 0);
		break;
	case 'q': case 'Q':
		call(quit, "exit", 0);
		break;
	case 's': case 'S':
		call(status, "status", 0);
		break;
	case 'x': case 'X':
		retval = extend();
		break;
	default:
		printf("?Invalid escape character %o\r\n", c);
		break;
	}
#ifdef	SIGCONT
	kill(fk, SIGCONT);
#endif
	return retval;
}
extend()
{
	register struct cmd *c;
	struct cmd *oldcmd;
	int oldmode;
	oldmode = mode(0);
	oldcmd = cmdtab;
	cmdtab = talktab;
	putchar('\n');
	do {
		printf("%s>", prompt);
		fflush(stdout);
		if (gets(line) == NULL)
			break;
		if (line[0] == 0)
			break;
		makeargv();
		c = getcmd(margv[0]);
		if (c == (struct cmd *)-1)
			printf("?Ambiguous command\n");
		else if (c == (struct cmd *)0)
			printf("?Invalid command\n");
		else
			(*c->handler)(margc, margv);
	} while (c->handler == help);
	cmdtab = oldcmd;
	mode(oldmode);
	putchar('\n');
	return(c->handler == bye);
}

rd()
{
	register int c;
	long waiting;
	extern int errno;
	while (1) {
		if (rdcnt <= 0
		 && ioctl(conn, FIONREAD, &waiting) == 0
		 && waiting == 0) {
			fflush(stdout);
			if (goahead) {
				wrconn(IAC);
				wrconn(GA);
			}
		}
		if ((c = rdconn()) == EOF) {
			if (errno == EINTR)
				continue;
			break;
		}
		if (c == IAC)
			option();
		else {
			if (monitor && (c < ' ' || c == 0177))
				printf("\\%03o", c);
			else {
				if (c == '\n')
					putchar('\r');
				putchar(c);
			}
		}
	}
}

option()
{
	register int code;
	register int c;
	
	if ((c = rdconn()) == EOF)
		return;
	switch(code = c) {
	case WILL:
	case WONT:
	case DO:
	case DONT:
		c = rdconn();
		if (verbose)
			badopt(code, c);
		switch(code) {
		case WILL:
			if (hisopts[c])	/* already active */
				break;
			if (c == TN_ECHO) {
				mode(1);
				hisopts[c] = 1;
				opt(DO, c);
			} else if (c == TN_SUPPRESS_GO_AHEAD) {
				hisopts[c] = 1;
				opt(DO, c);
			} else if (c == TN_TIMING_MARK) {
				opt(DONT, c);
			} else if (c == TN_TRANSMIT_BINARY) {
				hisopts[c] = 1;
				opt(DO, c);	/* why not? */
			} else {
				if (verbose)
					fprintf(stderr, "?Refused\r\n");
				else
					badopt(code, c);
				opt(DONT, c);
			}
			break;

		case WONT:
			if (!hisopts[c])	/* already inactive */
				break;
			if (c == TN_ECHO) {
				mode(3);
				opt(DONT, c);
				hisopts[c] = 0;
			} else if (c == TN_SUPPRESS_GO_AHEAD) {
				opt(DONT, c);
				hisopts[c] = 0;
			} else if (c == TN_TRANSMIT_BINARY) {
				opt(DONT, c);
				hisopts[c] = 0;
			} else {
				if (verbose)
					fprintf(stderr, "?Refused\r\n");
				else
					badopt(code, c);
				opt(DONT, c);
			}
			break;

		case DO:
			if (myopts[c])	/* already active */
				break;
			if (c == TN_TIMING_MARK) {
				opt(WONT, c);
			} else if (c == TN_SUPPRESS_GO_AHEAD) {
				wr_ipc(IPC_GA);
				wr_ipc(0);
#ifdef SIGEMT
				kill(pid, SIGEMT);
#endif

				opt(WILL, c);
				myopts[TN_SUPPRESS_GO_AHEAD] = 1;
			} else if (c == TN_TRANSMIT_BINARY) {
				wr_ipc(IPC_XMIT_BIN);
				wr_ipc(1);
#ifdef SIGEMT
				kill(pid, SIGEMT);
#endif

				opt(WILL, c);
				myopts[TN_TRANSMIT_BINARY] = 1;
			} else {
				if (verbose)
					fprintf(stderr, "?Refused\r\n");
				else
					badopt(code, c);
				opt(WONT, c);
			}
			break;

		case DONT:
			if (!myopts[c]) /* already inactive */
				break;
			myopts[c] = 0;
			if (c == TN_TIMING_MARK) {
				opt(WONT, c);
			} else if (c == TN_SUPPRESS_GO_AHEAD) {
				wr_ipc(IPC_GA);
				wr_ipc(1);
#ifdef SIGEMT
				kill(pid, SIGEMT);
#endif

				myopts[TN_SUPPRESS_GO_AHEAD] = 0;
				opt(WONT, c);
			} else if (c == TN_TRANSMIT_BINARY) {
				wr_ipc(IPC_XMIT_BIN);
				wr_ipc(0);
#ifdef SIGEMT
				kill(pid, SIGEMT);
#endif
				myopts[TN_TRANSMIT_BINARY] = 0;
				opt(WONT, c);
			} else {
				if (verbose)
					fprintf(stderr, "?Refused\r\n");
				else
					badopt(code, c);
				opt(WONT, c);
			}
			break;
		}
		chflush(DATOP);
		break;

	case DMARK:
		fflush(stdout);	/* write the output */
#ifndef TERMIOS
		ioctl(fileno(stdout), TIOCFLUSH, 0);
#endif
		break;

	case AO:
		wrconn(IAC);
		wrconn(DMARK);
		chflush(DATOP);
		break;

	case NOP:
	case GA:
		break;
	default:
		fprintf(stderr, "?Received IAC %d\r\n", code);
	}
}

/*
 * send an option
 */
opt(option, value)
int option, value;
{
	if (verbose)
		fprintf(stderr, "?Sending  %s %s\r\n",
			optcmd[option-SE], optnames[value]);
	wrconn(IAC);
	wrconn(option);
	wrconn(value);
}

/*
 * print an error message for a bad option
 */
badopt(cmd, option)
{
	if (option < sizeof(optnames) / sizeof(optnames[0]))
		fprintf(stderr, "?Received %s %s\r\n",
			optcmd[cmd-SE], optnames[option]);
	else
		fprintf(stderr, "?Received %s option %d\r\n",
			optcmd[cmd-SE], option);
}

/*
 * set the escape character
 */
setescape(argc, argv)
int argc;
char *argv[];
{
	register char *arg;
	char buf[50];
	if (argc <= 0) {	/* give help */
		printf("Set escape character\n");
		return(0);
	}
	if (argc > 2)
		arg = argv[1];
	else {
		printf("new escape character: ");
		fflush(stdout);
		gets(buf);
		arg = buf;
	}
	if (arg[0] == '\'')
		escape = arg[1];
	else
		escape = oatoi(arg);
	return(0);
}

/*
 * convert a string of octal digits
 */
oatoi(s)
register char *s;
{
	register int i;
	i = 0;
	while ('0' <= *s && *s <= '7')
		i = i * 8 + *s++ - '0';
	return(i);
}

/*
 * convert the Arpanet host name to upper case
 */
fixhostname(s)
register char *s;
{
	while (*s++ != ' ');	/* skip chaos host spec */
	while (*s != ' ') {
		if (islower(*s))
			*s = toupper(*s);
		s++;
	}
}


/*
 * read a character from the network connection
 */
rdconn()
{
	char c;
	static struct chpacket pkt;
	static char *ptr = pkt.cp_data;
	if (rdcnt-- > 0)
		return(*ptr++&0377);
	if ((rdcnt = read(conn, &pkt, sizeof(pkt))) <= 0)
		return(EOF);
	switch (pkt.cp_op&0377) {
	case DATOP:
		break;

	case CLSOP:
		fprintf(stderr, "\r\n%.*s\n", rdcnt - 1, pkt.cp_data);
	case EOFOP:
		return(EOF);

	default:
		fprintf(stderr, "?packet op = %o len = %d\r\n",
			pkt.cp_op&0377, rdcnt);
		break;
	}		
	rdcnt--;	/* skip past the opcode */
	ptr = pkt.cp_data+1;
	rdcnt--;
	return(pkt.cp_data[0]&0377);
}

/*
 * save up data to be sent
 */

struct chpacket opkt;	/* current output packet */
char *optr = opkt.cp_data;
int ocnt = sizeof opkt.cp_data;

/*
 * write a character to the connection
 */
wrconn(c)
int c;
{
	*optr++ = c;
	if (--ocnt <= 0)
		chflush(DATOP);
}

/*
 * flush the connection in a packet with the specified opcode
 */
chflush(op)
int op;
{
	if (optr==opkt.cp_data)
		return;
	opkt.cp_op = op;
	write(conn, &opkt, optr-opkt.cp_data+1);
	optr = opkt.cp_data;
	ocnt = sizeof(opkt.cp_data);
}

/*
 * send a synch code
 */
synch()
{
	chflush(DATOP);
	wrconn(IAC);
	wrconn(DMARK);
	chflush(0201);
}

/*
 * null func for timeout
 */
timeout()
	{
	}
