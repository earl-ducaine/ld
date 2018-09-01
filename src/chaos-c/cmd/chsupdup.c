/*
 *	User mode supdup program.
 *	Usable on the Chaosnet (or the Arpanet via a Chaos/Arpa gateway),
 *	although it should be easy to change for any other protocol supporting
 *	reliable bytestreams.
 *	Author: T. J. Teixeira <TJT@MIT-XX>
 *	Considerably hacked by Jim Kulp, Symbolics (jek@scrc-vixen)
 *	Some fixes by Web Dove, (dove@dspg)
 *
 * 1/20/84 dove
 *	change printf(seq) to tputs(seq) in case seq needs timing
 *	nice(-10)
 *
 * 1/21/84 jtw
 *	Stuff a longjmp in ipc() since 4.2 refuses to break the wr()
 *	getchar() when the close message comes over the pipe!
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

#include <hosttab.h>
#include <setjmp.h>
#include <sys/chaos.h>

#define ctrl(x) ((x)&037)

struct host_entry *host;
char contact[80];
char location[132];

int conn = -1;	/* connection */
FILE *fin;	/* input from other process */
FILE *fout;	/* output to other process */
int pid;	/* process id of write process */
int fk;		/* process id of read process */
int isopen;	/* connection open */
int rdcount;	/* Number of characters buffered on connection input */
int echomode;

/*
 * termcap variables
 */
char *tgetstr();
char PC;	/* padding character */
char *BC;	/* backspace if not ^H */
char *UP;	/* Upline (cursor up) */
char *cd;	/* clear to end of display */
char *ce;	/* clear to end of line */
char *cl;	/* clear display */
char *cm;	/* cursor movement */
char *dc;	/* delete character */
char *dl;	/* delete line */
char *dm;	/* delete mode */
char *dN;	/* # ms \n padding */
char *ed;	/* exit delete mode */
char *ei;	/* exit insert mode */
char *ho;	/* home */
char *ic;	/* insert character */
char *il;	/* insert line */
char *im;	/* insert mode */
char *ip;	/* insert padding */
char *nd;	/* non-destructive space */
char *vb;	/* visible bell */
char *se;	/* standout end */
char *so;	/* standout begin */
char *ec;	/* erase character */
char *DO;	/* down line (doesn't scroll) */
char *NL;	/* newline (scroll if necessary) */
int MT;		/* Meta keyboard */
int bs;		/* can baskspace */
int ns;		/* can't scroll */
int am;		/* wrap forward over end of line */
int sg;
int li, co;	/* lines, columns */
int curx, cury;
short ospeed;	/* output speed */
short ispeed;	/* input speed */

short speeds[] = {
	0, 50, 75, 110, 134, 150, 200, 300, 600, 1200, 1800, 2400, 4800, 9600
};

char termcap[1024];
char termstr[1024];	/* for string values */
char *termtype;		/* name of the terminal type for special checking */
char *termptr;
char *getenv();
/*
 * SUPDUP TTYOPT bits
 */

/* highest order byte */
#define TOALT 0200000
#define TOCLC 0100000
#define TOERS 040000
#define TOMVB 010000
#define TOSAI 04000
#define TOOVR 01000
#define TOMVU 0400
#define TOLWR 020
#define TOFCI 010
#define TOLID 02
#define TOCID 01

#define TOSA1 02000
#define TOMOR 0200
#define TOROL 0100
#define TPCBS 040
#define TPORS 010

/*
 * SUPDUP display protocol commands
 */
#define TDMOV 0200
#define TDMV1 0201
#define TDEOF 0202
#define TDEOL 0203
#define TDDLF 0204
#define TDCRL 0207
#define TDNOP 0210
#define TDBS	0211
#define TDLF	0212
#define TDCR	0213
#define TDORS 0214
#define TDQOT 0215
#define TDFS 0216
#define TDMVO 0217
#define TDCLR 0220
#define TDBEL 0221
#define TDILP 0223
#define TDDLP 0224
#define TDICP 0225
#define TDDCP 0226
#define TDBOW 0227
#define TDRST 0230
#define TDGRF	0231
#define TDSCU	0232	/* Scroll region up */
#define TDSCD	0233	/* Scroll region down */

#define ITP_QUOTE	034	/* quoting character for bucky bits */
/*
 * commands between processes
 */
#define IPC_MODE 0
#define IPC_CLOSED 1

char finbuf[BUFSIZ];
char foutbuf[BUFSIZ];

char *prompt;

char escape = ctrl('^');
char tkill, terase;

int connect();
int quit();
int pausecmd();
int bye();
int help();
int setlocation();
int setescape();
int status();
int abrtout();
int attn();
int brkkey();

#define HELPINDENT (sizeof("disconnect"))

struct cmd {
	char *name;
	int (*handler)();
};

struct cmd maintab[] = {
	"open",		connect,
	"connect",	connect,
	"bye",		bye,
	"disconnect",	bye,
	"exit",		quit,
	"quit",		quit,
	"pause",	pausecmd,
	"escape",	setescape,
	"location",	setlocation,
	"status",	status,
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
	"location",	setlocation,
	"status",	status,
	"help",		help,
	"?",		help,
	0
};

struct cmd *cmdtab = maintab;

int margc;
char *margv[20];
char line[132];

/*
 * construct an control character sequence for a special character
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

putch(c)
{
	putchar(c);
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
	nice(-5);			/* this prog needs a little speed */
	setuid(getuid());		/* in case setuid(root) was needed */
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
				cname = "SUPDUP";
	} else {
	arpa:
		c = chaos_addr(via);
		if (c == 0) {
			printf("routing host %s unknown\n", via);
			goto lose1;
		}
		sprintf(contact, "%s %s",
			host->host_name, cname ? cname : "137");
		cname = "TCP";
		fixhostname(contact);
	}
	printf("Trying...");
	fflush(stdout);
	if ((conn = chopen(c, cname, 2, 1, contact, 0, 0)) < 0) {
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
	isopen = 1;
	ioctl(conn, CHIOCSMODE, CHRECORD);
	tgetent(termcap, termtype = getenv("TERM"));
	termptr = termstr;
	bs = tgetflag("bs");
	MT = tgetflag("MT");
	ns = tgetflag("ns");
	am = tgetflag("am");
	BC = tgetstr("bc", &termptr);
	UP = tgetstr("up", &termptr);
	cd = tgetstr("cd", &termptr);
	ce = tgetstr("ce", &termptr);	
	cl = tgetstr("cl", &termptr);
	cm = tgetstr("cm", &termptr);
	dc = tgetstr("dc", &termptr);
	dl = tgetstr("dl", &termptr);
	dm = tgetstr("dm", &termptr);
	dN = tgetstr("dN", &termptr);
	ed = tgetstr("ed", &termptr);
	ei = tgetstr("ei", &termptr);
	ho = tgetstr("ho", &termptr);
	ic = tgetstr("ic", &termptr);
	il = tgetstr("al", &termptr);
	im = tgetstr("im", &termptr);
	ip = tgetstr("ip", &termptr);
	nd = tgetstr("nd", &termptr);
	vb = tgetstr("vb", &termptr);
	so = tgetstr("so", &termptr);
	se = tgetstr("se", &termptr);
	ec = tgetstr("ec", &termptr);
	DO = tgetstr("do", &termptr);
	NL = tgetstr("nl", &termptr);
	if(!NL) NL = "\n";
	co = tgetnum("co");
	li = tgetnum("li");	
	sg = tgetnum("sg");
#ifdef SIGEMT
	signal(SIGEMT, ipc);
#endif
	mode(0);	/* set up ospeed */
	fk = fork();
	if (fk == 0) {
		fin = fdopen(tord[0], "r");
		fout = fdopen(towr[1], "w");
		close(tord[1]);
		close(towr[0]);
		rd();
		printf("Connection closed by foreign host\r\n");
		putc(IPC_CLOSED, fout);
		fflush(fout);
#ifdef SIGEMT
		kill(pid, SIGEMT);
#endif
		exit(3);
	}
	fin = fdopen(towr[0], "r");
	fout = fdopen(tord[1], "w");
	close(towr[1]);
	close(tord[0]);
	mode(1);
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
	if (conn >= 0)
		printf("Connected to %s.\r\n", host->host_name);
	else
		printf("No connection.\r\n");
	printf("terminal location is '%s'\r\n", location);
	printf("escape character is '%s'\r\n", control(escape));
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
#ifdef SIGTSTP
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
	if (argc <= 0) {	/* give help */
		printf("Disconnect from the current site.\n");
		return(0);
	}
	mode(0);
	if (conn >= 0) {
		ioctl(conn, CHIOCOWAIT, 1);	/* sends an EOF first */
		if (fk) {
			kill(fk, SIGKILL);
			wait((int *)NULL);
		}
		chreject(conn, "");	/* must send a CLS explicitly */
		close(conn);
		if (fin != NULL) {
			fclose(fin);
			fclose(fout);
		}
		conn = -1;
		fin = NULL;
		fout = NULL;
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
jmp_buf jpbuf;

/*
 * handle IPC via the chaos connection between processes
 */
ipc()
{
	register int c;
#ifndef BSD42
	signal(SIGEMT, ipc);
#endif
	switch(c = getc(fin)) {
	case IPC_MODE:
		mode(getc(fin));
		break;

	case IPC_CLOSED:
		isopen = 0;
		longjmp(jpbuf,-1);
	}
}

mode(f)
register int f;
{
	register int old;
#ifndef TERMIOS
	struct sgttyb stbuf;
#endif
	if (fk == 0 && fout != NULL) {	/* child process */
		putc(IPC_MODE, fout);
		putc(f, fout);
		fflush(fout);
#ifdef SIGEMT
		kill(pid, SIGEMT);
#endif
		return;
	}
#ifndef TERMIOS
	ioctl(fileno(stdin), TIOCGETP, &stbuf);
	tkill = stbuf.sg_kill;
	terase = stbuf.sg_erase;
	ispeed = stbuf.sg_ispeed;
	ospeed = stbuf.sg_ospeed;
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
	long int waiting;
	long int hi, lo;
	extern int errno;
	supdupword(-8L, 0L);	/* count */
	supdupword(0L, 7L);	/* TCTYP */
	hi = 0;
	lo = 0;
	if (bs || BC)
		hi |= TOMVB;
	if (tgetflag("os"))
		hi |= TOOVR;
	if ((hi & TOMVB) != 0	/* need to to TDDLF by */
	 && (hi & TOOVR) == 0	/* space backspace */
	 && ce)
		hi |= TOERS;
	if (UP)
		hi |= TOMVU;
	hi |= TOLWR;
	if (il  && dl)
		hi |= TOLID;
	if (im && dm)
		hi |= TOCID;
	if (ns)
		hi |= TOMOR;
	else
		hi |= TOROL;
	lo |= TPCBS;
	supdupword(hi, lo);
	supdupword(0L, (long)li);
	/*
	 * ITS wants to be able to put a character in a position one past the
	 * last legal position on the screen, so we must subtract one from the
	 * real screen size.  Also, it expects this action not to wrap, so
	 * for terminals that wrap, we must subtract even one more column.
	 * We could check for some terminal types and specifically enable or
	 * disable wrapping here.  If the terminal is an incoming supdup, we
	 * don't subtract anything.
	 */
	supdupword(0L, (long)(co - (strcmp("supdup", termtype) == 0 ? 0 :
				    (am ? 2 : 1))));
	supdupword(0L, ns ? 0L : 1L);
	supdupword(0L, 0L);	/* TSMART */
	supdupword(0L, (long)speeds[ispeed]);
	supdupword(0L, (long)speeds[ospeed]);
	supduploc();
	chflush(DATOP);
	if (!setjmp(jpbuf)) {
loop:
		while ((c = getchar()) != EOF) {
			if (MT && (c & 0200)) {
				wrconn(ITP_QUOTE);
				wrconn(0100|2);
				wrconn(c & 0177);
				goto check;
			}
			c &= 0177;
			if (c != escape)
				wrconn(c);
			else if ((c = getchar()&0177) == escape)
				wrconn(c);
			else {
#ifdef SIGSTOP
				kill(fk, SIGSTOP);
#endif
				switch(c) {
				case '?':
					printf("\r\n");
					printf("?\tLists the command characters\r\n");
					printf("c\tEquivalent to the close command\r\n");
					printf("p\tEquivalent to the pause command\r\n");
					printf("q\tEquivalent to the exit command\r\n");
					printf("s\tEquivalent to the status command\r\n");
					printf("x\tEnter extended command mode\r\n");
					break;

				case 'C':
				case 'c':
#ifdef SIGCONT
					kill(fk, SIGCONT);
#endif
					return(0);

				case 'p':
				case 'P':
					call(pausecmd, "pause", 0);
					break;

				case 'q':
				case 'Q':
					call(quit, "exit", 0);
					break;

				case 's':
				case 'S':
					call(status, "status", 0);
					break;

				case 'x':
				case 'X':
					if (extend()) {
#ifdef SIGCONT
						kill(fk, SIGCONT);
#endif
						return(0);
					}
					break;

				default:
					printf("?Invalid escape character %o\r\n", c);
					break;
				}
#ifdef SIGCONT
			kill(fk, SIGCONT);
#endif
			}
	check:
			if (ioctl(fileno(stdin), FIONREAD, &waiting) >= 0
			 && waiting == 0) {
				chflush(DATOP);
			}
		}
		if (errno == EINTR && isopen) {
			clearerr(stdin);
			goto loop;
		}
	}
	return(0);
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
	static int eof;
	extern int errno;
	eof = 0;
	while (1) {
		if (rdcount <= 0 && ioctl(conn, FIONREAD, &waiting) == 0 &&
		    waiting == 0) {
			if (eof)
				break;
			fflush(stdout);
		}
		if ((c = rdconn()) == EOF) {
			if (errno != EINTR)
				if (eof++ > 0)	/* seen an EOF! */
					break;
			continue;
		}
		if (c >= 0200)
			supdup(c);
		else
			putchar(c);
	}
	fflush(stdout);
}

/*
 * SUPDUP output display protocol
 */
supdup(c)
register int c;
{
	int x, y;
	switch(c) {
	case TDMOV:
		cury = rdconn();
		curx = rdconn();
	case TDMV1:
	case TDMVO:
		y = rdconn();
		x = rdconn();
		if (cm)
			tputs(tgoto(cm, x, y), x+y, putch);
		/* do cursor addressing with up/down, etc */
		curx = x;
		cury = y;
		break;


	case TDEOF:
		tputs(cd, li-cury, putch);
		break;

	case TDEOL:
		tputs(ce, co-curx, putch);
		break;

	case TDDLF:
		if (ec) {
			tputs(ec, 1, putch);
			break;
		}
		putchar(' ');
	case TDBS:
		if (bs)
			putchar('\b');
		else if (BC)
			tputs(BC, 1, putch);
		break;
	case TDCR:
		putchar('\r');
		break;
	case TDLF:
		tputs(NL, 1, putch);
		if (dN) tputs(dN, 1, putch);
		break;
	case TDCRL:
		putchar('\r');
		tputs(NL, 1, putch);
		if (dN) tputs(dN, 1, putch);
		tputs(ce, 1, putch);
		break;

	case TDNOP:
		break;

	case TDORS:
		putchar(034);
		putchar(020);
		putchar(cury);
		putchar(curx);
		break;

	case TDQOT:
		putchar(rdconn());
		break;

	case TDFS:
		tputs(nd, 1, putch);
		break;

	case TDCLR:
		if (cl)
			tputs(cl, li, putch);
		else {
			if (ho)
				tputs(ho, curx+cury, putch);
			else if (cm)
				tputs(tgoto(cm, 0, 0), curx+cury, putch);
			tputs(cd, li, putch);
		}
		break;

	case TDBEL:
		if (vb)
			tputs(vb, 1, putch);
		else
			putchar('\007');
		break;

	case TDILP:
		x = rdconn();
		while (--x >= 0)
			tputs(il, 1, putch);
		break;

	case TDDLP:
		x = rdconn();
		while (--x >= 0)
			tputs(dl, 1, putch);
		break;

	case TDICP:
		x = rdconn();
		tputs(im, 1, putch);
		while (--x >= 0) {
			if (!im && ic)
				tputs(ic, 1, putch);
			putchar(' ');
			if (ip)
				tputs(ip, 1, putch);
		}
		tputs(ei, 1, putch);
		break;

	case TDDCP:
		x = rdconn();
		tputs(dm, 1, putch);
		while (--x >= 0)
			tputs(dc, 1, putch);
		tputs(ed, 1, putch);
		break;

	case TDBOW:
		if (so && sg <= 0)
			tputs(so, 1, putch);
		break;
	case TDRST:
		if (se && sg <= 0)
			tputs(se, 1, putch);
		break;
	default:
		fprintf(stderr, "bad supdup code %o ignored\r\n", c);
		break;
	}
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
 * set the terminal location
 */
setlocation(argc, argv)
int argc;
char *argv[];
{
	register char *arg;
	char buf[132];
	if (argc <= 0) {	/* give help */
		printf("Set terminal location\n");
		return(0);
	}
	if (argc > 2)
		arg = argv[1];
	else {
		printf("Location of terminal: ");
		fflush(stdout);
		gets(buf);
		arg = buf;
	}
	strncpy(location, arg, sizeof(location) - 1);
	if (conn >= 0) {	/* send the location */
		supduploc();
		chflush(DATOP);
	}
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

	if (rdcount-- > 0)
		return(*ptr++&0377);
	if ((rdcount = read(conn, &pkt, sizeof(pkt))) <= 0)
		return(EOF);
	switch (pkt.cp_op&0377) {
	case DATOP:
		break;

	case CLSOP:
		fwrite(pkt.cp_data, 1, rdcount-1, stdout);
		printf("\r\n");
	case EOFOP:
		return(EOF);
	case LOSOP:
		printf("lossage: ");
		fwrite(pkt.cp_data, 1, rdcount-1, stdout);
		printf("\r\n");
		return(EOF);

	default:
		fprintf(stderr, "?packet op = %o len = %d\r\n",
			pkt.cp_op&0377, rdcount);
		break;
	}		
	rdcount--;	/* skip past the opcode */
	ptr = pkt.cp_data+1;
	rdcount--;
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
 * send a 36-bit SUPDUP word.
 * hi and lo are the high and low 18 bits.
 * The word is sent with 6 bits per byte,
 * most significant 6 bits first.
 */
supdupword(hi, lo)
register long hi, lo;
{
	wrconn((int)(hi>>12)&077);
	wrconn((int)(hi>>6)&077);
	wrconn((int)hi&077);
	wrconn((int)(lo>>12)&077);
	wrconn((int)(lo>>6)&077);
	wrconn((int)lo&077);
}


/*
 * send the terminal location
 */
supduploc()
{
	register char *s;
	s = location;
	if (*s) {
		wrconn(0300);
		wrconn(0302);
		while (*s)
			wrconn(*s++);
		wrconn(0);
	}
}

/*
 * null func for timeout
 */
timeout()
{
}
