/*
 * read the host status of the specified host
 * (contact name STATUS)

 * mbm@cipg -- works with 16bit and long status on 11 and vax
 *             fflush for each host
 *             tried to clean up output format
 * jek	    -- Improve output, allow interrupting out of each probe.
 */

#include <stdio.h>
#include <hosttab.h>
#include <setjmp.h>
#include <signal.h>

#ifdef linux
#include <bsd/sgtty.h>
#else
#include <sgtty.h>
#endif

int skip;
jmp_buf skipjmp;
int interrupt();

main(argc, argv)
int argc;
char *argv[];
{
	register int i;
	register char *name;
	register struct host_entry *h;

	signal(SIGINT, interrupt);
	if (argc > 1) while (--argc > 0)
		hostat(*++argv);
	else for (host_start(); h = host_next(); )
		if (chaos_addr(h->host_name, 0)) {
			printf("\n");
			fflush(stdout);
			hostat(h->host_name);
		}
	return(0);
}

hostat(arg)
char *arg;
{
	char name[32];
	register int f = -1;
	register int i;
	int addr;
	struct {
		short code;
		short nwords;
	} hdr;
	short unsigned s;
	long l;

	if ((addr = chaos_addr(arg, 0)) == 0) {
		printf("Host %s unknown\n",arg);
		return;
	}
	if (setjmp(skipjmp)) {
		if (f >= 0)
			close(f);
		printf(" Skipped\n");
		return;
	}
	printf("%s (0%o): ", arg, addr);
	fflush(stdout);
	skip++;		/* allow for skips */
	if ((f = chopen(addr, "STATUS", 0, 0, 0, 0, 0)) < 0) {
		printf("not responding.\n");
		return;
	}
	read(f, name, sizeof(name));
	printf("%.32s\n", name);
	odrain(fileno(stdout));
	skip = 0;	/* Once he's seen the name, interrupts exit */
	printf("subnet rcvd       xmtd       abrt    lost");
	printf("    crc1    crc2    leng    rej\n");
	while (read(f, &hdr, sizeof(hdr)) == sizeof(hdr)) {
		if (hdr.code < 0400) {
			printf("%-7o", hdr.code);
			for (i = 0; i < hdr.nwords; i++) {
				char *foo = "%-8u";
				if (i < 2) foo = "%-11u";
				if (read(f, &s, sizeof(s)) >= 0)
					printf(foo, s);
				else
 					fprintf(stderr, "read error\n");
			}
		} else if (hdr.code < 01000) {
			printf("%-7o", hdr.code & 0377);
			for (i = 0; i < hdr.nwords; i += sizeof(long)/sizeof(short)) {
				char *foo = "%-8u";
				if (i < 2*sizeof(long)/sizeof(short))
 					foo = "%-11u"; /* xtra for rcv,xmt*/
				if (read(f, &l, sizeof(l)) >= 0) {
#ifdef	pdp11
					swaplong(&l);
#endif
					printf(foo, l);
				} else
					fprintf(stderr, "read error\n");
			}
		} else
			printf("unknown subnet code %o", hdr.code);
		putchar('\n');
	}
	close(f);
}

swaplong(lng)
int	lng[2];
{
	register int temp;

	temp = lng[0];
	lng[0] = lng[1];
	lng[1] = temp;
}

interrupt()
{
	if (skip) {
		signal(SIGINT, interrupt);
		skip = 0;
		longjmp(skipjmp, 1);
	}
	exit(1);
}

/*
 * This KLUDGE is the only way to cleanly wait for the output to actually
 * be sent to the tty.
 */
odrain(f)
{
#ifndef TERMIOS
	struct sgttyb sg;

	ioctl(f, TIOCGETP, &sg);
	ioctl(f, TIOCSETP, &sg);
#endif
}
