#include <stdio.h>
#include <sys/chaos.h>
#include <hosttab.h>

/*
 * Initialize the the NCP with my name and number.
 */
char myname[CHSTATNAME+1];
main(int argc, char *argv[])
{
    char *name;
    int addr;
    int fd;

    if (argc <= 1) {
	if ((name = host_me()) == NULL ||
	    (addr = chaos_addr(name)) == 0)
	{
	    fprintf(stderr, "CHAOS NET CAN'T BE INITIALIZED!\n");
	    fprintf(stderr, "I DON'T KNOW WHO I AM. Fix host table.\n\n");

	    fprintf(stderr, "usage: %s <addr> <name>\n", argv[0]);
	    exit(1);
	}
	strcpy(myname, name);
    } else {
	addr = atoi(argv[1]);
	strncpy(myname, argv[2], CHSTATNAME);
    }

    if ((fd = open(CHURFCDEV, 0)) < 0 ||
	ioctl(fd, CHIOCADDR, addr) < 0 ||
	ioctl(fd, CHIOCNAME, myname) < 0)
    {
	fprintf(stderr, "chaos net initializization failed!\n");
	exit(1);
    }

    fprintf(stderr, "Chaos NCP started. I am '%s' at 0%o\n", myname, addr);
    exit(0);
}
