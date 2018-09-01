#include <stdio.h>
#include <sys/chaos.h>

/*
 * Enable an ethernet interface
 */
main(argc, argv)
int argc;
char **argv;
{
	int i;
	extern int errno;
#if defined(BSD42) || defined(linux)
	struct chether che;

	if (argc < 2) {
	  printf("usage: %s <if-name> <node-number>\n", argv[0]);
	  exit(1);
	}
	strncpy(che.ce_name, argv[1], CHIFNAMSIZ);
	sscanf(argv[2], "%o", &i);
	che.ce_addr = i;
	printf("Setting device %s to chaos address 0%o\n", che.ce_name, che.ce_addr);
	if ((i = open(CHURFCDEV, 0)) < 0 ||
	    ioctl(i, CHIOCETHER, &che) < 0)
#else
	struct chiladdr ca;

	ca.cil_device = atoi(argv[1]);
	sscanf(argv[2], "%o", &i);
	ca.cil_address = i;
	printf("Setting device %d to 0%o\n", ca.cil_device, ca.cil_address);
	if ((i = open(CHURFCDEV, 0)) < 0 ||
	    ioctl(i, CHIOCILADDR, &ca) < 0)
#endif
		printf("Can't set chaos address on ethernet: %d\n", errno);
	exit(0);
}
