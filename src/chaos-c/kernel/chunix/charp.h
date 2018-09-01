/* $Id: charp.h,v 1.1 1999/11/24 18:16:55 brad Exp $ */

/* chaosnet arp internal structure */

#define ELENGTH	6
#define NPAIRS	20	/* how many addresses should we remember */
struct ar_pair	{
	chaddr		arp_chaos;
	u_char		arp_ether[6];
	long		arp_time;
};
