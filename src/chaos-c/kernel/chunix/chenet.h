/*
 *	$Source: /projects/chaos/kernel/chunix/chenet.h,v $
 *	$Author: brad $
 *	$Locker:  $
 *	$Log: chenet.h,v $
 *	Revision 1.2  1999/11/24 18:16:25  brad
 *	added basic stats code with support for /proc/net/chaos inode
 *	basic memory accounting - thought there was a leak but it's just long term use
 *	fixed arp bug
 *	removed more debugging output
 *	
 *	Revision 1.1.1.1  1998/09/07 18:56:08  brad
 *	initial checkin of initial release
 *	
 * Revision 1.2  86/09/29  14:47:32  mbm
 * ETHERPUP_CHAOSTYPE --> ETHERTYPE_CHAOS
 * 
 * Revision 1.1  84/06/12  20:08:54  jis
 * Initial revision
 * 
 */
struct chetherinfo	{
#ifdef BSD42
	struct arpcom *che_arpcom;
#endif
#ifdef linux
	void *prot_hook1;
	void *prot_hook2;
	void *bound_dev;
#endif
};
#define ETHERTYPE_CHAOS		0x0804		/* Chaos protocol */

