/*
 *	$Source: /projects/chaos/kernel/chunix/chconf.c,v $
 *	$Author: brad $
 *	$Locker:  $
 *	$Log: chconf.c,v $
 *	Revision 1.1.1.1  1998/09/07 18:56:08  brad
 *	initial checkin of initial release
 *	
 * Revision 1.2  85/09/07  15:33:55  root
 * Removed chipattach call.
 * 
 * Revision 1.1  84/06/12  20:06:03  jis
 * Initial revision
 * 
 */
#ifndef lint
static char *rcsid_chconf_c = "$Header: /projects/chaos/kernel/chunix/chconf.c,v 1.1.1.1 1998/09/07 18:56:08 brad Exp $";
#endif lint

#include "chaos.h"
#include "chsys.h"
#include "chconf.h"
#include "../chncp/chncp.h"

#ifdef linux
#include <linux/types.h>
#include <linux/signal.h>
#include <linux/errno.h>
#include <linux/sched.h>
#include <linux/proc_fs.h>
#endif

#ifdef BSD42
#include "time.h"
#include "kernel.h"
#endif

#if 0
#include "chip.h"
#endif

/*
 * This file contains initializations of configuration dependent data
 * structures and device dependent initialization functions.
 */

/*
 * We must identify ourselves
 */
short Chmyaddr = -1;
char Chmyname[CHSTATNAME] = "Uninitialized";
short chhosts[] = {0};
int Chhz = 60;		/* This is set correctly at auto-conf time but needs
			 * a non-zero initial value at boot time.
			 */

/*
 * Reset the NCP and all devices.
 */
void
chreset()
{
	register struct chroute *r;

	for (r = Chroutetab; r < &Chroutetab[CHNSUBNET]; r++)
		if (r->rt_cost == 0)
			r->rt_cost = CHHCOST;
#if NCHDR > 0
	chdrinit();
#endif NCHDR
#if NCHCH > 0
	chchinit();
#endif NCHCH
#if NCHIL > 0
	chilinit();
#endif NCHIL
#if NCHETHER > 0
	cheinit();
#endif
/* If we have an internet... allow UNC encapsulation. */
#ifdef INET
#if NCHIP > 0
	chipattach ();
#endif
#endif
	/*
	 * This is necessary to preserve the modularity of the
	 * NCP.
	 */
#ifdef linux
	Chhz = HZ;
#else
	Chhz = hz;
#endif
}

void
chdeinit()
{
#if NCHETHER > 0
	chedeinit();
#endif
}

/*
 * Check for interface timeouts
 */
void
chxtime()
{
#ifdef NDR11C
	chdrxtime();
#endif NDR11C
}
