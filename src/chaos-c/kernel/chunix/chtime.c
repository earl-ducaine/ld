/*
 *	$Source: /projects/chaos/kernel/chunix/chtime.c,v $
 *	$Author: brad $
 *	$Locker:  $
 *	$Log: chtime.c,v $
 *	Revision 1.1.1.1  1998/09/07 18:56:08  brad
 *	initial checkin of initial release
 *	
 * Revision 1.1  84/06/12  20:06:06  jis
 * Initial revision
 * 
 */
#ifndef lint
static char *rcsid_chtime_c = "$Header: /projects/chaos/kernel/chunix/chtime.c,v 1.1.1.1 1998/09/07 18:56:08 brad Exp $";
#endif lint

#include "chaos.h"

#ifdef BSD42
#include "param.h"
#include "systm.h"
#include "time.h"
#include "kernel.h"
#endif

#ifdef linux
#include <linux/signal.h>
#include <linux/errno.h>
#include <linux/sched.h>
#include <linux/time.h>
#endif

/*
 * Return the time according to the chaos TIME protocol, in a long.
 * No byte shuffling need be done here, just time conversion.
 */
void
ch_time(tp)
register long *tp;
{
#ifdef linux
	struct timeval time;
	do_gettimeofday(&time);
#endif

#if defined(BSD42) || defined(linux)
	*tp = time.tv_sec;
#else
	*tp = time;
#endif
	*tp += 60L*60*24*((1970-1900)*365L + 1970/4 - 1900/4);
}

void
ch_uptime(tp)
register long *tp;
{
#ifdef linux
	*tp = jiffies;
#else
#ifdef BSD42
	*tp = (time.tv_sec - boottime.tv_sec) * 60L;
#else
	*tp = (time - bootime) * 60L;
#endif
#endif
}
