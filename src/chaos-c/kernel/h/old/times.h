/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)times.h	7.1 (Berkeley) 6/4/86
 */
/*
 * RCS Info	
 *	$Header: /projects/chaos/kernel/h/old/times.h,v 1.1.1.1 1998/09/07 18:56:10 brad Exp $
 *	$Locker:  $
 */

/*
 * Structure returned by times()
 */
struct tms {
	time_t	tms_utime;		/* user time */
	time_t	tms_stime;		/* system time */
	time_t	tms_cutime;		/* user time, children */
	time_t	tms_cstime;		/* system time, children */
};
