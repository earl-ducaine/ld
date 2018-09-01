/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)vcmd.h	7.1 (Berkeley) 6/4/86
 */
/*
 * RCS Info	
 *	$Header: /projects/chaos/kernel/h/old/vcmd.h,v 1.1.1.1 1998/09/07 18:56:10 brad Exp $
 *	$Locker:  $
 */

#ifndef _IOCTL_
#ifdef KERNEL
#include "ioctl.h"
#else
#include <sys/ioctl.h>
#endif
#endif

#define	VPRINT		0100
#define	VPLOT		0200
#define	VPRINTPLOT	0400

#define	VGETSTATE	_IOR(v, 0, int)
#define	VSETSTATE	_IOW(v, 1, int)
