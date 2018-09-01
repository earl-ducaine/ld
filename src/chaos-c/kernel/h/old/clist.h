/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)clist.h	7.1 (Berkeley) 6/4/86
 */
/*
 * RCS Info	
 *	$Header: /projects/chaos/kernel/h/old/clist.h,v 1.1.1.1 1998/09/07 18:56:08 brad Exp $
 *	$Locker:  $
 */

/*
 * Raw structures for the character list routines.
 */
struct cblock {
	struct cblock *c_next;
	char	c_info[CBSIZE];
};
#ifdef KERNEL
struct	cblock *cfree;
int	nclist;
struct	cblock *cfreelist;
int	cfreecount;
#endif
