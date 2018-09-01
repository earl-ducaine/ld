/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)mtio.h	7.1 (Berkeley) 6/4/86
 */
/*
 * RCS Info	
 *	$Header: /projects/chaos/kernel/h/old/mtio.h,v 1.1.1.1 1998/09/07 18:56:09 brad Exp $
 *	$Locker:  $
 */

/*
 * Structures and definitions for mag tape io control commands
 */

/* structure for MTIOCTOP - mag tape op command */
struct	mtop	{
	short	mt_op;		/* operations defined below */
	daddr_t	mt_count;	/* how many of them */
};

/* operations */
#define MTWEOF	0	/* write an end-of-file record */
#define MTFSF	1	/* forward space file */
#define MTBSF	2	/* backward space file */
#define MTFSR	3	/* forward space record */
#define MTBSR	4	/* backward space record */
#define MTREW	5	/* rewind */
#define MTOFFL	6	/* rewind and put the drive offline */
#define MTNOP	7	/* no operation, sets status only */
#define MTCACHE	8	/* enable controller cache */
#define MTNOCACHE 9	/* disable controller cache */

/* structure for MTIOCGET - mag tape get status command */

struct	mtget	{
	short	mt_type;	/* type of magtape device */
/* the following two registers are grossly device dependent */
	short	mt_dsreg;	/* ``drive status'' register */
	short	mt_erreg;	/* ``error'' register */
/* end device-dependent registers */
	short	mt_resid;	/* residual count */
/* the following two are not yet implemented */
	daddr_t	mt_fileno;	/* file number of current position */
	daddr_t	mt_blkno;	/* block number of current position */
/* end not yet implemented */
};

#if defined(VAX630) && defined(VAXSTAR)
/* Basic definitions common to various tape drivers */
#define b_repcnt	b_bcount		/* Repeat count 	*/
#define b_command	b_resid 		/* Command value	*/
#define SSEEK		0x01			/* Seeking		*/
#define SIO		0x02			/* Doing sequential i/o */
#define SCOM		0x03			/* Sending control cmd. */
#define SREW		0x04			/* Sending drive rewnd. */
#define SERASE		0x05			/* Erase interrec. gap	*/
#define SERASED 	0x06			/* Erased interrec. gap */
#define MASKREG(r)	((r) & 0xffff)		/* Register mask	*/
#define UNIT(dev)	minor(dev)>0x001f ? \
			(minor(dev)&0x001f)| \
			(minor(dev)&0x03): \
			minor(dev)&0x03 	/* Tape unit number	*/
#define SEL(dev)	(minor(dev)&0x001c)	/* Tape select number	*/
#define INF		(daddr_t)1000000L	/* Invalid block number */
#define DISEOT		0x01			/* Disable EOT code	*/
#define DBSIZE		0x20			/* Dump blocksize (32)	*/
#define PHYS(a,b)	((b)((int)(a)&0x7fffffff)) /* Physical dump dev.*/
#define MTLR		0x00		/* Low density/Rewind (0)	*/
#define MTMR		0x10		/* Medium density/Rewind (16)	*/
#define MTHR		0x08		/* High density/Rewind (8)	*/
#define MTLN		0x04		/* Low density/Norewind (4)	*/
#define MTMN		0x14		/* Medium density/Norewind (20) */
#define MTHN		0x0c		/* High density/Norewind (12)	*/
#define MTX0		0x00		/* eXperimental 0		*/
#define MTX1		0x01		/* eXperimental 1		*/

/* more Tape operation definitions for operation word (mt_op) */
#define MTCSE		0x0a		/* Clear serious exception	*/
#define MTCLX		0x0b		/* Clear hard/soft-ware problem */
#define MTCLS		0x0c		/* Clear subsystem		*/
#define MTENAEOT	0x0d		/* Enable default eot code	*/
#define MTDISEOT	0x0e		/* Disable default eot code	*/

/* Get status definitions for device type word (mt_type) */
#define MT_ISTS 	0x01		/* ts11/ts05/tu80		*/
#define MT_ISHT 	0x02		/* tm03/te16/tu45/tu77		*/
#define MT_ISTM 	0x03		/* tm11/te10			*/
#define MT_ISMT 	0x04		/* tm78/tu78			*/
#define MT_ISUT 	0x05		/* tu45 			*/
/* notice DEC is cheating here, using SUN's numbers. */
#define MT_ISTMSCP	0x06		/* All tmscp tape drives	*/
#define MT_ISST		0x07		/* TZK50 on vaxstar		*/
#else /* !VAXSTAR */
/*
 * Constants for mt_type byte.  These are the same
 * for controllers compatible with the types listed.
 */
#define	MT_ISTS		0x01		/* TS-11 */
#define	MT_ISHT		0x02		/* TM03 Massbus: TE16, TU45, TU77 */
#define	MT_ISTM		0x03		/* TM11/TE10 Unibus */
#define	MT_ISMT		0x04		/* TM78/TU78 Massbus */
#define	MT_ISUT		0x05		/* SI TU-45 emulation on Unibus */
#define	MT_ISCPC	0x06		/* SUN */
#define	MT_ISAR		0x07		/* SUN */
#define	MT_ISTMSCP	0x08		/* DEC TMSCP protocol (TU81, TK50) */
#endif /* VAXSTAR */

/* mag tape io control commands */
#define	MTIOCTOP	_IOW(m, 1, struct mtop)		/* do a mag tape op */
#define	MTIOCGET	_IOR(m, 2, struct mtget)	/* get tape status */
#define MTIOCIEOT	_IO(m, 3)			/* ignore EOT error */
#define MTIOCEEOT	_IO(m, 4)			/* enable EOT error */

#ifndef KERNEL
#define	DEFTAPE	"/dev/rmt12"
#endif
