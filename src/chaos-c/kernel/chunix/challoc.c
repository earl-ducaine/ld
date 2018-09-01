/*
 *	$Source: /projects/chaos/kernel/chunix/challoc.c,v $
 *	$Author: brad $
 *	$Locker:  $
 *	$Log: challoc.c,v $
 *	Revision 1.1.1.1  1998/09/07 18:56:08  brad
 *	initial checkin of initial release
 *	
 * Revision 1.3  87/04/06  10:35:43  rc
 * Calls to spl6() are replaced to calls to splimp() in order to maintain
 * consistency with the rest of the Chaos code.
 * 
 * Revision 1.2  87/04/06  10:33:33  rc
 * The include files are updated to reflect the correct "include" directory.
 * These changes were not previously RCSed. The original changes did not
 * have lines of the form ... #include "../netchaos/..."
 * 
 * Revision 1.1  84/06/12  20:05:58  jis
 * Initial revision
 * 
 */
#ifndef lint
static char *rcsid_challoc_c = "$Header: /projects/chaos/kernel/chunix/challoc.c,v 1.1.1.1 1998/09/07 18:56:08 brad Exp $";
#endif lint
#ifndef linux
/*
 * challoc.c
 * UNIX memory allocator. For V7, 4.1 BSD, and 4.2 BSD.
 * This file really contains two entirely separate implementations, one
 * for 4.2 BSD which uses its network buffers, and the other for 11 and
 * 4.1 BSD implementations.
 */
#include "chaos.h"
#include "chsys.h"
#include "chconf.h"
#include "challoc.h"

#ifdef linux
#define printf printk
#endif

#ifdef BSD42
#include "mbuf.h"
/*
 * A 4.2 BSD interface using the network buffers.
 * Each allocation cannot exceed CLBYTES - MSIZE. (now 1024 - 128).
 * This is obviously a quick and dirty solution, but its so simple
 * that it shouldn't be much of a burden to change if the
 * other allocation code changes.  The alternative is to mung the entire
 * CHAOS NCP code to deal with buffer chains.
 *
 * The basic idea is that for allocations that fit entirely inside
 * an mbuf structure we do the normal thing, via MGET.  Otherwise
 * we allocate a cluster, leaving room for an mbuf at the front.
 * When freeing we recognize the clusters by their data address having an
 * address that is zero mod MSIZE, which can never happen for an
 * allocation inside an MBUF.  When we pass one of these larger
 * ones to a 4.2 BSD module, we must allocate an additional small
 * mbuf to point at the cluster since the 4.2 code requires a separate
 * mbuf to point at a cluster.
 *
 * A lot of this is because all the chaos code is written assuming
 * that buffers are contiguous rather than chained in small pieces.
 * For maximum size chaos packets, which use 512 bytes, the cluster,
 * being 1024 bytes is used up with an mbuf (128 bytes) and the
 * chaos packet (512 bytes) for a wastage of 1024 - 512 - 128.
 */
char *
ch_alloc(size, cantwait)
register int size;
{
	register struct mbuf *m;
	register int offset;

	if (size > MLEN) {
		if (size > CLBYTES - MSIZE)
			panic("ch_alloc too big");
		MCLALLOC(m,1);
		offset = MSIZE;
	} else {
		MGET(m, !cantwait, MT_DATA);
		offset = MMINOFF;
	}
	if (m == 0)
		return 0;
	m->m_off = offset;
	m->m_len = size;
	m->m_next = 0;
	return mtod(m, char *);
}

void
ch_free(p)
register char *p;
{
	register struct mbuf *m = chdtom(p);
	struct mbuf *n;

	if (m->m_off < MSIZE) {
		MFREE(m, n);
	} else {
	        MCLFREE(m);
	}
}

int
ch_size(p)
register char *p;
{
	return chdtom(p)->m_len;
}
/* ARGSUSED */
ch_badaddr(p)
char *p;
{
	return 0;
}
void ch_bufalloc(){}
void ch_buffree(){}

#else of ifdef BSD42
/*
 * This allocator is for non 4.2 BSD systems and uses disk buffers for space.
 */
#include "../chncp/chncp.h"
#include "../h/buf.h"

#define BUFPRI PRIBIO		/* Sleep priority for buffers */
#define	b_bits		b_blkno	/* Bit map for chaos bufs in buffer (0=free) */
#define b_list		b_resid	/* Index in chsize that buffer belongs to */
#define b_nfree	b_bcount	/* Number of free chaos buffers in buffer */

#ifdef vax
#define CHMAXBUF 25
#define CHNMINPKT 2
#define CHNMAXPKT 36

#ifdef BSD42
#define bpfromch(x) (&buf[((x) - buffers) / MAXBSIZE ])
#define CHBLKSIZE 1024
#else BSD42
#define bpfromch(x) (&buf[((x) - buffers) / BSIZE ])
#define CHBLKSIZE BSIZE
#endif else BSD42

#else				/* i.e. not a vax system but a PDP11 */
#define CHBLKSIZE BSIZE
#define CHMAXBUF 12
#define CHNMINPKT 1
#define CHNMAXPKT 9

#ifdef	UCB_BUFOUT
extern char abuffers[NABUF][BSIZE];
#define bpfromch(x) (&abuf[((x) - abuffers) / BSIZE ])
#else
extern char buffers[NBUF][(BSIZE+BSLOP)];
#define bpfromch(x) (&buf[((x) - buffers) / (BSIZE+BSLOP) ])
#endif else UCB_BUFOUT

#endif else vax

/*
 * Structure for keeping track of various sizes of chaos buffers.
 * Initialized values should be parameterized.
 */
struct chsize {
	short ch_bufsize;	/* Size of packets to allocate from buffer */
	short ch_mxbufs;	/* Maximum buffers to allocate to this size */
	short ch_bufcount;	/* The count of buffers already allocated */
	short ch_buffree;	/* Number of free buffers on this list now */
	struct buf *ch_bufptr;	/* Pointer to buffers of this size */
} Chsizes[] = {
#define CHMINPKT 32
	{ CHMINPKT,	CHNMINPKT, },
	{ 128,	(CHNCONNS + 16)/(CHBLKSIZE/128), },
	{ 512,	CHNMAXPKT, },
};

#define NSIZES (sizeof(Chsizes)/sizeof(Chsizes[0]))

int Chbufwait;		/* Someone's waiting for buffers */
struct buf *Chbuflist;	/* Unassigned buffers */
int	Chnbufs;

/*
 * Allocate a chunk at least "size" large,
 * set flag means called from interrupt level - don't hang waiting for buffers
 * just return NULL
 */
char *
ch_alloc(size, flag)
{
	register struct chsize *sp;
	register struct buf *bp;
	register int j;
	long bit;
	int opl;

	opl = splimp();
again:
	for (sp = Chsizes; sp < &Chsizes[NSIZES]; sp++) {
		if (sp->ch_bufsize < size)
			continue;
		if (sp->ch_buffree == 0) {
			if (sp->ch_bufcount == sp->ch_mxbufs ||
			    (bp = Chbuflist) == NULL)
				continue;
			Chbuflist = bp->av_forw;
			bp->av_forw = sp->ch_bufptr;
			sp->ch_bufptr = bp;
			bp->b_nfree = j = CHBLKSIZE / sp->ch_bufsize;
			bp->b_bits = 0;
			bp->b_list = sp - Chsizes;
			sp->ch_buffree += j;
			sp->ch_bufcount++;
		} else 
			for (bp = sp->ch_bufptr;; bp = bp->av_forw)
				if (bp == NULL)
					panic("buffer lost somewhere");
				else if (bp->b_nfree != 0)
					break;
		/* Here bp points to a buffer to allocate */
		for (bit = 1L, j = 0; ; bit <<= 1, j++)
			if (!(bit & bp->b_bits))
				break;
		bp->b_bits |= bit;
		bp->b_nfree--;
		sp->ch_buffree--;
		debug(DALLOC,printf("Alloc: size=%d,adr = %x\n", size, bp->b_un.b_addr+(j * sp->ch_bufsize)));
		splx(opl);
		return (bp->b_un.b_addr+(j * sp->ch_bufsize));
	}
	if (!flag) {
		Chbufwait++;
		sleep((caddr_t)&Chbufwait, BUFPRI);
		goto again;
	}
	debug(DALLOC|DABNOR,printf("Alloc: size=%d, failed\n", size));
	splx(opl);
	return((caddr_t)0);
}
/*
 * Free the previously allocated storage at "p"
 */
void
ch_free(p)
char *p;
{
	register struct buf *bp;
	register struct chsize *sp;
	register int opl;
	long bit;

	bp = bpfromch(p);
	sp = &Chsizes[bp->b_list];
	debug(DALLOC,printf("Free: addr=%x\n", p));
	bit = 1L << ((p - bp->b_un.b_addr) / sp->ch_bufsize);
	if (!(bp->b_bits & bit)) {
		printf("Free: buffer %x already freed\n", p);
		panic("Chaos buffer already freed");
	}
	bp->b_nfree++;
	bp->b_bits &= ~bit;
	sp->ch_buffree++;
	opl = splimp();
	if (Chbufwait) {
		wakeup((caddr_t)&Chbufwait);
		Chbufwait = 0;
	}
	splx(opl);
}

#ifdef DEBUG_CHAOS
/*
 * Check that address p is in the range of possible allocated packets
 */
ch_badaddr(p)
char *p;
{
	register struct buf *bp;
	register struct chsize *sp;
	register int opl = splimp();

	for (sp = Chsizes; sp < &Chsizes[NSIZES]; sp++)
		for (bp = sp->ch_bufptr; bp; bp = bp->av_forw)
			if (p >= bp->b_un.b_addr &&
			    p < bp->b_un.b_addr + CHBLKSIZE) {
				splx(opl);
				return(0);
			}
	splx(opl);
	return(1);
}
#endif
/*
 * Return the size of the place pointed at by "p"
 */
int
ch_size(p)
char *p;
{
	register struct buf *bp;

	bp = bpfromch(p);
	return (Chsizes[bp->b_list].ch_bufsize);
}
/*
 * Allocate some space when a new connection is created
 */
void
ch_bufalloc()
{
	register int cnt;
	register struct buf *bp;
	struct buf *geteblk();

	if (sizeof(bp->b_bits) != 4)
		panic("challoc bits");
	if (Chnbufs < 8)
		cnt = 4;
	else
		cnt = 1;
	if ((Chnbufs + cnt) > CHMAXBUF)
		return;
	Chnbufs += cnt;
	for (; cnt > 0; cnt--) {
#ifndef vax
#ifdef UCB_BUFOUT
		if (abfreelist.av_forw == &abfreelist)
#else
		if (bfreelist.av_forw == &bfreelist)
#endif
			break;
#endif vax
#ifdef BSD42
		bp = geteblk(CHBLKSIZE);
#else
		bp = geteblk();
#endif
		CHLOCK;
		bp->av_forw = Chbuflist;
		Chbuflist = bp;
		CHUNLOCK;
	}
	Chnbufs -= cnt;
}

void
ch_buffree()
{
	register int cnt;
	register struct buf *bp;

	if (Chnbufs <= 8)
		cnt = 4;
	else
		cnt = 1;
	if (Chnbufs - cnt >= CHMAXBUF)
		return;
	CHLOCK;
	for (; cnt > 0 && (bp = Chbuflist) != NULL; cnt--) {
		Chbuflist = bp->av_forw;
#ifdef	UCB_BUFOUT
		abrelse(bp);
#else
		brelse(bp);
#endif
		Chnbufs--;
	}
	CHUNLOCK;
}
#endif
#endif /* linux */
