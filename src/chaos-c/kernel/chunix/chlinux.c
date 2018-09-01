/*
 * Copied from chaos.c for linux
 * Basically the same functionality as chaos.c, but linux specific.
 * This is the o/s specific wrapper to generic NCP code in ../chncp
 *
 * the model is a pseudo device which "clones" fd's; not right in today's
 * world but it made sense in 1985.  rather than make this a full blown
 * network family I chose to just port the existing functionality.
 *
 * brad@parker.boston.ma.us
 *
 *	$Source: /projects/chaos/kernel/chunix/chlinux.c,v $
 *	$Author: brad $
 *	$Locker:  $
 *	$Log: chlinux.c,v $
 *	Revision 1.3  1999/11/24 18:16:25  brad
 *	added basic stats code with support for /proc/net/chaos inode
 *	basic memory accounting - thought there was a leak but it's just long term use
 *	fixed arp bug
 *	removed more debugging output
 *	
 *	Revision 1.2  1999/11/08 15:28:09  brad
 *	removed/lowered a lot of debug output
 *	fixed bug where read/write would always return zero
 *	still has a packet buffer leak but works ok
 *	
 *	Revision 1.1.1.1  1998/09/07 18:56:08  brad
 *	initial checkin of initial release
 *	
 *
 * Revision 1.6  87/04/06  10:28:05  rc
 * This is essentially the same as revision 1.5 except that the RCS log
 * was not updated to reflect the changes. In particular, calls to spl6()
 * are replaced with calls to splimp() in order to maintain consistency
 * with the rest of the Chaos code.
 * 
 * Revision 1.5  87/04/06  07:52:49  rc
 * Modified this file to include the select stuff for the ChaosNet. 
 * Essentially, the new distribution from "think" was integrated
 * together with the local modifications at MIT to reflect this
 * new version.
 * 
 * Revision 1.4.1.1  87/03/30  21:06:16  root
 * The include files are updated to reflect the correct "include" directory.
 * These changes were not previously RCSed. The original changes did not
 * have lines of the sort ... #include "../netchaos/...."
 * 
 * Revision 1.4  86/10/07  16:16:32  mbm
 * CHIOCGTTY ioctl; move DTYPE_CHAOS to file.h
 * 
 * Revision 1.2  84/11/05  18:24:31  jis
 * Add checks for allocation failure after some calls to pkalloc
 * so that when pkalloc fails to allocate a packet, the system
 * no longer crashes.
 * 
 * Revision 1.1  84/11/05  17:35:32  jis
 * Initial revision
 * 
 */

#ifndef lint
static char *rcsid_chaos_c = "$Header: /projects/chaos/kernel/chunix/chlinux.c,v 1.3 1999/11/24 18:16:25 brad Exp $";
#endif	lint

/*
 * UNIX device driver interface to the Chaos N.C.P.
 */

#include "chaos.h"
#include "chsys.h"
#include "chconf.h"
#include "../chncp/chncp.h"
#include "charp.h"

#ifdef linux

#include <linux/module.h>
/*#include <linux/config.h>*/

#include <linux/types.h>
#include <linux/param.h>

#include <linux/signal.h>
#include <linux/errno.h>
#include <linux/sched.h>
#include <linux/proc_fs.h>

#include <asm/segment.h>
#include <asm/irq.h>
#include <asm/io.h>

#define f_data private_data

static int		initted;	/* NCP initialization flag */
static int		chtimer_running;/* timer is running */

static unsigned long	alloc_count;
static unsigned long	free_count;
static unsigned long	alloc_bytes;
static unsigned long	free_bytes;

int			Rfcwaiting;	/* Someone waiting on unmatched RFC */
struct wait_queue 	*Rfc_wait_queue;/* rfc input wait queue */
int			Chrfcrcv;
int			chdebug;

#define CHIOPRIO	(PZERO+1)	/* Just interruptible */

#ifdef DEBUG_CHAOS
#define ASSERT(x,y)	if(!(x)) printk("%s: Assertion failure\n",y);
#define DEBUGF		if (chdebug) printk
#else
#define ASSERT(x,y)
#define DEBUGF		if (0) printk
#endif

enum { NOTFULL=1, EMPTY };		/* used by chwaitforoutput() */

/*
 * glue between generic file descriptor and chaos connection code
 */
static int chf_read(struct inode *inode, struct file *file,
		    char *buf, int size);
static int chf_write(struct inode *inode, struct file *file,
		     const char *buf, int size);
static int chf_select(struct inode *inode, struct file *file,
		      int which, select_table *seltable);
static int chf_ioctl(struct inode *inode, struct file *file,
		     unsigned int cmd, unsigned long arg);
static void chf_close(struct inode *inode, struct file *file);

struct connection *chopen(struct chopen *c, int wflag, int *errnop);
void chclose(struct connection *conn);
int chread(struct connection *conn, char *ubuf, int size);
int chwrite(struct connection *conn, const char *ubuf, int size);
int chioctl(struct connection *conn, int cmd, caddr_t addr);

static int chwaitforrfc(struct connection *conn, struct packet **ppkt);
static int chwaitforoutput(struct connection *conn, int state);
static int chwaitfornotstate(struct connection *conn, int state);

static struct inode *get_inode(void);
static void free_inode(struct inode *inode);

void chtimeout(void);
void ch_free(char *p);
int ch_size(char *p);
void ch_bufalloc(void);
void ch_buffree(void);

static struct file_operations chfileops = {
  	NULL,		/* lseek */
	chf_read,
	chf_write,
	NULL,		/* readdir */
	chf_select,
	chf_ioctl,
	NULL,		/* mmap */
	NULL,		/* no special open code... */
	chf_close,
	NULL,		/* no fsync */
	NULL		/* fasync */
};

int
chf_read(struct inode *inode, struct file *fp, char *ubuf, int size)
{
	int ret;

	DEBUGF("chf_read(inode=%p, fp=%p)\n", inode, fp);
	ASSERT(fp->f_op == &chfileops, "chf_read ent")
	ret = chread((struct connection *)fp->f_data, ubuf, size);
	ASSERT(fp->f_op == &chfileops, "chf_read exit")
	return ret;
}	

static int
chf_write(struct inode *inode, struct file *fp, const char *ubuf, int size)
{
	int ret;

	DEBUGF("chf_write(inode=%p, fp=%p)\n", inode, fp);
	ASSERT(fp->f_op == &chfileops, "chf_write ent")
	ret = chwrite((struct connection *)fp->f_data, ubuf, size);
	ASSERT(fp->f_op == &chfileops, "chf_write exit")
	return ret;
}	

int
chf_ioctl(struct inode *inode, struct file *fp,
	  unsigned int cmd, unsigned long value)
{
	int ret;

	DEBUGF("chf_ioctl(inode=%p, fp=%p)\n", inode, fp);
	ASSERT(fp->f_op == &chfileops, "chf_ioctl ent")
	ret = chioctl((struct connection *)fp->f_data, cmd, (caddr_t)value);
	ASSERT(fp->f_op == &chfileops, "chf_ioctl exit")
	return ret;
}

/* select support -- Bruce Nemnich, 6 May 85 */
int
chf_select(struct inode *inode, struct file *fp,
	   int sel_type, select_table *wait)
{
  	register struct connection *conn = (struct connection *)fp->f_data;
	int retval = 0;
	ASSERT(fp->f_op == &chfileops, "chf_select ent");

	switch (sel_type) {
		case SEL_IN:
			cli();
			if (!chrempty(conn)) {
				retval = 1;
		        	sti();
			} else {
				conn->cn_sflags |= CHIWAIT;
		        	sti();
				select_wait(&conn->cn_read_wait, wait);
			}
			break;
		case SEL_EX:
			break;
		case SEL_OUT:
			cli();
			if (!chtfull(conn)) {
				retval = 1;
				sti();
			} else {
				conn->cn_sflags |= CHOWAIT;
				sti();
				select_wait(&conn->cn_write_wait, wait);
			}
			break;
	}

	ASSERT(fp->f_op == &chfileops, "chf_select exit")
	return retval;
}


void
chf_close(struct inode *inode, struct file *fp)
{
	register struct connection *conn = (struct connection *)fp->f_data;

	DEBUGF("chf_close(inode=%p, fp=%p) conn %p\n", inode, fp, conn);
	/*
	 * If this connection has been turned into a tty, then the
	 * tty owns it and we don't do anything.
	 */
	
	ASSERT(fp->f_op == &chfileops, "chf_close ent")
	if (conn && conn->cn_mode != CHTTY) {
		chclose(conn);
		fp->f_data = 0;
	}
}

/*
 * character driver access to the Chaos NCP
 * opened for one of two reasons:
 *	1. To perform basic NCP control functions, via minor device CHURFCMIN
 *	2. To open a connection via CHAOSMIN
 * The actual opening of the connection happens later via the CHIOCOPEN ioctl.
 */

static int
chropen(struct inode * inode, struct file * file)
{
	unsigned int minor = MINOR(inode->i_rdev);
	int errno = 0;

        DEBUGF("chropen(inode=%p, fp=%p) minor=%d\n", inode, file, minor);

	MOD_INC_USE_COUNT;

	ch_bufalloc();
	/* initialize the NCP somewhere else? */
	if (!initted) {
		chreset();	/* Reset drivers */
                chtimer_running++;
		chtimeout();	/* Start clock "process" */
		initted++;
	}
	if (minor == CHURFCMIN)
		if(Chrfcrcv == 0)
			Chrfcrcv++;
		else {
			errno = -ENXIO;
			MOD_DEC_USE_COUNT;
		}

        DEBUGF("chropen() returns %d\n", errno);
	return errno;
}

/*
 * Return a connection or return NULL and set *errnop to any error.
 */
struct connection *
chopen(c, wflag, errnop)
struct chopen *c;
int wflag;
int *errnop;
{
	register struct connection *conn;
	register struct packet *pkt;
	int rwsize, length;
        struct chopen cho;

        DEBUGF("chopen(wflag=%d)\n", wflag);

        /* get main structure */
	*errnop = verify_area(VERIFY_READ, (int *)c, sizeof(struct chopen));
	if (*errnop)
		return NOCONN;
	memcpy_fromfs((void *)&cho, (char *)c, sizeof(struct chopen));
        c = &cho;

	length = c->co_clength + c->co_length +
		 (c->co_length ? 1 : 0);
	if (length > CHMAXPKT ||
	    c->co_clength <= 0) {
		*errnop = -E2BIG;
		return NOCONN;
	}
#if 0
	DEBUGF("c->co_length %d, c->co_clength %d, length %d\n",
	       c->co_length, c->co_clength, length);
#endif

	pkt = pkalloc(length, 0);
	if (pkt == NOPKT) {
		*errnop = -ENOBUFS;
		return NOCONN;
	}
	if (c->co_length)
	pkt->pk_cdata[c->co_clength] = ' ';

	memcpy_fromfs(pkt->pk_cdata, c->co_contact, c->co_clength);
	if (c->co_length)
		memcpy_fromfs(&pkt->pk_cdata[c->co_clength + 1], c->co_data,
			      c->co_length);

	rwsize = c->co_rwsize ? c->co_rwsize : CHDRWSIZE;
	pkt->pk_lenword = length;
	conn = c->co_host ? ch_open(c->co_host, rwsize, pkt) : ch_listen(pkt, rwsize);
	if (conn == NOCONN) {
#if 0
		DEBUGF("NOCONN\n");
#endif
		*errnop = -ENXIO;
		return NOCONN;
	}
#if 0
	DEBUGF("c->co_async %d\n", c->co_async);
#endif
	DEBUGF("conn %p\n", conn);
	if (!c->co_async) {
		/*
		 * We should hang until the connection changes from
		 * its initial state.
		 * If interrupted, flush the connection.
		 */

		current->timeout = (unsigned long) -1;

		*errnop = chwaitfornotstate(conn, c->co_host ?
                                            CSRFCSENT : CSLISTEN);
		if (*errnop) {
			rlsconn(conn);
			return NOCONN;
		}

		/*
		 * If the connection is not open, the open failed.
		 * Unless is got an ANS back.
		 */
		if (conn->cn_state != CSOPEN &&
		    (conn->cn_state != CSCLOSED ||
		     (pkt = conn->cn_rhead) == NOPKT ||
		     pkt->pk_op != ANSOP))
		{
#if 0
			DEBUGF("open failed; cn_state %d\n", conn->cn_state);
#endif
			rlsconn(conn);
			*errnop = -EIO;
			return NOCONN;
		}
	}
	if (wflag)
		conn->cn_sflags |= CHFWRITE;
	conn->cn_sflags |= CHRAW;
	conn->cn_mode = CHSTREAM;
        DEBUGF("chopen() done\n");
	return conn;
}

void
chrclose(struct inode * inode, struct file * file)
{
	unsigned int minor = MINOR(inode->i_rdev);

        DEBUGF("chrclose(inode=%p, fp=%p) minor=%d\n", inode, file, minor);
	if (minor == CHURFCMIN) {
		Chrfcrcv = 0;
		freelist(Chrfclist);
		Chrfclist = NOPKT;
	}

	MOD_DEC_USE_COUNT;
}

void
chclose(conn)
register struct connection *conn;
{
	register struct packet *pkt;

	DEBUGF("chclose(%p)\n", conn);
	switch (conn->cn_mode) {
	case CHTTY:
		panic("chclose on tty");
	case CHSTREAM:
		cli();
#if 0
		if (setjmp(&u.u_qsave)) {
			pkt = pktstr(NOPKT, "User interrupted", 16);
			if (pkt != NOPKT)
				pkt->pk_op = CLSOP;
			ch_close(conn, pkt, 0);
			goto shut;
		}
#endif
		if (conn->cn_sflags & CHFWRITE) {
			/*
			 * If any input packets other than the RFC are around
			 * something is wrong and we just abort the connection
			 */
			while ((pkt = conn->cn_rhead) != NOPKT) {
				ch_read(conn);
				if (pkt->pk_op != RFCOP)
					goto recclose;
			}
			/*
			 * We set this flag telling the interrupt time
			 * receiver to abort the connection if any new packets
			 * arrive.
			 */
			conn->cn_sflags |= CHCLOSING;
			/*
			 * Closing a stream transmitter involves flushing
			 * the last packet, sending an EOF and waiting for
			 * it to be acknowledged.  If the connection was
			 * bidirectional, the reader should have already
			 * read until EOF if everything is to be closed
			 * cleanly.
			 */
		checkfull:
			chwaitforoutput(conn, NOTFULL);

			if (conn->cn_state == CSOPEN ||
			    conn->cn_state == CSRFCRCVD) {
				if (conn->cn_toutput) {
					ch_sflush(conn);
					goto checkfull;
				}
				if (conn->cn_state == CSOPEN)
					(void)ch_eof(conn);
			}

			chwaitforoutput(conn, EMPTY);
		} else if (conn->cn_state == CSOPEN) {
			/*
			 * If we are only reading then we should read the EOF
			 * before closing and wait for the other end to close.
			 */
			if (conn->cn_flags & CHEOFSEEN) {
				chwaitfornotstate(conn, CSOPEN);
			}
		}
	recclose:
		sti();
		/* Fall into... */
	case CHRECORD:	/* Record oriented close is just sending a CLOSE */
		if (conn->cn_state == CSOPEN) {
			pkt = pkalloc(0, 0);
			if (pkt != NOPKT) {
				pkt->pk_op = CLSOP;
				pkt->pk_len = 0;
			}
			ch_close(conn, pkt, 0);
		}
	}
shut:
	sti();
	ch_close(conn, NOPKT, 1);
	ch_buffree();
}
/*
 * Raw read routine.
 */
int
chrread(struct inode *inode, struct file *fp, char *buf, int count)
{
  	register struct connection *conn = (struct connection *)fp->f_data;
	unsigned int minor = MINOR(inode->i_rdev);
	struct packet *pkt;
	register int errno = 0;

        DEBUGF("chrread(inode=%p, fp=%p) minor=%d\n", inode, fp, minor);

	/* only CHURFCMIN is readable as char device */
	if(minor == CHURFCMIN) {
		if ((errno = chwaitforrfc(conn, &pkt)) != 0)
			return errno;

		if (count < pkt->pk_len)
			errno = -EIO;
		else {
			memcpy_tofs(buf, (caddr_t)pkt->pk_cdata, pkt->pk_len);
			errno = pkt->pk_len;
		}
	} else
		errno = -ENXIO;

	return errno;
}

static int
chwaitforrfc(struct connection *conn, struct packet **ppkt)
{
	int retval = 0;
	struct wait_queue wait = { current, NULL };

	cli();
	add_wait_queue(&Rfc_wait_queue, &wait);
	while (1) {
		if ((*ppkt = ch_rnext()) != NOPKT)
			break;
		Rfcwaiting++;
		current->state = TASK_INTERRUPTIBLE;
		if (current->signal & ~current->blocked) {
			retval = -ERESTARTSYS;
			break;
		}
//		sleep((caddr_t)&Chrfclist, CHIOPRIO);
		schedule();
	}
	remove_wait_queue(&Rfc_wait_queue, &wait);
	current->state = TASK_RUNNING;
	sti();
	return retval;
}

static int
chwaitfordata(struct connection *conn)
{
	int retval = 0;
	struct wait_queue wait = { current, NULL };

	DEBUGF("chwaitfordata(%p)\n", conn);
	cli();
	add_wait_queue(&conn->cn_read_wait, &wait);
	while (chrempty(conn)) {
		conn->cn_sflags |= CHIWAIT;

		current->state = TASK_INTERRUPTIBLE;
		if (current->signal & ~current->blocked) {
			retval = -ERESTARTSYS;
			break;
		}
		schedule();
//		sleep((char *)&conn->cn_rhead, CHIOPRIO);
	}
	remove_wait_queue(&conn->cn_read_wait, &wait);
	current->state = TASK_RUNNING;
	sti();
	return retval;
}

static int
chwaitforoutput(struct connection *conn, int state)
{
	int retval = 0;
	struct wait_queue wait = { current, NULL };

	cli();
	add_wait_queue(&conn->cn_write_wait, &wait);
	while (1) {
		if ((state == NOTFULL && !chtfull(conn)) ||
		    (state == EMPTY && chtempty(conn)))
		    break;

		conn->cn_sflags |= CHOWAIT;

		current->state = TASK_INTERRUPTIBLE;
		if (current->signal & ~current->blocked) {
			retval = -ERESTARTSYS;
			break;
		}
		schedule();
//		sleep((caddr_t)&conn->cn_thead, CHIOPRIO);
	}
	remove_wait_queue(&conn->cn_write_wait, &wait);
	current->state = TASK_RUNNING;
	sti();
	return retval;
}

static int
chwaitforflush(struct connection *conn, int *pflag)
{
	int retval = 0;
	struct wait_queue wait = { current, NULL };

	cli();
	add_wait_queue(&conn->cn_write_wait, &wait);
	while ((*pflag = ch_sflush(conn)) == CHTEMP) {
		conn->cn_sflags |= CHOWAIT;

		current->state = TASK_INTERRUPTIBLE;
		if (current->signal & ~current->blocked) {
			retval = -ERESTARTSYS;
			break;
		}
		schedule();
//		sleep((char *)&conn->cn_thead, CHIOPRIO);
	}
	remove_wait_queue(&conn->cn_write_wait, &wait);
	current->state = TASK_RUNNING;
	sti();
	return retval;
}

static int
chwaitfornotstate(struct connection *conn, int state)
{
	int retval = 0;
	struct wait_queue wait = { current, NULL };

        DEBUGF("chwaitfornotstate(%p, state=%d)\n", conn, state);
	cli();
	add_wait_queue(&conn->cn_state_wait, &wait);
	while (conn->cn_state == state) {
		current->state = TASK_INTERRUPTIBLE;
		if (current->signal & ~current->blocked) {
			retval = -ERESTARTSYS;
			break;
		}
		schedule();
//		sleep((caddr_t)conn, CHIOPRIO);
	}
	remove_wait_queue(&conn->cn_state_wait, &wait);
	current->state = TASK_RUNNING;
	sti();
        DEBUGF("chwaitfornotstate(state=%d) exit %d\n", state, retval);
	return retval;
}


/*
 * Return an errno on error
 */
int
chread(struct connection *conn, char *ubuf, int size)
{
	register struct packet *pkt;
	register int count, errno;
	
	DEBUGF("chread(%p)\n", conn);
	switch (conn->cn_mode) {
	case CHTTY:
		return -ENXIO;
	case CHSTREAM:
		DEBUGF("chread() CHSTREAM\n");
		if (conn->cn_state == CSRFCRCVD)
			ch_accept(conn);
		for (count = size; size != 0; ) {
			int error, n;

			n = ch_sread(conn, ubuf, (unsigned)size, 0, &error);
			if (error)
				return error;
			switch (n) {
			case 0:	/* No data to read */
				if (count != size)
					return 0;
				if ((errno = chwaitfordata(conn)) != 0)
					return errno;
				continue;
			case CHEOF:
				return 0;
			default:
				if (n < 0)
					return -EIO;
				return n;
			}
		}
		return 0;
	/*
	 * Record oriented mode gives a one byte packet opcode
	 * followed by the data in the packet.  The buffer must
	 * be large enough to fit the data and the opcode, otherwise an
	 * i/o error results.
	 */
	case CHRECORD:
		DEBUGF("chread() CHRECORD size %d\n", size);
		if ((errno = chwaitfordata(conn)) != 0)
			return errno;
#ifdef DEBUG_CHAOS
		if ((pkt = conn->cn_rhead) != NOPKT)
			DEBUGF("chread() CHRECORD pk_len %d, size %d\n",
			       pkt->pk_len, size);
#endif
		if ((pkt = conn->cn_rhead) == NOPKT ||
		    pkt->pk_len + 1 > size)	/* + 1 for opcode */
			return -EIO;
		else {
			int errno;

			errno = verify_area(VERIFY_WRITE, (int *)ubuf,
					    1 + pkt->pk_len);
			if (errno == 0) {
				memcpy_tofs(ubuf, &pkt->pk_op, 1);
				memcpy_tofs(ubuf+1, pkt->pk_cdata,
					    pkt->pk_len);
				errno = pkt->pk_len + 1;

				cli();
				ch_read(conn);
				sti();
			}

			return errno;
		}
	}
	/* NOTREACHED */
}

/*
 * Raw write routine
 * Note that user programs can write to a connection
 * that has been CHIOCANSWER"'d, implying transmission of an ANS packet
 * rather than a normal packet.  This is illegal for TTY mode connections,
 * is handled in the system independent stream code for STREAM mode, and
 * is handled here for RECORD mode.
 */
int
chrwrite(struct inode *inode, struct file *file,
	 const char *buf, int count)
{
	unsigned int minor = MINOR(inode->i_rdev);
        DEBUGF("chrwrite(inode=%p, fp=%p) minor=%d\n", inode, file, minor);
	return -ENXIO;
}

/*
 * Return an errno or 0
 */
int
chwrite(struct connection *conn, const char *ubuf, int size)
{
	register struct packet *pkt;
	int errno;

	DEBUGF("chwrite(%p)\n", conn);
	if (conn->cn_state == CSRFCRCVD)
		ch_accept(conn);
	switch (conn->cn_mode) {
	case CHTTY:
		/* Fall into (on RAW mode only) */
	case CHSTREAM:
		while (size != 0) {
			int n;

			n = ch_swrite(conn, ubuf, (unsigned)size, 0, &errno);
			if (errno)
				return errno;
			switch (n) {
			case 0:
			case CHTEMP:	/* output window is full */
				if ((errno = chwaitforoutput(conn, NOTFULL)) != 0)
					return errno;
				continue;
			default:
				if (n < 0)
					return -EIO;
				return n;
			}
		}
		return 0;
	case CHRECORD:	/* One write call -> one packet */
		if (size < 1 || size - 1 > CHMAXDATA ||
		    conn->cn_state == CSINCT)
		    	return -EIO;

		DEBUGF("chwrite() tlast %d, tacked %d, twsize %d\n",
		       conn->cn_tlast, conn->cn_tacked, conn->cn_twsize);

		if ((errno = chwaitforoutput(conn, NOTFULL)) != 0)
			return errno;

#if 0
		while ((pkt = pkalloc((int)(size - 1), 0)) == NOPKT) {
			/* block if we can't get a packet */
			sleep((caddr_t)&lbolt, CHIOPRIO);
		}
#else
		if ((pkt = pkalloc((int)(size - 1), 0)) == NOPKT)
			return -EIO;
#endif

		memcpy_fromfs(&pkt->pk_op, ubuf, 1);
		pkt->pk_lenword = size - 1;
		if (size)
			memcpy_fromfs(pkt->pk_cdata, ubuf+1, size-1);

		cli();
		if (ch_write(conn, pkt))
			errno = -EIO;
		else
			errno = size;
		sti();

		return errno;
	}
	/* NOTREACHED */
}
/*
 * Raw ioctl routine - perform non-connection functions, otherwise call down
 * errno holds error return value until "out:"
 */
int
chrioctl(struct inode *inode, struct file *fp,
	 unsigned int cmd, unsigned long addr)
{
	unsigned int minor = MINOR(inode->i_rdev);
	int errno = 0;

        DEBUGF("chrioctl(inode=%p, fp=%p) minor=%d\n", inode, fp, minor);
	if (minor == CHURFCMIN) {
		switch(cmd) {
		/*
		 * Skip the first unmatched RFC at the head of the queue
		 * and mark it so that ch_rnext will never pick it up again.
		 */
		case CHIOCRSKIP:
			cli();
			ch_rskip();
			sti();
			break;

		case CHIOCETHER:
			errno = cheaddr(addr);
			break;

		case CHIOCNAME:
			errno = verify_area(VERIFY_READ, (int *)addr,
					    CHSTATNAME);
			if (errno == 0)
				memcpy_fromfs(Chmyname, (char *)addr,
					      CHSTATNAME);
			break;
		/*
		 * Specify my own network number.
		 */
		case CHIOCADDR:
			Chmyaddr = addr;
			break;
		}
	} else {
		int fd;
		struct inode *inode;

		if (cmd != CHIOCOPEN)
			return -ENXIO;

		inode = get_inode();
		if (inode == 0)
			return -ENFILE;

		/* get a file descriptor suitable for return to the user */
		fd = get_unused_fd();
		if (fd >= 0) {
			struct file *file = get_empty_filp();

			if (!file) {
				DEBUGF("no file\n");
				free_inode(inode);
				put_unused_fd(fd);
				return -ENFILE;
			}

			current->files->fd[fd] = file;
			file->f_op = &chfileops;
			file->f_mode = 3;
			file->f_flags = fp->f_flags;
			file->f_inode = inode;
			if (inode) 
				inode->i_count++;
			file->f_pos = 0;

			file->f_data = (caddr_t)chopen((struct chopen *)addr,
						       fp->f_flags &
                                                       (O_WRONLY|O_RDWR),
						       &errno);

			if (file->f_data == NULL) {
				file->f_count--;
			} else
				errno = fd;
		}
	}
	return errno;
}

/*
 * Returns an errno
 */

int
chioctl(conn, cmd, addr)
register struct connection *conn;
int cmd;
caddr_t addr;
{
	register struct packet *pkt;
	int flag, retval;

	DEBUGF("chioctl(%p)\n", conn);
	switch(cmd) {
	/*
	 * Read the first packet in the read queue for a connection.
	 * This call is primarily intended for those who want to read
	 * non-data packets (which are normally ignored) like RFC
	 * (for arguments in the contact string), CLS (for error string) etc.
	 * The reader's buffer is assumed to be CHMAXDATA long.
	 *
	 * An error results if there is no packet to read.
	 * No hanging is currently provided for.
	 * The normal mode of operation for reading such packets is to
	 * first do a CHIOCGSTAT call to find out whether there is a packet
	 * to read (and what kind) and then make this call - except for
	 * RFC's when you know it must be there.
	 */	
	case CHIOCPREAD:
		DEBUGF("CHIOCPREAD\n");
		if ((pkt = conn->cn_rhead) == NULL)
			return -ENXIO;

		retval = verify_area(VERIFY_WRITE, (int *)addr, pkt->pk_len);
		if (retval)
			return retval;

		memcpy_tofs((int *)addr, (caddr_t)pkt->pk_cdata, pkt->pk_len);

		cli();
		ch_read(conn);
		sti();
		return 0;
	/*
	 * Change the mode of the connection.
	 * The default mode is CHSTREAM.
	 */
	case CHIOCSMODE:
		DEBUGF("CHIOCSMODE conn %p\n", conn);
		switch ((int)addr) {
		case CHTTY:
#if NCHT > 0
			if (conn->cn_state == CSOPEN &&
			    conn->cn_mode != CHTTY &&
			    chttyconn(conn) == 0)
				return 0;
#endif
			return -ENXIO;
		case CHSTREAM:
		case CHRECORD:
			if (conn->cn_mode == CHTTY)
				return -EIO;
			conn->cn_mode = (int)addr;
			return 0;
		}
		return -ENXIO;

	/* 
	 * Like (CHIOCSMODE, CHTTY) but return a tty unit to open.
	 * For servers that want to do their own "getty" work.
	 */

      case CHIOCGTTY:
			DEBUGF("CHIOCGTTY\n");

#if NCHT > 0
			if (((conn->cn_state == CSOPEN) ||
			     (conn->cn_state == CSRFCRCVD)) &&
			    conn->cn_mode != CHTTY)
			  {
			    int x = chtgtty(conn);
			    *(int *)(addr) = x;
			    if (x >= 0)
			    {
			      if (conn->cn_state == CSRFCRCVD)
				ch_accept(conn);
			      conn->cn_mode = CHTTY;
			      return 0;
			    }
			  }
#endif
			return -ENXIO;
		
	/*
	 * Flush the current output packet if there is one.
	 * This is only valid in stream mode.
	 * If the argument is non-zero an error is returned if the
	 * transmit window is full, otherwise we hang.
	 */
	case CHIOCFLUSH:
		DEBUGF("CHIOCFLUSH\n");
		if (conn->cn_mode == CHSTREAM) {
			if (addr) {
				flag = ch_sflush(conn);
			} else {
				if ((retval = chwaitforflush(conn, &flag)) != 0)
					return retval;
			}
			return flag ? -EIO : 0;
		}
		return -ENXIO;
	/*
	 * Wait for all output to be acknowledged.  If addr is non-zero
	 * an EOF packet is also sent before waiting.
	 * If in stream mode, output is flushed first.
	 */
	case CHIOCOWAIT:
		DEBUGF("CHIOCOWAIT\n");
		if (conn->cn_mode == CHSTREAM) {
			if ((retval = chwaitforflush(conn, &flag)) != 0)
				return retval;
 			if (flag)
				return -EIO;
		}
		if (addr) {
			if ((retval = chwaitforoutput(conn, NOTFULL)) != 0)
				return retval;
			cli();
			flag = ch_eof(conn);
			sti();
			if (flag)
				return -EIO;
		}
		if ((retval = chwaitforoutput(conn, EMPTY)) != 0)
			return retval;
		return conn->cn_state != CSOPEN ? EIO : 0;
	/*
	 * Return the status of the connection in a structure supplied
	 * by the user program.
	 */
	case CHIOCGSTAT:
		DEBUGF("CHIOCGSTAT conn %p\n", conn);
		if (conn == 0)
			return -ENXIO;

		{
		struct chstatus chst;
		int errno;

		chst.st_fhost = conn->cn_faddr;
		chst.st_cnum = conn->cn_ltidx;
		chst.st_rwsize = conn->cn_rwsize;
		chst.st_twsize = conn->cn_twsize;
		chst.st_state = conn->cn_state;
		chst.st_cmode = conn->cn_mode;
		chst.st_oroom = conn->cn_twsize - (conn->cn_tlast - conn->cn_tacked);
		if ((pkt = conn->cn_rhead) != NOPKT) {
			DEBUGF("pkt %p\n", pkt);
			chst.st_ptype = pkt->pk_op;
			chst.st_plength = pkt->pk_len;
		} else {
			chst.st_ptype = 0;
			chst.st_plength = 0;
		}

		errno = verify_area(VERIFY_WRITE, (int *)addr,
				    sizeof(struct chstatus));
		if (errno)
			return errno;

		memcpy_tofs((int *)addr, (caddr_t)&chst,
			    sizeof(struct chstatus));

		DEBUGF("done\n");
		return 0;
		}
	/*
	 * Wait for the state of the connection to be different from
	 * the given state.
	 */
	case CHIOCSWAIT:
		DEBUGF("CHIOCSWAIT\n");
		if ((retval = chwaitfornotstate(conn, (int)addr)) != 0)
			return retval;
		return 0;
	/*
	 * Answer an RFC.  Basically this call does nothing except
	 * setting a bit that says this connection should be of the
	 * datagram variety so that the connection automatically gets
	 * closed after the first write, whose data is immediately sent
	 * in an ANS packet.
	 */
	case CHIOCANSWER:
		DEBUGF("CHIOCANSWER\n");
		flag = 0;
		cli();
		if (conn->cn_state == CSRFCRCVD && conn->cn_mode != CHTTY)
			conn->cn_flags |= CHANSWER;
		else
			flag = EIO;
		sti();
		return flag;
	/*
	 * Reject a RFC, giving a string (null terminated), to put in the
	 * close packet.  This call can also be used to shut down a connection
	 * prematurely giving an ascii close reason.
	 */
	case CHIOCREJECT:
		DEBUGF("CHIOCREJECT\n");
		{
		struct chreject cr;

		retval = verify_area(VERIFY_READ, (int *)addr,
				    sizeof(struct chreject));
		if (retval)
			return retval;

		memcpy_fromfs(&cr, (int *)addr, sizeof(struct chreject));

		pkt = NOPKT;
		flag = 0;
		cli();
		if (cr.cr_length != 0 &&
		    cr.cr_length < CHMAXPKT &&
		    cr.cr_length >= 0 &&
		    (pkt = pkalloc(cr.cr_length, 0)) != NOPKT)
                  {
			retval = verify_area(VERIFY_READ, (int *)cr.cr_reason,
                                            cr.cr_length);
                        if (retval) {
				ch_free((char *)pkt);
                        	return retval;
                        }

                        memcpy_fromfs(pkt->pk_cdata, cr.cr_reason,
                                      cr.cr_length);

                        pkt->pk_op = CLSOP;
                        pkt->pk_lenword = cr.cr_length;
		}
		ch_close(conn, pkt, 0);
		sti();
		return flag;
		}
	/*
	 * Accept an RFC causing the OPEN packet to be sent
	 */
	case CHIOCACCEPT:
		DEBUGF("CHIOCACCEPT conn %p\n", conn);
		if (conn->cn_state == CSRFCRCVD) {
			ch_accept(conn);
			return 0;
		}
		return -EIO;
	/*
 	 * Count how many bytes can be immediately read.
	 */
	case FIONREAD:
		DEBUGF("FIONREAD\n");
		if (conn->cn_mode != CHTTY) {
			off_t nread = 0;
			int nr, errno;

			for (pkt = conn->cn_rhead; pkt != NOPKT; pkt = pkt->pk_next)
				if (ISDATOP(pkt))
					nread += pkt->pk_len;
			if (conn->cn_rhead != NOPKT)
				nread -= conn->cn_roffset;

			errno = verify_area(VERIFY_WRITE, (int *)addr,
					    sizeof(int));
			if (errno)
				return errno;

			nr = nread;
			memcpy_tofs((int *)addr, (caddr_t)&nr, sizeof(int));
			DEBUGF("FIONREAD returns %d bytes\n", nr);
			return 0;
		}
	}
	return -ENXIO;
}

static struct timer_list chtimer;
static int chtimer_running;

/*
 * Timeout routine that implements the chaosnet clock process.
 * (called periodically)
 */
void
chtimeout()
{
	cli();
	ch_clock();

        if (chtimer_running) {
		if (chtimer.next)
                	del_timer(&chtimer);
		
                chtimer.expires = jiffies + 1;
                chtimer.function = chtimeout;
                chtimer.data = (unsigned long)0;
                add_timer(&chtimer);
        }

	sti();
}

void
chtimeout_stop()
{
	cli();
        chtimer_running = 0;
	if (chtimer.next)
		del_timer(&chtimer);
        sti();
}

/*
 * Routines to wake up processes sleeping on connections.  These were
 * macros before, but they were getting too hairy to be expanded inline.
 * --Bruce Nemnich, 6 May 85
 */

void
chrwake(conn)
register struct connection *conn;
{
    if (conn->cn_sflags & CHCLOSING) {
	conn->cn_sflags &= ~CHCLOSING;
	clsconn(conn, CSCLOSED, NOPKT);
    } else {
	if (conn->cn_sflags & CHIWAIT) {
	    conn->cn_sflags &= ~CHIWAIT;
	    wake_up_interruptible(&conn->cn_read_wait);
//	    wakeup((char *)&conn->cn_rhead);
	}
	if (conn->cn_mode == CHTTY)
	    chtrint(conn);
    }
}

void
chtwake(conn)
register struct connection *conn;
{
    if (conn->cn_sflags & CHOWAIT) {
	conn->cn_sflags &= ~CHOWAIT;
	wake_up_interruptible(&conn->cn_write_wait);
//	wakeup((char *)&conn->cn_thead);
    }
    if (conn->cn_mode == CHTTY)
	chtxint(conn);
}


static struct inode *get_inode(void)
{
	struct inode * inode;

	inode = get_empty_inode();
	if (!inode)
		return NULL;

	inode->i_mode = S_IFSOCK;
	inode->i_sock = 1;
	inode->i_uid = current->uid;
	inode->i_gid = current->gid;

	return inode;
}

static void free_inode(struct inode *inode)
{
	iput(inode);
}

static int praddr(char *buffer, short h)
{
	return sprintf(buffer, "%02d.%03d",
		       ((chaddr *)&h)->ch_subnet & 0377,
		       ((chaddr *)&h)->ch_host & 0377);
}

static int pridle(char *buffer, chtime n)
{
	register int idle;

	idle = Chclock - n;
	if (idle < 0)
		idle = 0;
	if (idle < Chhz)
		return sprintf(buffer, "%3dt", idle);

	idle += Chhz/2;
	idle /= Chhz;
	if (idle < 100)
	    return sprintf(buffer, "%3ds", idle);
	else if (idle < 60*99)
	    return sprintf(buffer, "%3dm", (idle + 30) / 60);

	return sprintf(buffer, "%3dh", (idle + 60*30) / (60*60));
}

int
chaos_get_info(char *buffer, char **start, off_t offset, int length, int dummy)
{
        /* From  net/socket.c  */
        extern int socket_get_info(char *, char **, off_t, int);
        extern struct proto packet_prot;

        int i, len = 0;

        len += sprintf(buffer+len,"Myaddr is 0%o\n", Chmyaddr);

#if 0
	len += sprintf(buffer+len"Connections:\n # T St  Remote Host  Idx Idle Tlast Trecd Tackd Tw Rlast Rackd Rread Rw Flags\n");

	for (i = 0; i < CHNCONNS; i++)
	    if (Chconntab[i] != NOCONN)
		prconn(Chconntab[i], i);

	if (Chrfclist)
	    prrfcs();
#endif

	{
	    struct chxcvr *xp;
	    struct device *dev;
	    extern struct chxcvr chetherxcvr[NCHETHER];

	    for (xp = chetherxcvr; xp->xc_etherinfo.bound_dev; xp++) {
		if (xp->xc_etherinfo.bound_dev == 0)
		    break;
		if ((dev = xp->xc_etherinfo.bound_dev)) {
		    len += sprintf(buffer+len, "%-8s Netaddr: ",
				   /*dev->name*/"eth0");
		    len += praddr(buffer+len,
				  xp->xc_addr);
		    len += sprintf(buffer+len," Devaddr: %x\n",
				   xp->xc_devaddr);

		    len += sprintf(buffer+len,
				   " Received Transmitted CRC(xcvr) CRC(buff) Overrun Aborted Length Rejected\n");
		    len += sprintf(buffer+len,
				   "%9d%12d%10d%8d%8d%8d%8d%8d\n",
				   xp->xc_rcvd, xp->xc_xmtd, xp->xc_crcr,
				   xp->xc_crci, xp->xc_lost, xp->xc_abrt,
				   xp->xc_leng, xp->xc_rej);
		    len += sprintf(buffer+len,
				   " Tpacket Ttime Rpacket Rtime\n");
		    len += sprintf(buffer+len,"%8x ", xp->xc_tpkt);
		    len += pridle(buffer+len,xp->xc_ttime);
		    len += sprintf(buffer+len," %8x ", xp->xc_rpkt);
		    len += pridle(buffer+len,xp->xc_rtime);
		    len += sprintf(buffer+len,"\n");
		}
	    }
	}

	{
		len += sprintf(buffer+len,
			       "alloc  bytes   free   bytes\n");
		len += sprintf(buffer+len,"%6d %7d %6d %7d\n",
			       alloc_count, alloc_bytes,
			       free_count, free_bytes);
		len += sprintf(buffer+len,
			       "in-use bytes\n");
		len += sprintf(buffer+len,"%6d %7d\n",
			       alloc_count - free_count,
			       alloc_bytes - free_bytes);
	}

#if 1
	{
	    extern struct ar_pair charpairs[NPAIRS];
	    register struct ar_pair *app;

		len += sprintf(buffer+len,"Arp table:\n");
		len += sprintf(buffer+len,"Addr   time\n");

		for (app = charpairs; app < &charpairs[NPAIRS]; app++) {
		    if (app->arp_chaos.ch_addr != 0) {
			len += sprintf(buffer+len,"%7o %d\n",
				       app->arp_chaos.ch_addr, app->arp_time);
		    }
		}
	}
#endif


        *start = buffer + offset;
        len -= offset;
        if (len > length)
                len = length;
        return len;
}

static struct file_operations choas_fops = {
	NULL,		/* chaos_lseek */
	chrread,
	chrwrite,
	NULL,		/* chaos_readdir */
	NULL,		/* chaos_select */
	chrioctl,
	NULL,		/* chaos_mmap */
	chropen,
	chrclose
};

int
chaos_init()
{
	DEBUGF("chaos_init()\n");

	if (register_chrdev(CHAOS_MAJOR, "chaos", &choas_fops)) {
		DEBUGF("chaos: unable to get chaos major %d\n", CHAOS_MAJOR);
		return -EIO;
	}

#define PROC_NET_CHAOS PROC_NET_LAST+12
        proc_net_register(&(struct proc_dir_entry) {
                PROC_NET_CHAOS, 5, "chaos",
                S_IFREG | S_IRUGO, 1, 0, 0,
                0, &proc_net_inode_operations,
                chaos_get_info
        });

	DEBUGF("chaos_init() ok\n");
	return 0;
}

void
chaos_deinit()
{
	DEBUGF("chaos_deinit()\n");

        chtimeout_stop();
	chdeinit();
        proc_net_unregister(PROC_NET_CHAOS);
	unregister_chrdev(CHAOS_MAJOR, "chaos");
}

char *
ch_alloc(int size, int cantwait)
{
	char *p;

	if ((p = kmalloc(size + 4, cantwait ? GFP_ATOMIC : GFP_KERNEL))) {
		*(int *)p = size;
		p += 4;
	}

	if (p) {
	  DEBUGF("ch_alloc(%d) = %p\n", size, p-4);

	  alloc_count++;
	  alloc_bytes += size;
	}

	return p;
}

void
ch_free(char *p)
{
	p -= 4;
	DEBUGF("ch_free(%p)\n", p);

	free_count++;
	free_bytes += *(int *)p;

	kfree(p);
}

int
ch_size(char *p)
{
	p -= 4;
	return *(int *)p;
}

void ch_bufalloc() {}
void ch_buffree() {}

char *
chwcopy(from, to, count, uio, errorp)
	char *from, *to;
	unsigned count;
	int *errorp;
{
	*errorp = verify_area(VERIFY_READ, (int *)to, count);
        if (*errorp)
        	return 0;

        memcpy_fromfs(to, from, count);
        return to + count;
}

char *
chrcopy(from, to, count, uio, errorp)
	char *from, *to;
	unsigned count;
	int *errorp;
{
	*errorp = verify_area(VERIFY_WRITE, (int *)to, count);
        if (*errorp)
        	return 0;

        memcpy_tofs(to, from, count);
        return to + count;
}

#ifdef MODULE
/*
 * loadable module initialization
 */

int init_module()
{
	DEBUGF("init_module()\n");
	return chaos_init();
}

void cleanup_module(void)
{
	DEBUGF("cleanup_module()\n");
        chaos_deinit();
}
#endif /* MODULE */

#if 0
int get_unused_fd(void)
{
	int fd;
	struct files_struct * files = current->files;

	fd = find_first_zero_bit(&files->open_fds, NR_OPEN);
	if (fd < current->rlim[RLIMIT_NOFILE].rlim_cur) {
		FD_SET(fd, &files->open_fds);
		FD_CLR(fd, &files->close_on_exec);
		return fd;
	}
	return -EMFILE;
}

inline void put_unused_fd(int fd)
{
	FD_CLR(fd, &current->files->open_fds);
}

static inline void remove_file_free(struct file *file)
{
	struct file *next, *prev;

	next = file->f_next;
	prev = file->f_prev;
	file->f_next = file->f_prev = NULL;
	if (first_file == file)
		first_file = next;
	next->f_prev = prev;
	prev->f_next = next;
}

static inline void put_last_free(struct file *file)
{
	struct file *next, *prev;

	next = first_file;
	file->f_next = next;
	prev = next->f_prev;
	next->f_prev = file;
	file->f_prev = prev;
	prev->f_next = file;
}

struct file * get_empty_filp(void)
{
	int i;
	struct file * f;

	if (!first_file)
		return NULL;

        for (f = first_file, i=0; i < nr_files; i++, f = f->f_next)
        	if (!f->f_count) {
			remove_file_free(f);
			memset(f,0,sizeof(*f));
			put_last_free(f);
			f->f_count = 1;
			f->f_version = ++event;
			return f;
		}

	return NULL;
}
#endif

#endif /* linux */
