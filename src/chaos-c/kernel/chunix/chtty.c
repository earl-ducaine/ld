#include "cht.h"
#if NCHT > 0

#include "chaos.h"
#include "chsys.h"
#include "chconf.h"
#include "../chncp/chncp.h"

#ifdef BSD42
/*#include "vnode.h"		/* was inode */
#include "file.h"
#include "tty.h"
#include "ioctl.h"
#include "dir.h"
#include "user.h"
#include "conf.h"
#include "uio.h"
#endif

#ifdef BSD_TTY

/* mbm -- test for valid connection (tp->t_addr) in chtclose(), chflush() 
 *
 * This file contains functions for treating a chaos channel as a UNIX tty
 */

#if NCHT == 1
#undef NCHT
#define NCHT 32
#endif
struct tty cht_tty[NCHT];
int cht_cnt = NCHT;



chttyconn(conn)
register struct connection *conn;
{
	register struct tty *tp;
	/*
	 * To turn a connection into a tty,
	 * we need to find a tty that is waiting to
	 * be opened and connect ourselves to it.
	 */
	for (tp = cht_tty; tp < &cht_tty[cht_cnt]; tp++)
	 	if (tp->t_addr == 0 &&
		    tp->t_state & TS_WOPEN) {
			tp->t_addr = (caddr_t)conn;
			tp->t_state |= TS_CARR_ON;
			conn->cn_ttyp = tp;
			conn->cn_mode = CHTTY;
			wakeup((caddr_t)&tp->t_rawq);
			return 0;
		}
	return 1;
}

chtgtty(conn)
register struct connection *conn;
{
	register struct tty *tp;
	/*
	 * A different mechanism.  Find a tty that is 
	 * free, hook it up, and return its unit.
	 */
	for (tp = cht_tty; tp < &cht_tty[cht_cnt]; tp++)
	 	if (tp->t_addr == 0 && (tp->t_state & TS_CARR_ON) == 0) {
			tp->t_addr = (caddr_t)conn;
			tp->t_state |= TS_CARR_ON;
			conn->cn_ttyp = tp;
/*			conn->cn_mode = CHTTY;*/
			return (tp - cht_tty);
		}
	return (-1);
}

/*
 * Do the additional work necessary to open a channel as a UNIX tty.
 * Basically the existence of an associated connection is much like
 * the existence of a carrier.
 */
/* ARGSUSED */
chtopen(dev, flag)
	dev_t dev;
{
	register struct tty *tp;
	register int unit;
	int chtstart(), chtinput();

	unit = minor(dev);
	if (unit >= cht_cnt) {
		return(ENXIO);
	}
	tp = &cht_tty[unit];
	if (tp->t_state & TS_XCLUDE && u.u_uid != 0) {
		return( EBUSY);
	}
	tp->t_state |= TS_WOPEN;
	tp->t_oproc = chtstart;
/*	tp->t_iproc = chtinput;*/
	if ((tp->t_state&TS_ISOPEN) == 0) {
		ttychars(tp);
		tp->t_ispeed = B9600;
		tp->t_ospeed = B9600;
		tp->t_flags = ODDP|EVENP|ECHO;
		tp->t_state |= TS_HUPCLS;
	}
/*	tp->t_lstate |= LSCHAOS;*/
	CHLOCK;
	/*
	 * Since ttyclose forces CARR_ON off, we turn it on again if
	 * the connection is still around.
	 */
/*

Nope.  This lets a new getty reopen a chtty on a closed connection. --mbm

	if (tp->t_addr)
		tp->t_state |= TS_CARR_ON;
	else
*/
	if (flag & O_NDELAY)
/*	  tp->t_state |= TS_ONDELAY*/ ;
	else
	  while ((tp->t_state & TS_CARR_ON) == 0)
	    sleep((caddr_t)&tp->t_rawq, TTIPRI);
	CHUNLOCK;
	tp->t_line = NTTYDISC;
	return((*linesw[tp->t_line].l_open)(dev, tp));
}
/*
 * Close the tty associated with the given connection.
 */
chtclose(dev, flag)
	dev_t dev;
	int flag;
{
	register struct connection *conn;
	register struct tty *tp;

	tp = &cht_tty[minor(dev)];
	(*linesw[tp->t_line].l_close)(tp);
	conn = (struct connection *)tp->t_addr;
	if (conn && 	/* mbm */
		 (tp->t_state & TS_HUPCLS || conn->cn_state != CSOPEN)) {
		chuntty(conn);
		tp->t_addr = 0;
		tp->t_state &= ~TS_CARR_ON;
/*		tp->t_lstate &= ~LSCHAOS;*/
	}
	ttyclose(tp);
}

/*
 * Read from a chaos channel that is a tty.
 */
chtread(dev,uio)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp = &cht_tty[minor(dev)];
	register struct connection *conn = (struct connection *)tp->t_addr;
	/*
	 * Since ttys are quite possibly interactive, be sure
	 * to flush any output when input is desired.  It would
	 * be soon anyway due to timeouts.
	 */

	if ((conn == 0) || ((tp->t_state & TS_CARR_ON) == 0))
	{
	  if (conn)
	  {
	    chuntty(conn);
	    tp->t_addr = 0;
	  }
	  return(EIO);
	}

	splimp();
	if (conn->cn_toutput != NOPKT && !chtfull(conn))
		ch_sflush(conn);
	spl0();
	return((*linesw[tp->t_line].l_read)(tp,uio));
}

/*
 * Write to a chaos channel that is a tty.
 */
chtwrite(dev,uio)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp = &cht_tty[minor(dev)];
	register struct connection *conn = (struct connection *)tp->t_addr;

	if ((conn == 0) || ((tp->t_state & TS_CARR_ON) == 0))
	{
	  if (conn)
	  {
	    chuntty(conn);
	    tp->t_addr = 0;
	  }
	  return(EIO);
	}
	if (tp->t_flags & RAW) {
	  register x ;
	  x = chwrite((struct connection *)tp->t_addr,uio);
	  if (x)
	    return x;
	  x= splimp();	
	  if (conn && conn->cn_toutput != NOPKT && !chtfull(conn))
	    ch_sflush((struct connection *)tp->t_addr);
	  splx(x);
	  return 0;
	} 
	else
	  return((*linesw[tp->t_line].l_write)(tp,uio));

}
/*
 * We only allow tty ioctl's for a chaos tty.
 * We could also allow chaos ioctl's if needed.
 */
chtioctl(dev, cmd, addr, flag)
	dev_t dev;
	caddr_t addr;
{
	register struct tty *tp = &cht_tty[minor(dev)];
	register int com;

	com = cmd;
	cmd = (*linesw[tp->t_line].l_ioctl)(tp, cmd, addr,flag);
	if (cmd >= 0)
		return cmd;
	cmd = ttioctl(tp, com, addr, flag);
	if (cmd >= 0)
		return cmd;
	else switch(com) {
	/*
	 * We need a remote terminal protocol here. - Yick.
	 */
	case TIOCSBRK:
		/* maybe someday a code for break on output ? */
		break;
	case TIOCCBRK:
		break;
	case TIOCSDTR:
		break;
	case TIOCCDTR:
		break;
	case TIOCSETP:
	case TIOCSETN:
		break;
			/*
			 * Send virtual terminal codes here?
			 */
/*			chparam(conn); */
	}
}
/*
 * Interrupt routine called when a new packet is available for a tty channel
 * Basically empty the packet into the tty system.
 * Called both from interrupt level when the read packet queue becomes
 * non-empty and also (at high priority) from top level.  THe top level
 * call is needed to retrieve data not queued at interrupt time due to
 * input queue high water mark reached.
 */
chtrint(conn)
struct connection *conn;
{
	int x = splimp();
	register struct packet *pkt;
	register struct tty *tp = conn->cn_ttyp;
	register char *cp;

	while ((pkt = conn->cn_rhead) != NOPKT) {
		if (ISDATOP(pkt) && conn->cn_state == CSOPEN)
			for (cp = &pkt->pk_cdata[conn->cn_roffset];
			     pkt->pk_len != 0; pkt->pk_len--)
				if (tp->t_rawq.c_cc + tp->t_canq.c_cc >=
				    TTYHOG) {
					conn->cn_roffset = cp - pkt->pk_cdata;
					return;
				} else
					(*linesw[tp->t_line].l_rint)
						(*cp++ & 0377, tp);
		ch_read(conn);
	}
	/*
	 * Flush any output since we might have echoed something at
	 * interrupt level.
	 */
	if (conn->cn_toutput != NOPKT && !chtfull(conn))
		ch_sflush(conn);
}
/*
 * Get more data if possible.  This is called from ttread (ntread) to
 * see if more data can be read from the connection.
 * It is only needed to account for the case where packets are not
 * completely emptied at interrupt level due to input clist buffer
 * overflow (>TTYHOG).  In this respect, chaos connections win better
 * than ttys, which just throw the data away.
 */
chtinput(tp)
register struct tty *tp;
{
	register int opl = splimp();

	chtrint((struct connection *)tp->t_addr);
	splx(opl);
}
/*
 * Interrupt routine called when more packets can again be sent after things
 * block due to window full.
 */
chtxint(conn)
register struct connection *conn;
{
	register struct tty *tp = conn->cn_ttyp;
	register int s = splimp();

	tp->t_state &= ~TS_BUSY;
	if (tp->t_outq.c_cc<=TTLOWAT(tp)) {
		if (tp->t_state&TS_ASLEEP) {
			tp->t_state &= ~TS_ASLEEP;
			wakeup((caddr_t)&tp->t_outq);
		}
		if (tp->t_wsel) {
			selwakeup(tp->t_wsel, tp->t_state & TS_WCOLL);
			tp->t_wsel = 0;
			tp->t_state &= ~TS_WCOLL;
		}
	}

	if (tp->t_line)
		(*linesw[tp->t_line].l_start)(tp); 
	else
		chtstart(tp);
	splx(s);
}
/*
 * Are we output flow controlled?
 */
chtblocked(tp)
	struct tty *tp;
{
	register struct connection *conn = (struct connection *)tp->t_addr;

	return chtfull(conn);
}
/*
 * Are we empty on output?
 */
chtnobuf(tp)
struct tty *tp;
{
	register struct connection *conn = (struct connection *)tp->t_addr;

	return !conn->cn_toutput;
}
/*
 * Flush any buffered output that we can.
 * Called from high priority.
 */
chtflush(tp)
struct tty *tp;
{
	register struct connection *conn = (struct connection *)tp->t_addr;

	
	if (conn &&  /* mbm */
		conn->cn_toutput) { 	
		ch_free((caddr_t)conn->cn_toutput);
		conn->cn_toutput = NOPKT;
	}
}

/* restart transmission */
chtstart(tp)
register struct tty *tp;
{
	register struct connection *conn = (struct connection *)tp->t_addr;
	register struct packet *pkt;
	int sps, cc;
	extern int ttrstrt();

	sps = splimp();
	if (conn->cn_state == CSOPEN &&
	    (tp->t_state & (TS_TIMEOUT|TS_BUSY|TS_TTSTOP)) == 0) {
		while (tp->t_outq.c_cc != 0) {
			if ((pkt = conn->cn_toutput) == NOPKT) {
				cc = chtfull(conn) ? CHMAXDATA :
					chroundup(tp->t_outq.c_cc * 2);
				if ((pkt = pkalloc(cc, 1)) == NOPKT) {
					cc = CHMINDATA;
					if ((pkt = pkalloc(cc, 1)) == NOPKT) {
						timeout(ttrstrt, (caddr_t)tp,
							CHBUFTIME);
						tp->t_state |= TS_TIMEOUT;
						splx(sps);
						return;
					}
				}
				pkt->pk_op = DATOP;
				pkt->pk_type = 0;
				pkt->pk_lenword = 0;
				conn->cn_troom = cc;
				conn->cn_toutput = pkt;
			}
			if ((cc = tp->t_outq.c_cc) > conn->cn_troom)
				cc = conn->cn_troom;
			if (cc != 0) {
				(void)q_to_b(&tp->t_outq,
					&pkt->pk_cdata[pkt->pk_len], cc);
				pkt->pk_time = Chclock;
				pkt->pk_lenword += cc;
			}
			if (tp->t_outq.c_cc<=TTLOWAT(tp)) {
			  if (tp->t_state&TS_ASLEEP) {
			    tp->t_state &= ~TS_ASLEEP;
			    wakeup((caddr_t)&tp->t_outq);
			  }
			  if (tp->t_wsel) {
			    selwakeup(tp->t_wsel, tp->t_state & TS_WCOLL);
			    tp->t_wsel = 0;
			    tp->t_state &= ~TS_WCOLL;
			  }
			}

			if ((conn->cn_troom -= cc) == 0)
				if (chtfull(conn)) {
					tp->t_state |= TS_BUSY;
					splx(sps);
					return;
				} else
					ch_sflush(conn);
		}
		/*
		 * Here we must deal with a partially full packet.
		 * For now, just get rid of it.
		 * We might schedule a timeout flush or something.
		 * Or maybe check if we are at interrupt level...
		 */
		if (conn->cn_toutput)
			if (chtfull(conn))
				tp->t_state |= TS_BUSY;
			else
				ch_sflush(conn);
	/*	*/
	}
	splx(sps);
}

/*
 * Put out one character and return non-zero if we couldn't
 */
chtputc(c, tp)
char c;
struct tty *tp;
{
	return chtout(&c, 1, tp);
}
/*
 * Send a contiguous array of bytes.
 * Return the number we can't accept now.
 * Packet allocation strategy:
 * We allocate a packet that is at least large enough to hold all bytes
 * remaining to be sent in this system call. We rely on the fact that
 * packets are really powers of two in size and at least 16 bytes of data,
 * since we don't round up our request at all.
 * If our "clever" request fails, we try for a small packet.
 */
chtout(cp, cc, tp)
	register char *cp;
	struct tty *tp;
{
	register struct connection *conn = (struct connection *)tp->t_addr;
	register struct packet *pkt;
	register int n;
	int sps = splimp();
	extern int ttrstrt();

	if (conn->cn_state == CSOPEN &&
	    (tp->t_state & (TS_TIMEOUT|TS_BUSY|TS_TTSTOP)) == 0) {
		while (cc != 0) {
			if ((pkt = conn->cn_toutput) == NOPKT) {
				if (chtfull(conn))
					n = CHMAXDATA;
				else {
/* Nope. This can be called from interrupt level. 
					n = chroundup(u.u_count + cc);
*/					n = chroundup (cc);
					if (n > CHMAXDATA) n = CHMAXDATA;
				}
				if ((pkt = pkalloc(n, 1)) == NOPKT) {
					n = CHMINDATA;
					if ((pkt = pkalloc(n, 1)) == NOPKT)
						break;
				}
				pkt->pk_op = DATOP;
				pkt->pk_type = 0;
				pkt->pk_lenword = 0;
				conn->cn_troom = n;
				conn->cn_toutput = pkt;
			}
			if ((n = cc) > conn->cn_troom)
				n = conn->cn_troom;
			if (n != 0) {
				chmove(cp, &pkt->pk_cdata[pkt->pk_lenword], n);
				pkt->pk_time = Chclock;
				pkt->pk_lenword += n;
				cp += n;
			}
			cc -= n;
			if ((conn->cn_troom -= n) == 0)
				if (chtfull(conn))
					break;
				else
					ch_sflush(conn);
		}
	}
	splx(sps);
	return (cc);
}
/*
 * Process a connection state change for a tty.
 */
chtnstate(conn)
register struct connection *conn;
{
	register struct tty *tp;

	tp = conn->cn_ttyp;
	switch (conn->cn_state) {
	/*
	 * This shouldn't really happen since the connection shouldn't
	 * become a tty until it is open.
	 */
	case CSOPEN:
		if ((tp->t_state & TS_CARR_ON) == 0) {
			wakeup((caddr_t)&tp->t_rawq);
			tp->t_state |= TS_CARR_ON;
		}
		break;
	case CSCLOSED:
	case CSINCT:
	case CSLOST:
		if (tp->t_state & TS_CARR_ON) {
			if ((tp->t_flags & NOHANG) == 0 &&
			    tp->t_state & TS_ISOPEN) {
				gsignal(tp->t_pgrp, SIGHUP);
#ifdef SIGCONT
				gsignal(tp->t_pgrp, SIGCONT);
#endif
				ttyflush(tp, FREAD|FWRITE);
			}
			tp->t_state &= ~TS_CARR_ON;
		} 
		break;

	default:
		panic("chtnstate");
	}
}


chuntty(conn)
register struct connection *conn;
{
  /* Put the connection back in record mode.  This will let the chaos
     device side close it.  If there is no chaos device reference to 
     the connection, close it now.
  */

  register struct file *fp;

  conn->cn_mode = CHRECORD;
  for (fp = file; fp < fileNFILE; fp++) {
    if (fp->f_type != DTYPE_CHAOS)
      continue;
    if (fp->f_count && (((struct connection *)(fp->f_data) == conn)))
      return;
  }
  chclose(conn);
}
#endif /* BSD_TTY */

#endif /* NCHT > 0 */

