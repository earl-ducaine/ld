/*
 *	$Source: /projects/chaos/kernel/chncp/chuser.c,v $
 *	$Author: brad $
 *	$Locker:  $
 *	$Log: chuser.c,v $
 *	Revision 1.2  1999/11/08 15:28:05  brad
 *	removed/lowered a lot of debug output
 *	fixed bug where read/write would always return zero
 *	still has a packet buffer leak but works ok
 *	
 *	Revision 1.1.1.1  1998/09/07 18:56:08  brad
 *	initial checkin of initial release
 *	
 * Revision 1.2  87/04/06  08:28:00  rc
 * The include files are updated to reflect the correct "include" directory.
 * These changes were not previously RCSed. The original changes did not
 * have lines of the sort ... #include "../netchaos/...".
 * 
 * In addition, the call to spl6() is replaced with a call to splimp() in 
 * order to maintain consistency with the rest of the Chaos code.
 * 
 * Revision 1.1  84/06/12  20:27:21  jis
 * Initial revision
 * 
 */
#ifndef lint
static char *rcsid_chuser_c = "$Header: /projects/chaos/kernel/chncp/chuser.c,v 1.2 1999/11/08 15:28:05 brad Exp $";
#endif lint

#include "chaos.h"
#include "../chunix/chsys.h"
#include "../chunix/chconf.h"
#include "chncp.h"

#ifdef linux
#include <asm/system.h>
#define printf printk
#endif

/*
 * User (top level) interface routines. Mostly assumed called from low
 * priority unless otherwise mentioned.
 */
static struct packet *rfcseen;	/* used by ch_rnext and ch_listen */
/*
 * Open a connection (send a RFC) given a destination host a RFC
 * packet, and a default receive window size.
 * Return the connection, on which the RFC has been sent.
 * The connection is not necessarily open at this point.
 */
struct connection *
ch_open(destaddr, rwsize, pkt)
struct packet *pkt;
{
	register struct connection *conn;

	debug(DCONN,printf("ch_open()\n"));
        Chdebug = -1;
        Chdebug = DCONN | DABNOR;
	if ((conn = allconn()) == NOCONN) {
		ch_free((char *)pkt);
		return(NOCONN);
	}
	/*
	 * This could be optimized to use the local address which has
	 * an active transmitter which is on the same subnet as the
	 * destination subnet.  This would involve knowing all the
	 * local addresses - which we don't currently maintain in a
	 * convenient form.
	 */
	conn->cn_laddr = Chmyaddr;
	conn->cn_faddr = destaddr;
	conn->cn_state = CSRFCSENT;
	conn->cn_fidx = 0;
	conn->cn_rwsize = rwsize;
	conn->cn_rsts = rwsize / 2;
	conn->cn_active = Chclock;
	pkt->pk_op = RFCOP;
	pkt->pk_fc = 0;
	pkt->pk_ackn = 0;
	debug(DCONN,printf("Conn #%x: RFCS state\n", conn->cn_lidx));
	/*
	 * By making the RFC packet written like a data packet,
	 * it will be freed by the normal receipting mechanism, enabling
	 * to easily be kept around for automatic retransmission.
	 * xmitdone (first half) and rcvpkt (handling OPEN pkts) help here.
	 * Since allconn clears the connection (including cn_tlast) the
	 * packet number of the RFC will be 1 (ch_write does pkn = ++tlast)
	 */
	CHLOCK;
	(void)ch_write(conn, pkt);	/* No errors possible */
	CHUNLOCK;
        debug(DCONN,printf("ch_open() done\n"));
	return(conn);
}
/*
 * Start a listener, given a packet with the contact name in it.
 * In all cases packet is consumed.
 * Connection returned is in the listen state.
 */
struct connection *
ch_listen(pkt, rwsize)
struct packet *pkt;
{
	register struct connection *conn;
	register struct packet *pktl, *opkt;

        debug(DCONN,printf("ch_listen()\n"));
	if ((conn = allconn()) == NOCONN) {
		ch_free((char *)pkt);
		return(NOCONN);
	}
	conn->cn_state = CSLISTEN;
	conn->cn_rwsize = rwsize;
	pkt->pk_op = LSNOP;
	setpkt(conn, pkt);
	CHLOCK;
	opkt = NOPKT;
	for (pktl = Chrfclist; pktl != NOPKT; pktl = (opkt = pktl)->pk_next)
		if (concmp(pktl, pkt->pk_cdata, (int)pkt->pk_len)) {
			if(opkt == NOPKT)
				Chrfclist = pktl->pk_next;
			else
				opkt->pk_next = pktl->pk_next;
			if (pktl == Chrfctail)
				Chrfctail = opkt;
			if (pktl == rfcseen)
				rfcseen = NOPKT;
			ch_free((char *)pkt);
			lsnmatch(pktl, conn);
			CHUNLOCK;
			return(conn);
		}
	/*
	 * Should we check for duplicate listeners??
	 * Or is it better to allow more than one?
	 */
	pkt->pk_next = Chlsnlist;
	Chlsnlist = pkt;
	debug(DCONN,printf("Conn #%x: LISTEN state\n", conn->cn_lidx));
	CHUNLOCK;
	return(conn);
}
/*
 * Send a new data packet on a connection.
 * Called at high priority since window size check is elsewhere.
 */
int
ch_write(conn, pkt)
register struct connection *conn;
register struct packet *pkt;
{
	debug(DIO,printf("ch_write()\n"));
	switch (pkt->pk_op) {
	case ANSOP:
	case FWDOP:
		if (conn->cn_state != CSRFCRCVD ||
		    (conn->cn_flags & CHANSWER) == 0)
			goto err;
		ch_close(conn, pkt, 0);
		return 0;
	case RFCOP:
	case BRDOP:
		if (conn->cn_state != CSRFCSENT)
			goto err;
		break;
	case UNCOP:
		pkt->pk_pkn = 0;
		setpkt(conn, pkt);
		senddata(pkt);
		return 0;
	default:
		if (!ISDATOP(pkt))
			goto err;
	case OPNOP:
	case EOFOP:
		if (conn->cn_state != CSOPEN)
			goto err;
		setack(conn, pkt);
		break;
	}
	setpkt(conn, pkt);
	pkt->pk_pkn = ++conn->cn_tlast;
	senddata(pkt);
	debug(DIO,printf("ch_write() done\n"));
	return 0;
err:
	ch_free((char *)pkt);
	debug(DIO|DABNOR,printf("ch_write() error\n"));
	return CHERROR;
}
/*
 * Consume the packet at the head of the received packet queue (rhead).
 * Assumes high priority because check for available is elsewhere
 */
void
ch_read(conn)
register struct connection *conn;
{
	register struct packet *pkt;

        debug(DIO,printf("ch_read()\n"));
	if ((pkt = conn->cn_rhead) == NOPKT)
		return;
	conn->cn_rhead = pkt->pk_next;
	if (conn->cn_rtail == pkt)
		conn->cn_rtail = NOPKT;
	if (CONTPKT(pkt)) {
		conn->cn_rread = pkt->pk_pkn;
		if (pkt->pk_op == EOFOP ||
		    3 * (short)(conn->cn_rread - conn->cn_racked) > conn->cn_rwsize) {
			debug(DPKT,
				printf("Conn#%x: rread=%d rackd=%d rsts=%d\n",
				conn->cn_lidx, conn->cn_rread,
				conn->cn_racked, conn->cn_rsts));
			pkt->pk_next = NOPKT;
			makests(conn, pkt);
			reflect(pkt);
			return;
		}
	}
	ch_free((char *)pkt);
}
/*
 * Send an eof packet on a channel.
 */
int
ch_eof(conn)
struct connection *conn;
{
	register struct packet *pkt;
	register int ret = 0;

	if ((pkt = pkalloc(0, 0)) != NOPKT) {
		pkt->pk_op = EOFOP;
		pkt->pk_len = 0;
		ret = ch_write(conn, pkt);
	}
	return ret;
}
/*
 * Close a connection, giving close pkt to send (CLS or ANS).
 */
void
ch_close(conn, pkt, release)
register struct connection *conn;
register struct packet *pkt;
{
#ifdef BSD42
	int s = splimp();
#endif
#ifdef linux
	cli();
#endif
        debug(DCONN,printf("ch_close()\n"));

	switch (conn->cn_state) {
	    case CSOPEN:
	    case CSRFCRCVD:
		if (pkt != NOPKT) {
			setpkt(conn, pkt);
			pkt->pk_ackn = pkt->pk_pkn = 0;
			sendctl(pkt);
			pkt = NOPKT;
		}
		/* Fall into... */
	    case CSRFCSENT:
		clsconn(conn, CSCLOSED, NOPKT);
		break;
	    case CSLISTEN:
		rmlisten(conn);
		break;
	    default:
		break;
	}
	if (pkt)
		ch_free((char *)pkt);

#ifdef BSD
	splx(s);
#endif
#ifdef linux
	sti();
#endif

	if (release)
		rlsconn(conn);
}
/*
 * Top level raw sts sender - at high priority, like ch_write.
 */
void
ch_sts(conn)
struct connection *conn;
{
	sendsts(conn);		/* This must be locked */
}
/*
 * Accept an RFC, called on a connection in the CSRFCRCVD state.
 */
void
ch_accept(conn)
struct connection *conn;
{
	register struct packet *pkt = pkalloc(sizeof(struct sts_data), 0);

	CHLOCK;
	if (conn->cn_state != CSRFCRCVD)
		ch_free((char *)pkt);
	else {
		if (conn->cn_rhead != NOPKT && conn->cn_rhead->pk_op == RFCOP)
			 ch_read(conn);
		conn->cn_state = CSOPEN;
		conn->cn_tlast = 0;
		conn->cn_rsts = conn->cn_rwsize >> 1;
		pkt->pk_op = OPNOP;
		pkt->pk_len = sizeof(struct sts_data);
		pkt->pk_receipt = conn->cn_rlast;
		pkt->pk_rwsize = conn->cn_rwsize;
		debug(DCONN,printf("Conn #%x: open sent\n",conn->cn_lidx));
		(void)ch_write(conn, pkt);
	}
	CHUNLOCK;
}
/*
 * Return the next rfc packet on the list, flushing the one previously
 * looked at if it hasn't been consumed (or skipped) yet.
 * Flushed RFC's get a Close packet sent back.
 * LOCK must be in effect when called. - High priority.
 */
struct packet *
ch_rnext()
{
	register struct packet *pkt, *lpkt;

	if ((pkt = Chrfclist) != NOPKT && rfcseen == pkt) {
		if ((Chrfclist = pkt->pk_next) == NOPKT)
			Chrfctail = NOPKT;
		CHUNLOCK;
		if (pkt->pk_op != BRDOP &&
		    (pkt = pktstr(pkt, "Contact name refused", 20)) != NOPKT) {
			pkt->pk_op = CLSOP;
			pkt->pk_fc = 0;
			pkt->pk_ackn = pkt->pk_pkn = 0;
			reflect(pkt);
		}
		CHLOCK;
		pkt = Chrfclist;
		rfcseen = NOPKT;
	}
	for (lpkt = pkt; pkt != NOPKT && pkt->pk_ackn != 0; pkt = pkt->pk_next)
		lpkt = pkt;
	if (pkt != NOPKT) {
		if (pkt != Chrfclist) {
			Chrfctail->pk_next = Chrfclist;
			Chrfclist = pkt;
			lpkt->pk_next = NOPKT;
			Chrfctail = lpkt;
		}
		rfcseen = pkt;
	}
	return (pkt);
}
/*
 * Skip the RFC at the head of the unmatched-rfc list, and mark it so that
 * ch_rnext never sees it again.  This is to allow an unmatched rfc server
 * to basically say that the RFC should be handled by a specific listener.
 * This would be necessary in the case where two calls for a given
 * specific listener - say login - came so close together that the
 * login server did not get done with the first one, and back inside the
 * listen before the next one came in. Called at low priority from top level.
 * To mark te RFC we set the pk_ackn to non-zero.  It is assured to be zero
 * when first queued.
 */
void
ch_rskip()
{
	register struct packet *pkt;

	if ((pkt = Chrfclist) != NOPKT && rfcseen == pkt) {
		pkt->pk_ackn = 1;
		rfcseen = NOPKT;
	}
}
