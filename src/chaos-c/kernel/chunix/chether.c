/*
 *	$Source: /projects/chaos/kernel/chunix/chether.c,v $
 *	$Author: brad $
 *	$Locker:  $
 *	$Log: chether.c,v $
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
 * Revision 1.3  85/11/12  14:00:47  mbm
 * IF_ADJ() in chpktin() for 4.3
 * 
 * Revision 1.2  85/09/07  12:06:29  root
 * - Changed ARP (#define) to 4.3 spelling.
 * - Added include of "in.h", since one of its structures is declared in
 *   if_ether.h.  (How did it EVER compile?)
 * 
 * Revision 1.2  85/09/07  11:38:53  root
 * Changed ARP define to 4.3 version.
 * 
 * Revision 1.1  84/06/12  20:06:04  jis
 * Initial revision
 * 
 */
#ifndef lint
static char *rcsid_chether_c = "$Header: /projects/chaos/kernel/chunix/chether.c,v 1.3 1999/11/24 18:16:25 brad Exp $";
#endif lint

#include "chaos.h"
#include "chsys.h"
#include "chconf.h"
#include "../chncp/chncp.h"
#include "challoc.h"
#include "charp.h"

#ifdef BSD42
#include "mbuf.h"
#include "errno.h"
#include "socket.h"
#include "net/if.h"
#include "netinet/in.h"
#include "netinet/if_ether.h"
#endif

#ifdef linux
#include <linux/signal.h>
#include <linux/errno.h>
#include <linux/sched.h>
#include <linux/proc_fs.h>
#include <linux/if_arp.h>

#include <asm/byteorder.h>

#define ETHERTYPE_ARP ETH_P_ARP
#endif

#ifdef DEBUG_CHAOS
extern int		chdebug;
#define DEBUGF		if (chdebug) printf
#else
#define ASSERT(x,y)
#define DEBUGF		if (0) printf
#endif

#ifdef linux
#define printf printk
#endif

void chedeinit(void);

/*
 * This file provides the glue between the 4.2 BSD ethernet device drivers and
 * the Chaos NCP.
 */
struct chxcvr chetherxcvr[NCHETHER];
struct ar_pair charpairs[NPAIRS];
long	charptime = 1;	/* LRU clock for ar_pair slots */

#ifdef BSD_INTF

/*
 * Process an incoming address resolution packet for the Chaosnet.
 * Stay clear of chaos packet buffers altogether.
 *
 * The defines below kludge around the krufty IP specific definitions
 * of the address resolution formats.
 */
#define arp_scha(arp)	((chaddr *)(arp)->arp_spa)->ch_addr
#define arp_tcha(arp)	((chaddr *)&(arp)->arp_tpa[-2])->ch_addr
#define arp_tea(arp)	((arp)->arp_tha - 2)

charpin(m, ac)
struct mbuf *m;
struct arpcom *ac;
{
	register struct ether_arp *arp = mtod(m, struct ether_arp *);
	short mychaddr;

	if (m->m_len < sizeof(struct ether_arp) ||
	    ntohs(arp->arp_hrd) != ARPHRD_ETHER ||
	    ntohs(arp->arp_pro) != ETHERTYPE_CHAOS ||
	    arp->arp_pln != sizeof(chaddr) ||
	    arp->arp_hln != ELENGTH ||
	    arp_scha(arp) == 0 ||
	    arp_tcha(arp) == 0)
		goto freem;
	{
		register struct chxcvr *xp;

		for (xp = chetherxcvr; xp->xc_etherinfo.che_arpcom != ac; xp++)
			if (xp == &chetherxcvr[NCHETHER - 1])
				goto freem;
		mychaddr = xp->xc_addr;
	}
	{
		register struct ar_pair *app;
		register struct ar_pair *nap = charpairs;
		u_char *eaddr = 0;

		for (app = nap; app < &charpairs[NPAIRS]; app++)
			if (arp_scha(arp) == app->arp_chaos.ch_addr) {
				eaddr = app->arp_ether;
				break;
			} else if (app->arp_time < nap->arp_time) {
				nap = app;
				if (app->arp_chaos.ch_addr == 0)
					break;
			}
		/*
		 * If we are already cacheing the senders addresses,
		 * update our cache with possibly new information.
		 */
		if (eaddr) {
			nap = app;
			bcopy(arp->arp_sha, app->arp_ether, ELENGTH);
		}
		if (arp_tcha(arp) != mychaddr)
			goto freem;
		/*
		 * If we have never heard of this host before, find
		 * a slot and remember him.
		 * We set the time to 1 to be better than empty entries
		 * but worse than any that have actually been used.
		 * This is to prevent other hosts from flushing our cache
		 */
		if (eaddr == 0) {
			bcopy(arp->arp_sha, nap->arp_ether, ELENGTH);
			nap->arp_chaos.ch_addr = arp_scha(arp);
			nap->arp_time = 1;
		}
	}
	if (ntohs(arp->arp_op) == ARPOP_REQUEST) {
		struct sockaddr sa;
		register struct ether_header *eh;

		arp_tcha(arp) = arp_scha(arp);
		arp_scha(arp) = mychaddr;
		eh = (struct ether_header *)sa.sa_data;
		bcopy(arp->arp_sha, eh->ether_dhost, ELENGTH);
		bcopy(arp->arp_sha, arp_tea(arp), ELENGTH);
		bcopy(ac->ac_enaddr, arp->arp_sha, ELENGTH);
		arp->arp_op = htons(ARPOP_REPLY);
		eh->ether_type = ETHERTYPE_ARP;	/* if_output will swap it! */
		sa.sa_family = AF_UNSPEC;
		(*ac->ac_if.if_output)(&ac->ac_if, m, &sa);
		return;
	}
freem:
	m_freem(m);
}			

/*
 * Output a chaos packet on a 4.2 BSD ethernet driver.
 * Note that 4.2BSD cannot support the notion of putting something
 * at the head of the transmit queue, so we ignore this.
 */
/* ARGSUSED */
cheoutput(xcvr, pkt, head)
struct chxcvr *xcvr;
register struct packet *pkt;
{
	struct arpcom *ac = xcvr->xc_etherinfo.che_arpcom;
	register struct mbuf *m = 0;
	int resolving = 1;
	struct sockaddr sa;
	struct ether_header *eh;

	xcvr->xc_xmtd++;
	eh = (struct ether_header *)sa.sa_data;
	sa.sa_family = AF_UNSPEC;
	MGET(m, M_DONTWAIT, MT_DATA);
	if (m == NULL) {
	  printf("cheoutput: no mbufs, cant xmt pkt %X\n", pkt);
		xmitdone(pkt);
		return;
	}
	if (pkt->pk_xdest == 0) {
		eh->ether_type = ETHERTYPE_CHAOS;
		bcopy(etherbroadcastaddr, eh->ether_dhost, ELENGTH);
		resolving = 0;
	} else {
		register struct ar_pair *app;

		for (app = charpairs;
		     app < &charpairs[NPAIRS] && app->arp_chaos.ch_addr; app++)
			if (pkt->pk_xdest == app->arp_chaos.ch_addr) {
				app->arp_time = ++charptime;
				eh->ether_type = ETHERTYPE_CHAOS;
				bcopy(app->arp_ether, eh->ether_dhost, ELENGTH);
				resolving = 0;
				break;
			}
	}
	if (resolving) {
		register struct ether_arp *arp;

		eh->ether_type = ETHERTYPE_ARP;
		bcopy(etherbroadcastaddr, eh->ether_dhost, ELENGTH);
		m->m_len = sizeof(struct ether_arp);
		m->m_off = MMAXOFF - sizeof(struct ether_arp);
		arp = mtod(m, struct ether_arp *);
		arp->arp_hrd = htons(ARPHRD_ETHER);
		arp->arp_pro = htons(ETHERTYPE_CHAOS);
		arp->arp_hln = ELENGTH;
		arp->arp_pln = sizeof(chaddr);
		arp->arp_op = htons(ARPOP_REQUEST);
		bcopy((caddr_t)ac->ac_enaddr, (caddr_t)arp->arp_sha, ELENGTH);
		arp_scha(arp) = xcvr->xc_addr;
		arp_tcha(arp) = pkt->pk_xdest;
		bcopy(etherbroadcastaddr, arp_tea(arp), ELENGTH);
	} else {
		register struct mbuf *p = chdtom(pkt);

		m->m_len = pkt->pk_len + sizeof(struct pkt_header);
		if (p->m_off >= MSIZE) {
			m->m_off = (int)(&pkt->pk_phead) - (int)m;
			m->m_cltype = 1; /* Needed with WISC NFS mbuf hacks */
			++mclrefcnt[mtocl(p)];
		} else {
			m->m_off = MMAXOFF - m->m_len;
			bcopy((char *)&pkt->pk_phead, mtod(m, char *), m->m_len);
		}
	}
	xmitdone(pkt);
	(*ac->ac_if.if_output)(&ac->ac_if, m, &sa);
}
/*
 * Receive a packet from a 4.2BSD network driver.
 * If we're lucky, this is fast, but many cases must be handled.
 * It is assumed that
 *  CHMAXDATA + sizeof(ncp_header) + sizeof(pkt_header) + MSIZE
 * is less than CLBYTES.
 */
chpktin(m, ac)
register struct mbuf *m;
struct arpcom *ac;
{
	struct pkt_header *php;
	int  chlength;
	register struct chxcvr *xp;

	for (xp = chetherxcvr; xp->xc_etherinfo.che_arpcom != ac; xp++)
		if (xp >= &chetherxcvr[NCHETHER - 1]) {
			m_freem(m);
			return;
		}
	IF_ADJ(m);
	/*
	 * First handle the easy cases.
	 */
	php = mtod(m, struct pkt_header *);
	chlength = php->ph_len + sizeof(struct pkt_header);
	if (m->m_len < sizeof(struct pkt_header) ||
	    chlength > CHMAXDATA + sizeof(struct pkt_header)) {
		m_freem(m);
		return;
	}
	if (m->m_next == 0) {
		/*
		 * Incoming packet is in a single mbuf.
		 */
		if (m->m_off < MSIZE)
			if (m->m_off >= MMINOFF + sizeof(struct ncp_header))
				m->m_off -= sizeof(struct ncp_header);
			else if (chlength <= MLEN - sizeof(struct ncp_header)) {
				bcopy((char *)php, &m->m_dat[sizeof(struct ncp_header)],
					chlength);
				m->m_off = MMINOFF;
			} else
				goto copyout;
		else {
			struct mbuf *mm;

			m->m_off = 0;
			MFREE(m, mm);
			m = (struct mbuf *)((int)php & ~CLOFSET);
			bcopy(php,
			      (char *)m + MSIZE + sizeof(struct ncp_header),
			      chlength);
			m->m_off = MSIZE;
		}
		m->m_len = chlength + sizeof(struct ncp_header);
	} else {
		register char *cp;
		struct packet *pkt;
		int nbytes;
copyout:
		if ((pkt = (struct packet *)
			   ch_alloc(chlength + sizeof(struct ncp_header), 1)
			   ) == NULL) {
			m_freem(m);
			return;
		}
		cp = (char *)&pkt->pk_phead;
		nbytes = chlength;
		do {
			struct mbuf *mp;

			if (nbytes > 0) {
				bcopy(mtod(m, char *), cp, MIN(m->m_len, nbytes));
				cp += m->m_len;
				nbytes -= m->m_len;
			}
			MFREE(m, mp);
			m = mp;
		} while (m);
		if (nbytes > 0) {
			ch_free((char *)pkt);
			return;
		}
		m = chdtom(pkt);
	}
	xp->xc_rpkt = mtod(m, struct packet *);
	rcvpkt(xp);
}

/*
 * Nothing happens at initialization time.
 */
cheinit() {}
chereset() {}
chestart() {}

/*
 * Handle the CHIOCETHER ioctl to assign a chaos address to an
 * ethernet interface.  Note that all initialization is done here.
 */
cheaddr(addr)
char *addr;
{
	extern struct ifnet *ifunit();
	register struct chether *che = (struct chether *)addr;
	register struct ifnet *ifp = ifunit(che->ce_name);
	register struct chxcvr *xp;

	if (ifp == 0)
		return ENXIO;
	for (xp = chetherxcvr; xp->xc_etherinfo.che_arpcom; xp++)
		if (xp->xc_etherinfo.che_arpcom == 0 ||
		    &xp->xc_etherinfo.che_arpcom->ac_if == ifp)
			break;
		else if (xp == &chetherxcvr[NCHETHER - 1])
			return EIO;
	xp->xc_etherinfo.che_arpcom = (struct arpcom *)ifp;
	xp->xc_addr = che->ce_addr;
	xp->xc_cost = CHCCOST;
	xp->xc_xmit = cheoutput;
	xp->xc_reset = chereset;
	xp->xc_start = chestart;
	printf("chether: %s enabled as Chaosnet address 0%o\n", ifp->if_name, che->ce_addr);
	chattach(xp);
	return 0;
}

#endif /* BSD_INTF */

#ifdef linux

#define arp_sha(arp)	(           (((char *)(arp+1))      ) )
#define arp_scha(arp)	((chaddr *) (((char *)(arp+1))+6    ) )->ch_addr
#define arp_tea(arp)	(           (((char *)(arp+1))+6+2  ) )
#define arp_tcha(arp)	((chaddr *) (((char *)(arp+1))+6+2+6) )->ch_addr

int charpin(struct sk_buff *skb, struct device *dev, struct packet_type *pt)
{
	struct arphdr *arp = (struct arphdr *)skb->h.raw;
/*	unsigned char *arp_ptr = (unsigned char *)(arp+1); */
	short mychaddr;

	if (ntohs(arp->ar_hrd) != ARPHRD_ETHER ||
            ntohs(arp->ar_pro) != ETHERTYPE_CHAOS ||
            dev->type != ntohs(arp->ar_hrd) ||
            arp->ar_pln != sizeof(chaddr) ||
	    arp->ar_hln != ELENGTH ||
            arp->ar_hln != dev->addr_len || 
            dev->flags & IFF_NOARP ||
	    arp_scha(arp) == 0 ||
	    arp_tcha(arp) == 0)
	{
		kfree_skb(skb, FREE_READ);
		return 0;
	}

	/* find inbound interface so we know our address */
	{
		register struct chxcvr *xp;

		for (xp = chetherxcvr; xp->xc_etherinfo.bound_dev != dev; xp++)
			if (xp == &chetherxcvr[NCHETHER - 1])
				goto freem;
		mychaddr = xp->xc_addr;
	}

	DEBUGF("charpin() target 0%o, mychaddr 0%o\n",
	       arp_tcha(arp), mychaddr);

	/* lookup in our arp cache */
	{
		register struct ar_pair *app;
		register struct ar_pair *nap = charpairs;
		u_char *eaddr = 0;

		for (app = nap; app < &charpairs[NPAIRS]; app++)
			if (arp_scha(arp) == app->arp_chaos.ch_addr) {
				eaddr = app->arp_ether;
				break;
			} else if (app->arp_time < nap->arp_time) {
				nap = app;
				if (app->arp_chaos.ch_addr == 0)
					break;
			}
		/*
		 * If we are already cacheing the senders addresses,
		 * update our cache with possibly new information.
		 */
		if (eaddr) {
			nap = app;
			memcpy(app->arp_ether, arp_sha(arp), ELENGTH);
			DEBUGF("charpin() remembering eaddr for 0%o\n",
			       arp_scha(arp));
		}
		if (arp_tcha(arp) != mychaddr) {
			goto freem;
		}
		/*
		 * If we have never heard of this host before, find
		 * a slot and remember him.
		 * We set the time to 1 to be better than empty entries
		 * but worse than any that have actually been used.
		 * This is to prevent other hosts from flushing our cache
		 */
		if (eaddr == 0) {
			memcpy(nap->arp_ether, arp_sha(arp), ELENGTH);
			nap->arp_chaos.ch_addr = arp_scha(arp);
			nap->arp_time = 1;
		}
	}

	if (ntohs(arp->ar_op) == ARPOP_REQUEST) {
		char sha[MAX_ADDR_LEN], tha[MAX_ADDR_LEN];

		memcpy(sha, arp_tea(arp), dev->addr_len);
		memcpy(tha, arp_sha(arp), dev->addr_len);

		/* flip the protocol addresses */
		arp_tcha(arp) = arp_scha(arp);
		arp_scha(arp) = mychaddr;

		/* flip the h/w addresses, adding ours */
		memcpy(arp_tea(arp), arp_sha(arp), ELENGTH);
		memcpy((caddr_t)arp_sha(arp), dev->dev_addr, ELENGTH);

		arp->ar_op = htons(ARPOP_REPLY);

		dev->hard_header(skb, dev, ETHERTYPE_ARP, tha, sha, skb->len);

		DEBUGF("charpin() responding to arp request\n");
		dev_queue_xmit(skb, dev, 0);

		return 0;
	}
freem:
	DEBUGF("charpin() bailing\n");
	kfree_skb(skb, FREE_READ);
	return 0;
}			

int chein(struct sk_buff *skb, struct device *dev, struct packet_type *pt)
{
	struct pkt_header *php = (struct pkt_header *)skb->h.raw;
	struct chxcvr *xp;
	int chlength;

	for (xp = chetherxcvr; xp->xc_etherinfo.bound_dev != dev; xp++)
		if (xp >= &chetherxcvr[NCHETHER - 1]) {
			kfree_skb(skb, FREE_READ);
			return 0;
		}

	/* verify the lengths before we start */
	chlength = php->ph_len + sizeof(struct pkt_header);

	if (skb->len < sizeof(struct pkt_header) ||
	    chlength > CHMAXDATA + sizeof(struct pkt_header)) {
		kfree_skb(skb, FREE_READ);
		return 0;
	}

	/* copying is painful, but it works */
	{
		struct packet *pkt;

		pkt = (struct packet *)ch_alloc(chlength +
						sizeof(struct ncp_header), 1);
		if (pkt != NULL) {
			memcpy((char *)&pkt->pk_phead, skb->h.raw, chlength);
			xp->xc_rpkt = pkt;
			rcvpkt(xp);
		}
	}

	kfree_skb(skb, FREE_READ);
	return 0;
}

void
cheoutput(xcvr, pkt, head)
struct chxcvr *xcvr;
register struct packet *pkt;
int head;
{
	int chlength, arplength, resolving = 1;
	struct sk_buff *skb;
	struct device *dev;

        DEBUGF("cheoutput() pk_xdest 0x%x\n", pkt->pk_xdest);
	xcvr->xc_xmtd++;

	dev = xcvr->xc_etherinfo.bound_dev;

	/* sizeof(pkt_header) better be >= sizeof(arp) */
	chlength = pkt->pk_len + sizeof(struct pkt_header);
	arplength = sizeof(struct arphdr) + 2 * (dev->addr_len + 4);

	skb = alloc_skb(ETH_HLEN +
			(chlength < arplength ? arplength : chlength),
			GFP_ATOMIC);
	if (skb == 0) {
        	xmitdone(pkt);
		return;
	}
	DEBUGF("allocated %d byte packet\n",
	       ETH_HLEN + (chlength < arplength ? arplength : chlength));

	/* save room for the ethernet header */
	skb_reserve(skb, ETH_HLEN);

	if (pkt->pk_xdest == 0) {
		/* broadcast */
		dev->hard_header(skb, dev, ETHERTYPE_CHAOS,
				 dev->broadcast, 0, skb->len);
		resolving = 0;
	} else {
		register struct ar_pair *app;

		/* get h/w address from arp cache */
		for (app = charpairs;
		     app < &charpairs[NPAIRS] && app->arp_chaos.ch_addr; app++)
			if (pkt->pk_xdest == app->arp_chaos.ch_addr) {
				app->arp_time = ++charptime;
				DEBUGF("found in cache\n");
				dev->hard_header(skb, dev, ETHERTYPE_CHAOS,
						 app->arp_ether, 0, skb->len);
				resolving = 0;
				break;
			}
	}

	DEBUGF("resolving %d\n", resolving);
	if (resolving) {
		struct arphdr *arp;

		dev->hard_header(skb, dev, ETHERTYPE_ARP,
				 dev->broadcast, 0, skb->len);

		/* advance tail & len */
		DEBUGF("need %d bytes for arp\n",
		       sizeof(struct arphdr) + 2 * (dev->addr_len + 4));
		arp = (struct arphdr *)skb_put(skb, sizeof(struct arphdr) +
					       2 * (dev->addr_len + 4));

		arp->ar_hrd = htons(ARPHRD_ETHER);
		arp->ar_pro = htons(ETHERTYPE_CHAOS);
		arp->ar_hln = ELENGTH;
		arp->ar_pln = sizeof(chaddr);
		arp->ar_op = htons(ARPOP_REQUEST);
		memcpy((caddr_t)arp_sha(arp), dev->dev_addr, ELENGTH);
		arp_scha(arp) = xcvr->xc_addr;
		memcpy(arp_tea(arp), dev->broadcast, ELENGTH);
		arp_tcha(arp) = pkt->pk_xdest;
		DEBUGF("sending chaos arp for %o from %o\n",
		       pkt->pk_xdest, xcvr->xc_addr);
	} else {
		char *pp = skb_put(skb, chlength);
		if (pp)
			memcpy(pp, (char *)&pkt->pk_phead, chlength);
		DEBUGF("sending chaos datagram\n");
	}

	skb->arp = 1;

	xmitdone(pkt);
	dev_queue_xmit(skb, xcvr->xc_etherinfo.bound_dev, 0);
}

cheinit() {}
chereset() {}
chestart() {}

/*
 * Handle the CHIOCETHER ioctl to assign a chaos address to an
 * ethernet interface.  Note that all initialization is done here.
 */
int
cheaddr(addr)
char *addr;
{
	struct chether che;
	struct chxcvr *xp;
        struct device *dev;
	struct packet_type *p1, *p2;
        int errno;

        errno = verify_area(VERIFY_READ, (int *)addr, sizeof(struct chether));
        if (errno)
        	return errno;

        memcpy_fromfs(&che, (int *)addr, sizeof(struct chether));

	/* find named ethernet device */
	dev = dev_get(che.ce_name);
        if (dev == NULL)
        	return -ENODEV;

	/* put an entry in the chetherxcvr array for inbound demux */
	for (xp = chetherxcvr; xp->xc_etherinfo.bound_dev; xp++) {
	    if (xp->xc_etherinfo.bound_dev == 0 ||
		xp->xc_etherinfo.bound_dev == dev)
		break;
	    else if (xp == &chetherxcvr[NCHETHER - 1])
		return -EIO;
	}

	/* attach it */
	xp->xc_etherinfo.bound_dev = dev;
	xp->xc_addr = che.ce_addr;
	xp->xc_cost = CHCCOST;
	xp->xc_xmit = cheoutput;
	xp->xc_reset = chereset;
	xp->xc_start = chestart;

	printf("chether: %s enabled as Chaosnet address 0%o\n",
	       che.ce_name, che.ce_addr);

	chattach(xp);

	/* set up for reception of chaos & arp packets */
	if (xp->xc_etherinfo.prot_hook1 == 0) {
	    p1 = (struct packet_type *) kmalloc(sizeof(*p1), GFP_KERNEL);
	    if (p1 == NULL) {
		chedeinit();
		return -ENOMEM;
	    }

	    /* hook into packet reception */
	    p1->func = chein;
	    p1->type = htons(ETHERTYPE_CHAOS);
	    p1->data = (void *)0;
	    p1->dev = dev;
	    dev_add_pack(p1);

	    xp->xc_etherinfo.prot_hook1 = p1;
	}

	if (xp->xc_etherinfo.prot_hook2 == 0) {
	    p2 = (struct packet_type *) kmalloc(sizeof(*p2), GFP_KERNEL);
	    if (p2 == NULL) {
		chedeinit();
		return -ENOMEM;
	    }

	    p2->func = charpin;
	    p2->type = htons(ETHERTYPE_ARP);
	    p2->data = (void *)0;
	    p2->dev = dev;
	    dev_add_pack(p2);

	    xp->xc_etherinfo.prot_hook2 = p2;
	}

	xp->xc_etherinfo.bound_dev = dev;

	return 0;
}

void
chedeinit()
{
	struct chxcvr *xp;

	DEBUGF("chedeinit()\n");
	printf("chether: uninitializing\n");

	for (xp = chetherxcvr; xp->xc_etherinfo.bound_dev; xp++) {

	    if (xp->xc_etherinfo.prot_hook1) {
		dev_remove_pack((struct packet_type *)
				xp->xc_etherinfo.prot_hook1);
#if 0
		/* memory leak unloading till we turn this on */
		kfree_s(xp->xc_etherinfo.prot_hook1,
			sizeof(struct packet_type));
#endif
		xp->xc_etherinfo.prot_hook1 = NULL;
	    }

	    if (xp->xc_etherinfo.prot_hook2) {
		dev_remove_pack((struct packet_type *)
				xp->xc_etherinfo.prot_hook2);
#if 0
		/* memory leak unloading till we turn this on */
		kfree_s(xp->xc_etherinfo.prot_hook2,
			sizeof(struct packet_type));
#endif
		xp->xc_etherinfo.prot_hook2 = NULL;
	    }
	}
}

#endif /* linux */
