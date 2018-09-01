/* NOTES
 * 2/1/85 dove
 *	accept chaos address numbers in host_info()
 */
#include <ctype.h>
#include <stdio.h>
#include "hosttab.h"

#define H host_data
#define CHECK	if (!host_data) readhosts()
#define CHECKCH	if (ch_net == 0) getchaos()
static int ch_net;	/* The net number of the local chaosnet */
static int arpa_net;
static int hostlib_debug;
/*
 * Copy s2 to s1, converting to lower case
 * and truncating or null-padding to always copy n bytes
 * return s1
 */

static
lowercase(s1, s2, n)
register char *s1, *s2;
{
	register i;
	for (i = 0; i < n; i++)
		if ((*s1++ = isupper(*s2)?tolower(*s2++):*s2++) == '\0')
			return;
}

struct host_entry *
host_info(name)
char *name;
{
	register struct host_entry *h;
	struct host_entry *chaos_entry();
	register char **p;
	register int i;
	char lcname[MAXHOST];

	if(isdigit(*name))
	{
		if (hostlib_debug) printf("host_info() numeric\n");
		return(chaos_entry(chaos_addr(name, 0)));
	}

	CHECK;
	lowercase(lcname, name, MAXHOST);
	for (i = H->ht_hsize, h = H->ht_hosts; --i >= 0; h++) {
		if (strncmp(lcname, h->host_name, MAXHOST) == 0)
			return(h);
		if (p = h->host_nicnames)
			while (*p)
				if (strncmp(lcname, *p++, MAXHOST) == 0)
					return(h);
	}
	return(0);
}

char *
host_name(name)
char *name;
{
	register struct host_entry *h = host_info(name);

	return(h ? h->host_name : 0);
}

char *
host_system(name)
char *name;
{
	register struct host_entry *h = host_info(name);

	return(h ? h->host_system : 0);
}

char *
host_machine(name)
char *name;
{
	register struct host_entry *h = host_info(name);

	return(h ? h->host_machine : 0);
}

net_number(name)
char *name;
{
	register struct net_entry *n;
	register int i;
	char lcname[MAXHOST];

	CHECK;
	lowercase(lcname, name, MAXHOST);
	for (i = H->ht_nsize, n = H->ht_nets; --i >= 0; n++)
		if (strncmp(lcname, n->net_name, MAXHOST) == 0)
			return(n->net_number);
	return(0);
}
getchaos()
{
	register struct net_address *a;
	register struct net_entry *n;
	register int i;

	if (H == 0)
		return;
	for (a = H->ht_me->host_address; a->addr_net; a++) {
		for (n = H->ht_nets, i = 0; i < H->ht_nsize; i++, n++) {
			if (n->net_number == a->addr_net)
				break;
		}
		if (n->net_type == NT_CHAOS) {
			ch_net = a->addr_net;
			break;
		}
	}
}

/*
 * Return the local chaos network address for the given host.
 * First we must know which network we are on, then find the given
 * host's address on that same network.
 */
unsigned short
chaos_addr(name, subnet)
char *name;
int subnet;	/* preferred subnet */
{
	register struct host_entry *h;
	int found = 0;

if (hostlib_debug) printf("chaos_addr('%s')\n", name);
	CHECK;
	CHECKCH;
if (hostlib_debug) printf("checking '%s' for numberic\n", name);
	if (isdigit(*name))
		if (sscanf(name, "%o", &found) != 1) {
if (hostlib_debug) printf("name '%s' failed sscanf\n", name);
			return 0;
		} else
			return found;
	if ((h = host_info(name)) == 0) {
if (hostlib_debug) printf("name '%s' failed host_info\n", name);
		return(0);
	}
	return chaos_host(h, 0);
}

unsigned short
chaos_host(h, subnet)
register struct host_entry *h;
{
	register struct net_address *a;
	int found;

if (hostlib_debug) printf("chaos_host()\n");
	CHECK;
	CHECKCH;
	for (a = h->host_address, found = 0; a->addr_net; a++)
		if (a->addr_net == ch_net)
			if ((a->addr_host>>8) == subnet)
				return(a->addr_host);
			else if (found == 0)
				found = a->addr_host;
	return(found);
}


chaosnames(fp)
FILE *fp;
{
	register struct host_entry *h;
	int i;
	register struct net_address *a;
	register char **p;

	CHECK;
	CHECKCH;
	for (i = H->ht_hsize, h = H->ht_hosts; --i >= 0; h++)
/*
THis doesn't work since this field isn't really maintained.
		if (h->host_server)
*/
		for (a = h->host_address; a->addr_net; a++)
			if (a->addr_net == ch_net) {
				fprintf(fp, "%s\n", h->host_name);
				if (p = h->host_nicnames)
					while (*p)
						fprintf(fp, "%s\n", *p++);
			}
}

arpa_addr(name)
char *name;
{
	register struct host_entry *h;

	if ((h = host_info(name)) == 0)
		return(0);
	return arpa_host(h);
}
arpa_host(h)
register struct host_entry *h;
{
	register struct net_address *a;

	if (arpa_net == 0)
		if ((arpa_net = net_number("ARPANET")) == 0)
			return(0);
	for (a = h->host_address; a->addr_net; a++)
		if (a->addr_net == arpa_net)
			return(a->addr_host);
	return(0);
}

ip_addr(name, net, subnet, ip)
char *name;
int net, subnet;	/* preferred net and subnet */
register struct ip_address *ip;
{
	register struct host_entry *h = host_info(name);
	register struct net_address *a;
	struct ip_address i, b;

	i.ip_net = 0;
	i.ip_hhost = 0;
	i.ip_mhost = 0;
	i.ip_lhost = 0;
	b = i;
	if (h == 0)
		return(1);
	for (a = h->host_address; a; a++) {
		i.ip_net = a->addr_net;
		i.ip_hhost = a->addr_host >> 8;
		i.ip_lhost = a->addr_host & 0377;
		if (i.ip_net == net) {
			if (i.ip_hhost == subnet) {
				*ip = i;
				return(0);
			}
			if (net && b.ip_net != net)
				b = i;
		} else if (b.ip_net == 0)
			b = i;
	}
	if (b.ip_net == 0)
		return(1);
	*ip = b;
	return(0);
}

/*
 * return true if a host supports TFTP (in particular, mail mode)
 */

static char *tftphosts[] = {
	"mit-multics",
	"mit-ln",
	"mit-rts",
	0
};

istftphost(name)
char *name;
{
	char lcname[MAXHOST];
	register char **p;
	lowercase(lcname, name, MAXHOST);
	for (p = tftphosts; *p; p++)
		if (strncmp(lcname, *p, MAXHOST) == 0)
			return(1);
	return(0);
}
/*
 * Return the name of the host with the given local chaos net address.
 */

char *
chaos_name(addr)
short addr;
{
	register struct host_entry *h, *hend;
	register struct net_address *a;
	static char name[MAXHOST];
/*	char *sprintf(); */

	CHECK;
	CHECKCH;
	hend = &H->ht_hosts[H->ht_hsize];
	for (h = H->ht_hosts; h < hend; h++)
		for (a = h->host_address; a->addr_net; a++)
			if (a->addr_net == ch_net &&
			    a->addr_host == addr)
				return h->host_name;
	sprintf(name, "host%0o", addr);
	return name;
}
/*
 * Return the host_entry of the host with the given local chaos net address.
 */

struct host_entry *
chaos_entry(addr)
short addr;
{
	register struct host_entry *h, *hend;
	register struct net_address *a;
	static char name[MAXHOST];
/*	char *sprintf();*/

if (hostlib_debug) printf("chaos_entry() %o\n", addr);
	CHECK;
	CHECKCH;
if (hostlib_debug) printf("hsize %d\n", H->ht_hsize);
	hend = &H->ht_hosts[H->ht_hsize];
	for (h = H->ht_hosts; h < hend; h++) {
if (hostlib_debug) printf("host_name %s\n", h->host_name);
		for (a = h->host_address; a->addr_net || a->addr_host; a++) {
if (hostlib_debug) printf("(%d %d) == (%d %d)\n", a->addr_net, a->addr_host, ch_net, addr);

			if (a->addr_net == ch_net &&
			    a->addr_host == addr)
				return(h);
		}
	}
	return(NULL);
}
static int hostn;

host_start()
{
	CHECK;
	hostn = 0;
}
struct host_entry *
host_next()
{
	return (hostn >= H->ht_hsize ? 0 : &H->ht_hosts[hostn++]);
}
struct host_entry *
host_here()
{
	CHECK;
	return H->ht_me;
}
char *
host_me()
{
	return host_here()->host_name;
}
