#include <stdio.h>
#include "hosttab.h"

#ifdef linux
#include <sys/types.h>
#endif

struct host_data *host_data;

#ifdef linux

#define MAXNAMELENGTH 50
#define NNETS 50
#define NHOSTS 1000
#define NSYSTEMS 100
#define NMACHINES 100

static struct net_entry nets[NNETS];
static struct host_entry hosts[NHOSTS];
static char *systems[NSYSTEMS];
static char *machines[NMACHINES];

static struct host_data data;
int nsize, hsize;
char *myhostname;
static int read_debug;

readhosts()
{
	int myhost = -1;
	struct net_entry *nextnet = nets;
	struct host_entry *nexthost = hosts;

	FILE *f;
	char line[1024];

if (read_debug) printf("readhosts()\n");
	if ((f = fopen(HOSTTAB, "r")) == 0) {
		perror(HOSTTAB);
		exit(1);
	}
	while (fgets(line, sizeof(line), f)) {
		char name[MAXNAMELENGTH];
		int number;
		if (line[0] == '#' || line[0] == ';' || line[0] == '\n')
			continue;
		line[strlen(line)-1] = 0;
if (read_debug) printf("line '%s'\n", line);
		if (sscanf(line, "NET %[^,], %d", name, &number) == 2) {
			char *cp;
if (read_debug) printf("net\n");
			lower(name);
			nextnet->net_name = malloc(strlen(name)+1);
			strcpy(nextnet->net_name, name);
			nextnet->net_number = number;
			for (cp = name; *cp; cp++) {
				if (*cp == 'c' &&
				    strncmp("chaos", cp, 5) == 0) {
					nextnet->net_type = NT_CHAOS;
					break;
				}
			}
			nextnet += 1;
			nsize++;
		} else if (sscanf(line, "HOST %[^,],", name) == 1) {
			register char *p, *q;
			char status[8], system[20], machine[20];
if (read_debug) printf("host\n");
			nexthost->host_name = malloc(strlen(name) + 1);
			lower(name);
			strcpy(nexthost->host_name, name);
			if (myhost == -1 && myhostname &&
			    strcmp(myhostname, name) == 0)
				myhost = nexthost - hosts;
			for (p = line; *p++ != ','; );
			while (*p == ' ' || *p == '\t')
				p++;
			p = parsehosts(nexthost, p);
			while (*p == ' ' || *p == '\t')
				p++;
			for (q = status; *p && *p != ','; p++)
				if (q < &status[7])
					*q++ = *p;
			*q = 0;
			if (*p == ',')
				p += 1;
			while (*p == ' ' || *p == '\t')
				p++;
			for (q = system; *p && *p != ','; p++)
				if (q < &system[19])
					*q++ = *p;
			*q = 0;
			if (*p == ',')
				p += 1;
			while (*p == ' ' || *p == '\t')
				p++;
			for (q = machine; *p && *p != ','; p++)
				if (q < &machine[19])
					*q++ = *p;
			*q = 0;
if (read_debug) printf("status '%s'\n", status);
			if (strcmp("SERVER", status) == 0)
				nexthost->host_server = 1;
			else if (strcmp("USER", status) == 0)
				nexthost->host_server = 0;
			else {
if (read_debug) printf("bad status\n");
				fprintf(stderr, "bad status %s for %s\n",
					status, name);
				exit(5);
			}
			nexthost->host_system = canon(systems, system);
			nexthost->host_machine = canon(machines, machine);
			if (*p == ',')
				nicnames(nexthost, ++p);
			nexthost += 1;
			hsize++;
		} else {
if (read_debug) printf("syntax\n");
			fprintf(stderr, "syntax error in hosts\n");
			fprintf(stderr, "%s\n", line);

			exit(6);
		}
	} 
	fclose(f);

	data.ht_nets = nets;
	data.ht_hosts = hosts;
	data.ht_me = hosts;
	data.ht_nsize = nsize;
	data.ht_hsize = hsize;

	host_data = &data;

if (read_debug) printf("readhosts() done\n");
}

#else

readhosts()
{
	register int i;
	register struct host_data *h;
	register struct host_entry *hp;
	register struct net_entry *np;
	register char **p;
	struct exec e;

	if ((i = open(HOSTTAB, 0)) < 0 ||
	    read(i, (char *)&e, sizeof(e)) != sizeof(e) ||
	    (host_data = h = (struct host_data *)malloc(e.a_data)) == NULL ||
	    lseek(i, (off_t)e.a_text, SEEK_CUR) <= 0 ||
	    read(i, (char *)host_data, e.a_data) != e.a_data) {
		fprintf(stderr, "readhosts: can't read host table: %s\n",
			HOSTTAB);
		exit(1);
	}
	close(i);
#define reloc(x, caste)	(x = caste((char *)h + (int)(x)))
	reloc(h->ht_nets, (struct net_entry *));
	reloc(h->ht_hosts, (struct host_entry *));
	reloc(h->ht_me, (struct host_entry *));
	for (np = h->ht_nets, i = 0; i < h->ht_nsize; np++, i++)
		reloc(np->net_name, (char *));
	for (hp = h->ht_hosts, i = 0; i < h->ht_hsize; hp++, i++) {
		reloc(hp->host_name, (char *));
		reloc(hp->host_system, (char *));
		reloc(hp->host_machine, (char *));
		reloc(hp->host_address, (struct net_address *));
		if (hp->host_nicnames) {
			reloc(hp->host_nicnames, (char **));
			for (p = hp->host_nicnames; *p; p++)
				reloc(*p, (char *));
		}
	}
}

#endif

#ifdef linux

char *parsehosts(h, p)
register struct host_entry *h;
register char *p;
{
	struct net_address addrs[10];
	register struct net_address *a = addrs, *b;
if (read_debug) printf("parsehosts('%s')\n", p);
	if (*p == '[') {
		p += 1;
		while (*p != ']') {
			p = parseaddr(a++, p);
			if (*p == ',')
				p += 1;
		}
		p += 1;
	} else
		p = parseaddr(a++, p);
	h->host_address = (struct net_address *)
			malloc((a - addrs + 1)*sizeof(*h->host_address));
	b = &h->host_address[a - addrs];
	b->addr_net = 0;	/* indicate end of list */
	while (--a >= addrs)
		*--b = *a;
	if (*p == ',')
		p += 1;
if (read_debug) printf("parsehosts() done\n");
	return(p);
}

char *parseaddr(a, p)
register struct net_address *a;
register char *p;
{
	char net[20];
	register struct net_entry *n;
if (read_debug) printf("parseaddr('%s')\n", p);
	while (*p == ' ' || *p == '\t')
		p++;
	if (isalpha(*p)) {
		sscanf(p, "%s", net);
		while (*p != ' ' && *p != '\t')
			p++;
#ifdef OLDHOSTS2
		if (strcmp("DIAL", net) == 0)
			strcpy(net, "DIALNET");
		else if (strcmp("ARPA", net) == 0)
			strcpy(net, "ARPANET");
		else if (strcmp("RCC", net) == 0)
			strcpy(net, "RCC-NET");
		else if (strcmp("SU", net) == 0)
			strcpy(net, "SU-NET-TEMP");
		else if (strcmp("LCS", net) == 0)
			strcpy(net, "MIT");
		else if (strcmp("RU", net) == 0)
			strcpy(net, "RU-NET");
#endif
	} else
		strcpy(net, "ARPANET");
	for (n = nets; n->net_name; n++) {
		if (strcasecmp(net, n->net_name) == 0)
			break;
	}
	if (!n->net_name) {
if (read_debug) printf("bad net name\n");
		fprintf(stderr, "bad net name %s\n", net);
		exit(7);
	}
	a->addr_net = n->net_number;
	if (strcmp(net + strlen(net) - strlen("CHAOS"), "CHAOS") == 0) {
		int number;

		if (sscanf(p, "%o", &number) != 1) {
if (read_debug) printf("bad chaos addr '%s'\n", p);
			fprintf(stderr, "bad chaos net address '%s'\n", p);
			exit(8);
		}
		a->addr_host = number;
	} else if (strcmp(net, "ARPANET") == 0
#ifdef OLDHOSTS2
	 || strcmp(net, "BBN-RCC") == 0
#else
	 || strcmp(net, "MILNET") == 0
	 || strcmp(net, "RCC") == 0
#endif
	 ) {
		int host, imp;
		if (sscanf(p, "%d/%d", &host, &imp) != 2) {
			fprintf(stderr, "bad arpanet address %s\n", p);
			exit(9);
		}
		a->addr_host = host*64+imp;
	} else if (
#ifdef OLDHOSTS2
	 strcmp(net, "LCSNET") == 0
#else
	 strcmp(net, "LCS") == 0
#endif
	 ) {
		int subnet, host;
		if (sscanf(p, "%o/%o", &subnet, &host) != 2) {
			fprintf(stderr, "bad lcsnet address %s\n", p);
			exit(11);
		}
		a->addr_host = (subnet<<8)+host;
	} else
		a->addr_host = 0;
	while (*p && *p != ']' && *p != ',')
		p += 1;
	if (read_debug) printf("parseaddr() done\n");
	return(p);
}

char *canon(t, s)
register char **t;
register char *s;
{
	register char *p;
	for (p = s; *p; p++)
		if (isupper(*p))
			*p = tolower(*p);
	for ( ;*t; t++)
		if (strcmp(*t, s) == 0)
			return(*t);
	*t = malloc(strlen(s) + 1);
	strcpy(*t, s);
	return(*t);
}

/*
 * parse nicnames
 */
nicnames(h, p)
struct host_entry *h;
register char *p;
{
	char *names[20], name[MAXNAMELENGTH];
	register char *q;
	register char **a, **b;
	for (a = names; a < &names[20]; a++)
		*a = 0;
	if (*p == '[') {
		p += 1;
		while (*p != ']') {
			for (q = name; *p && *p != ',' && *p != ']'; p++)
				if (q < &name[MAXNAMELENGTH-1])
					*q++ = *p;
			*q = 0;
			canon(names, name);
			if (*p == ',')
				p += 1;
		}
	} else
		canon(names, p);
	for (a = names; *a; a++);
	h->host_nicnames = (char **)malloc((a - names + 1)*sizeof *h->host_nicnames);
	b = &h->host_nicnames[a-names];
	while (a >= names)
		*b-- = *a--;
}

/*
 * convert a string to lower case
 */
char *lower(s)
char *s;
{
	register char *p;
	for (p = s; *p; p++)
		if (isupper(*p))
			*p = tolower(*p);
	return(s);
}

#ifdef UNIT_TEST
main()
{
  readhosts();
}
#endif

#endif
