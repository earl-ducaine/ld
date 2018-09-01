/*
 * read an SRI-style official host table
 * and produce a hosttab.c file
 */

#include <stdio.h>
#include <ctype.h>
#include "hosttab.h"

#define MAXNAMELENGTH 50
#define NNETS 50
#define NHOSTS 1000
#define NSYSTEMS 100
#define NMACHINES 100

struct net_entry nets[NNETS];
struct host_entry hosts[NHOSTS];
char *systems[NSYSTEMS];
char *machines[NMACHINES];

char *malloc();
char *parsehosts();
char *parseaddr();
char *canon();

main(argc, argv)
int argc;
char **argv;
{
	char line[512];
	int myhost = -1;
	struct net_entry *nextnet = nets;
	struct host_entry *nexthost = hosts;

	if (argc != 2) {
		fprintf(stderr, "Usage is: hosts2 this-host-in-upper-case\n");
		return (1);
	}

	if (freopen("hosts", "r", stdin) == NULL) {
		fprintf(stderr, "can't open hosts for input\n");
		return(1);
	}
	if (freopen("hosttab.c", "w", stdout) == NULL) {
		fprintf(stderr, "can't open hosttab.c for output\n");
		return(2);
	}
	while (gets(line) != NULL) {
		char name[MAXNAMELENGTH];
		int number;
		if (line[0] == ';' || line[0] == 014 || line[0] == 0)
			continue;
		if (sscanf(line, "NET %[^,], %d", name, &number) == 2) {
			nextnet->net_name = malloc(strlen(name)+1);
			strcpy(nextnet->net_name, name);
			nextnet->net_number = number;
			nextnet += 1;
		} else if (sscanf(line, "HOST %[^,],", name) == 1) {
			register char *p, *q;
			char status[8], system[20], machine[20];
			nexthost->host_name = malloc(strlen(name) + 1);
			strcpy(nexthost->host_name, name);
			if (myhost == -1 && strcmp(argv[1], name) == 0)
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
			if (strcmp("SERVER", status) == 0)
				nexthost->host_server = 1;
			else if (strcmp("USER", status) == 0)
				nexthost->host_server = 0;
			else {
				fprintf(stderr, "bad status %s for %s\n",
					status, name);
				return(5);
			}
			nexthost->host_system = canon(systems, system);
			nexthost->host_machine = canon(machines, machine);
			if (*p == ',')
				nicnames(nexthost, ++p);
			nexthost += 1;
		} else {
			fprintf(stderr, "syntax error in hosts\n");
			fprintf(stderr, "%s\n", line);

			return(6);
		}
	}
	if (myhost == -1) {
		fprintf(stderr, "No host in table matches: %s\n", argv[1]);
		return(7);
	}

	printf("# include \"hosttab.h\"\n");
	printf("struct host_entry host_table[];\n");
	printf("struct net_entry net_table[];\n");
	printf("struct host_data everything = {\n");
	printf("\tnet_table,\n\thost_table,\n\t&host_table[%d],\n", myhost);
	printf("\t%d,\n\t%d\n};\n", nextnet - nets, nexthost - hosts);
	dumpnets(nextnet - nets);
	dumpsystems();
	dumpmachines();
	dumphosts(nexthost - hosts);
}

char *parsehosts(h, p)
register struct host_entry *h;
register char *p;
{
	struct net_address addrs[10];
	register struct net_address *a = addrs, *b;
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
	return(p);
}

char *parseaddr(a, p)
register struct net_address *a;
register char *p;
{
	char net[20];
	register struct net_entry *n;
	while (*p == ' ' || *p == '\t')
		p++;
	if (isalpha(*p)) {
		sscanf(p, "%s", net);
		while (*p++ != ' ');
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
	for (n = nets; n->net_name; n++)
		if (strcmp(net, n->net_name) == 0)
			break;
	if (!n->net_name) {
		fprintf(stderr, "bad net name %s\n", net);
		exit(7);
	}
	a->addr_net = n->net_number;
	if (strcmp(net + strlen(net) - strlen("CHAOS"), "CHAOS") == 0) {
		int number;

		if (sscanf(p, "%o", &number) != 1) {
			fprintf(stderr, "bad chaos net address %s\n", p);
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

/*
 * dump net table
 */
dumpnets(no)
register int no;
{
	register struct net_entry *n;
	register int i;
	register char *cp;

	printf("struct net_entry net_table[] = {\n");
	for (i = 0, n = nets; i < no; i++, n++) {
		lower(n->net_name);
		for (cp = n->net_name; *cp; cp++)
			if (*cp == 'c' && strncmp("chaos", cp, 5) == 0) {
				n->net_type = NT_CHAOS;
				break;
			}
		printf("\t{\"%s\", %d, %d},\n", n->net_name, n->net_number,
			n->net_type);
	}
	printf("};\n");
}

/*
 * dump operating system name table
 */
dumpsystems()
{
	register char **p;
	register int i;

	for (p = systems, i = 0; *p; p++)
		printf("static char h_s%d[] = \"%s\";\n", i++, *p);
}

/*
 * dump machine name table
 */
dumpmachines()
{
	register char **p;
	register int i;
	for (p = machines, i = 0; *p; p++)
		printf("static char h_m%d[] = \"%s\";\n", i++, *p);
}

/*
 * dump main host table
 */
dumphosts(n)
{
	register struct host_entry *h;
	register int i;
	for (i = 0, h = hosts; i < n; h++, i++) {
		dumpaddr(h, i);
		dumpnicnames(h, i);
	}
	printf("struct host_entry host_table[] = {\n");
	for (i = 0, h = hosts; i < n; h++, i++) {
		printf("\t{\"%s\",\taddr%d,\t%d,", lower(h->host_name), i,
			h->host_server);
		printf("\th_s%d,\th_m%d,",
			id(h->host_system, systems),
			id(h->host_machine, machines));
		if (h->host_nicnames)
			printf("\tnic%d},\n", i);
		else
			printf("\t0},\n");
	}
	printf("};\n");
}

/*
 * compute the id of a string in a string table
 */
id(s, t)
register char *s, **t;
{
	register int i;
	for (i = 0; *t++ != s; i++);
	return(i);
}

/*
 * dump an address table
 */
dumpaddr(h, i)
register struct host_entry *h;
int i;
{
	register struct net_address *a;
	printf("static struct net_address addr%d[] = {\n", i);
	for (a = h->host_address; a->addr_net; a++)
		printf("\t0%o, 0%o,\n", a->addr_net, a->addr_host);
	printf("\t0, 0\n};\n");
}

/*
 * dump nicnames
 */
dumpnicnames(h, i)
register struct host_entry *h;
int i;
{
	register char **p;
	p = h->host_nicnames;
	if (p) {
		printf("static char *nic%d[] = {\n", i);
		for ( ; *p; p++)
			printf("\t\"%s\",\n", *p);
		printf("\t0\n};\n");
	}
}

