/*
 * declarations for host table
 */

#define HOSTTAB	"/etc/hosttab"
#define MAXHOST	20		/* Maximum size of host name */

struct net_entry {
	char *net_name;
	int net_number;
	int net_type;
};
#define NT_CHAOS	1	/* Net is a chaos net */

struct host_entry {
	char *host_name;
	struct net_address *host_address;
	int host_server;	/* if SERVER, else USER */
	char *host_system;
	char *host_machine;
	char **host_nicnames;
};

struct net_address {
	short addr_net;
	short addr_host;
};

struct ip_address {	/* Internet Protocol address */
	char ip_net;	/* network */
	char ip_hhost;	/* most significant byte of address (often subnet) */
	char ip_mhost;	/* middle byte of address */
	char ip_lhost;	/* least significant byte of address */
};

struct host_data	{
	struct net_entry	*ht_nets;	/* Pointer to net table */
	struct host_entry	*ht_hosts;	/* Pointer to host table */
	struct host_entry	*ht_me;		/* This host */
	int			ht_nsize;	/* Size of net table */
	int			ht_hsize;	/* Size of host table */
};
extern struct host_data *host_data;	/* Filled inside the library */

extern struct host_entry *host_info();
extern char *host_name();
extern char *host_system();
extern char *host_machine();
extern char *chaos_name();
extern int net_number();
extern unsigned short chaos_addr();
extern unsigned short chaos_host();
extern int arpa_addr();
extern int ip_addr();
extern struct host_entry *host_here();	/* This host's entry */
extern char *host_me();			/* Name of this host */
extern int host_start();
extern struct host_entry *host_next();
