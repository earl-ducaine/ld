#include <sys/chaos.h>

chopen(address, contact, mode, async, data, dlength, rwsize)
int address;
char *contact;
char *data;
{
	struct chopen rfc;
	int f, connfd = -1;

	rfc.co_host = address;
	rfc.co_contact = contact;
	rfc.co_data = data;
	rfc.co_length = data ? (dlength ? dlength : strlen(data)) : 0;
	rfc.co_clength = strlen(contact);
	rfc.co_async = async;
	rfc.co_rwsize = rwsize;
	f = open(CHAOSDEV, mode);
#if defined(BSD42) || defined(linux)
	if (f >= 0) {
		connfd = ioctl(f, CHIOCOPEN, &rfc);
	}
	close(f);
#else
	if (f >= 0 && ioctl(f, CHIOCOPEN, &rfc))
		close(f);
	else
		connfd = f;
#endif
	return connfd;
}

chlisten(contact, mode, async, rwsize)
char *contact;
{
	return chopen(0, contact, mode, async, 0, 0, rwsize);
}

chreject(fd, string)
int fd;
char *string;
{
	struct chreject chr;

	if(string==0||strlen(string)==0)
	  string = "No Reason Given";
	chr.cr_reason = string;
	chr.cr_length = strlen(string);
	return ioctl(fd, CHIOCREJECT, &chr);
}
