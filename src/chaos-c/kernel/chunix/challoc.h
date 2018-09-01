/*
 *	$Source: /projects/chaos/kernel/chunix/challoc.h,v $
 *	$Author: brad $
 *	$Locker:  $
 *	$Log: challoc.h,v $
 *	Revision 1.1.1.1  1998/09/07 18:56:08  brad
 *	initial checkin of initial release
 *	
 * Revision 1.1  84/06/12  20:08:52  jis
 * Initial revision
 * 
 */
/*
 * Definitions which are shared between the system dependent storage
 * allocator and other system dependent modules.
 */
#ifdef BSD42
/*
 * Macro to convert a chaos allocation into the corresponding mbuf pointer.
 */
#define chdtom(d) \
		((int)(d) & (MSIZE - 1) ? dtom(d) : (struct mbuf *)((int)(d)&~CLOFSET))
#endif

char *ch_alloc(int size, int cantwait);
void ch_free(char *p);
int ch_size(char *p);

void ch_bufalloc(void);
void ch_buffree(void);
