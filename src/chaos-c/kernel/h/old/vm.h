/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)vm.h	7.1 (Berkeley) 6/4/86
 */
/*
 * RCS Info	
 *	$Header: /projects/chaos/kernel/h/old/vm.h,v 1.1.1.1 1998/09/07 18:56:10 brad Exp $
 *	$Locker:  $
 */

/*
 *	#include "../h/vm.h"
 * or	#include <vm.h>		 in a user program
 * is a quick way to include all the vm header files.
 */
#ifdef KERNEL
#include "vmparam.h"
#include "vmmac.h"
#include "vmmeter.h"
#include "vmsystm.h"
#else
#include <sys/vmparam.h>
#include <sys/vmmac.h>
#include <sys/vmmeter.h>
#include <sys/vmsystm.h>
#endif
