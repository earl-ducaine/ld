AM_CFLAGS = -std=gnu99 -Wall -Wextra -Wno-packed-bitfield-compat -ggdb -ggdb -Og
AM_LDFLAGS = -fPIC -Og

bin_PROGRAMS = lam lpart

lam_SOURCES = 3com.c \
	3com.h \
	kernel.c \
	lambda_cpu.c \
	lambda_cpu.h \
	ld.h \
	keyboard-sdl.h \
	main.c \
	mem.c \
	mem.h \
	nubus.c \
	nubus.h \
	sdu.c \
	sdu.h \
	sdu_hw.c \
	sdu_hw.h \
	smd.c \
	smd.h \
	syms.c \
	syms.h \
	tapemaster.c \
	tapemaster.h \
	trace.c \
	trace.h \
	vcmem.c \
	vcmem.h

if SDL1
lam_SOURCES += keyboard-sdl-1.c
endif

if SDL2
lam_SOURCES += keyboard-sdl-2.c
endif

lpart_SOURCES = lpart.c
