

find_package(SDL 1.2 EXACT REQUIRED)

include_directories(${CMAKE_SOURCE_DIR}/src)


# Need to test for the following:
# HAVE_LINUX_IF_H) && defined (HAVE_LINUX_IF_TUN_H)


if(CONFIG_2X2)
    add_definitions(-DCONFIG_2X2)
endif(CONFIG_2X2)

add_definitions(-DSDL1)

#However, the file(GLOB...) allows for wildcard additions:
# file(GLOB LIB_SOURCES "lib/*.c")
file(GLOB LIB_SOURCES
  "3com.c"
  "3com.h"
  "3com-tun.c"
  "kernel.c"
  "lambda_cpu.c"
  "lambda_cpu.h"
  "ld.h"
  "keyboard-sdl.h"
  "keyboard-sdl.c"
  "mem.c"
  "mem.h"
  "nubus.c"
  "nubus.h"
  "sdu.c"
  "sdu.h"
  "sdu_hw.c"
  "sdu_hw.h"
  "smd.c"
  "smd.h"
  "syms.c"
  "syms.h"
  "tapemaster.c"
  "tapemaster.h"
  "trace.c"
  "trace.h"
  "vcmem.c"
  "vcmem.h"
  "keyboard-sdl-1.c")


file(GLOB SOURCES "main.c")

#Generate the shared library from the sources
add_library(lam_lib SHARED ${LIB_SOURCES})

#Set the location for library installation -- i.e., /usr/lib in this case
# not really necessary in this example. Use "sudo make install" to apply
install(TARGETS lam_lib DESTINATION /usr/lib)

add_executable(lam_main ${SOURCES})

target_link_libraries(lam_main lam_lib m  ${SDL_LIBRARY})
