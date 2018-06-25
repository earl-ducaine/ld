# -*- mode: bash;  -*-


rm -f *.o *.so tun-tap

export libs="-lm"

# Note, the -Wl,-R flags will make our shared library available to the
# executable app from the location that it was compiled, rather than
# having to be installed globably or adding the build path to
# LD_LIBRARY_PATH.

export ldflags="-L. -Wl,-R -Wl,."
export cflags="-Wno-packed-bitfield-compat -DGC_LINUX_THREADS -D_REENTRANT -fPIC  -g -pipe -Wall"

gcc $cflags -c 3com.c
gcc -shared -Wl,-soname,lib3com.so $ldflags -lecl -o lib3com.so 3com.o $libs
gcc main.c $cflags $ldflags -lapp_main -lecl -o app
gcc $cflags -c tun-tap.c




gcc main.c -DGC_LINUX_THREADS -D_REENTRANT -fPIC  -g -pipe -Wall -L. -Wl,-R -Wl,. -lapp_main -lecl -o app
gcc main.c -DGC_LINUX_THREADS -D_REENTRANT -fPIC  -g -pipe -Wall -L. -Wl,-R -Wl,. -lapp_main -o s2latex

gcc main.c -DGC_LINUX_THREADS -D_REENTRANT -fPIC  -g -pipe -Wall "-L. -Wl,-R -Wl,." -lapp_main -o s2latex
gcc main.c "-DGC_LINUX_THREADS -D_REENTRANT -fPIC  -g -pipe -Wall" ""-L. -Wl,-R -Wl,."" -lapp_main -o s2latex
