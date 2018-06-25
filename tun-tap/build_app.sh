
# -*- mode: bash;  -*-


rm -f *.o *.so app

export libs="-lm"

# Note, the -Wl,-R flags will make our shared library available to the
# executable app from the location that it was compiled, rather than
# having to be installed globably or adding the build path to
# LD_LIBRARY_PATH.

export ldflags="-L. -Wl,-R -Wl,."
export cflags="-DGC_LINUX_THREADS -D_REENTRANT -fPIC  -g -pipe -Wall"

gcc $cflags -c app_main.c
gcc -shared -Wl,-soname,libapp_main.so $ldflags -lecl -o libapp_main.so *o $libs
gcc main.c $cflags $ldflags -lapp_main -lecl -o app




gcc main.c -DGC_LINUX_THREADS -D_REENTRANT -fPIC  -g -pipe -Wall -L. -Wl,-R -Wl,. -lapp_main -lecl -o app
gcc main.c -DGC_LINUX_THREADS -D_REENTRANT -fPIC  -g -pipe -Wall -L. -Wl,-R -Wl,. -lapp_main -o s2latex

gcc main.c -DGC_LINUX_THREADS -D_REENTRANT -fPIC  -g -pipe -Wall "-L. -Wl,-R -Wl,." -lapp_main -o s2latex
gcc main.c "-DGC_LINUX_THREADS -D_REENTRANT -fPIC  -g -pipe -Wall" ""-L. -Wl,-R -Wl,."" -lapp_main -o s2latex
