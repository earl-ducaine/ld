chaostables:
	cd /tmp;make -f /usr/lib/chaos/update.make go
	cd /tmp;make -f /usr/lib/chaos/update.make clean
	
go:	/etc/hosttab /etc/chaoshosts

/etc/hosttab:		/usr/lib/chaos/hosts.global /usr/lib/chaos/hosts.site\
		        /usr/lib/chaos/hosts.local 
	make -f /usr/lib/chaos/update.make hosttab
	install -c -m 644 hosttab /etc/hosttab

/etc/chaoshosts: /etc/hosttab
	make -f /usr/lib/chaos/update.make chaoshosts
	install -c -m 644 chaoshosts /etc/chaoshosts

hosttab: hosttab.c
	$(CC) -c hosttab.c
	mv hosttab.o hosttab
	strip hosttab

hosttab.c: hosts /usr/lib/chaos/hosts2
	/usr/lib/chaos/hosts2 `/bin/hostname | tr a-z A-Z`.LCS.MIT.EDU

hosts:	/usr/lib/chaos/hosts.global /usr/lib/chaos/hosts.site \
		/usr/lib/chaos/hosts.local
	-cat /usr/lib/chaos/hosts.global /usr/lib/chaos/hosts.site \
		/usr/lib/chaos/hosts.local >hosts

chaoshosts: /etc/hosttab /usr/lib/chaos/chaosnames
	/usr/lib/chaos/chaosnames > chaoshosts

clean:
	cd /tmp;rm -f hosttab.c hosttab chaoshosts

