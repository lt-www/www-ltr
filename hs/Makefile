build-ghc:
	ghc -Wall -fwarn-tabs -O0 --make lt-static.hs
	strip -s lt-static
	mv lt-static ../administration.cgi

build-cgi:
	ghc -Wall -fwarn-tabs --make lt-cgi.hs -o ../lt.cgi

rd-rebuild:
	ghc lt-static.hs -e rd_rebuild

clean:
	rm -f *.hi *.o
	rm -f lt-static lt-cgi
