build-static-ghc:
	ghc -Wall -fwarn-tabs -O0 --make ltr-static.hs
	strip -s ltr-static
	mv ltr-static ../administration.cgi

build-cgi-ghc:
	ghc -Wall -fwarn-tabs --make ltr-cgi.hs -o ltr-cgi
	strip -s ltr-cgi
	mv ltr-cgi ../ltr.cgi

rd-rebuild:
	ghc ltr-static.hs -e rd_rebuild

clean:
	rm -f *.hi *.o
	rm -f ltr-static
