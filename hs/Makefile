build-static-ghc:
	ghc -Wall -fwarn-tabs -O0 --make ltr-static.hs
	strip -s ltr-static
	cp ltr-static ../administration.cgi

build-cgi-ghc:
	ghc -Wall -fwarn-tabs --make ltr-cgi.hs -o ltr-cgi
	strip -s ltr-cgi
	cp ltr-cgi ../index.cgi

rd-rebuild:
	ghc ltr-static.hs -e rd_rebuild

clean:
	rm -f *.hi *.o
	rm -f ltr-static ltr-cgi
