build-ghc:
	ghc -Wall -fwarn-tabs --make ltr-cgi.hs -o ltr-cgi
	strip -s ltr-cgi
	cp ltr-cgi ../index.cgi

build-ghc-static:
	ghc -O2 --make -static -optc-static -optl-static -optl-pthread ltr-cgi.hs
	strip -s ltr-cgi
	cp ltr-cgi ../index.cgi

clean:
	rm -f *.hi *.o
	rm -f ltr-cgi
