HS_OPT = -Wall -fwarn-tabs -O2
STATIC_OPT = -static -optc-static -optl-static -optl-pthread

build-ghc:
	ghc $(HS_OPT) --make ltr-cgi.hs -o ltr-cgi
	strip -s ltr-cgi
	cp ltr-cgi ../index.cgi

build-ghc-static:
	ghc $(HS_OPT) $(STATIC_OPT) --make ltr-cgi.hs -o ltr-cgi-static
	strip -s ltr-cgi-static
	cp ltr-cgi-static ../index.static.cgi

clean:
	rm -f *.hi *.o
	rm -f ltr-cgi ltr-cgi-static

ln-local:
	rm -f WWW Text/HTML
	mkdir -p Text
	ln -s $(HOME)/sw/www-minus/WWW WWW
	ln -s $(HOME)/sw/html-minus/Text/HTML Text/HTML

cabal-dep:
	cabal install $(shell hsc3-setup pkg-dep -non-local *.hs)
