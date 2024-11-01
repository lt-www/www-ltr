HS_OPT = -Wall -fwarn-tabs -O2
STATIC_OPT = -static -optc-static -optl-static -optl-pthread

build-ghc:
	ghc $(HS_OPT) --make lt-cgi.hs -o lt-cgi
	strip -s lt-cgi
	cp lt-cgi ../index.cgi

clean:
	rm -f *.hi *.o
	rm -f lt-cgi

ln-local:
	rm -f WWW Text
	mkdir -p Text
	ln -s $(HOME)/sw/www-minus/Www Www
	ln -s $(HOME)/sw/html-minus/Text/Html Text/Html
	ln -s $(HOME)/opt/src/xml-1.3.9/Text/XML Text/XML
