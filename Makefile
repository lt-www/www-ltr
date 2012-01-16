build-ghc:
	(cd hs ; make build-ghc)

build-cabal:
	cabal configure --disable-optimisation
	cabal build
	cp dist/build/lt-static/lt-static administration.cgi
	strip -s administration.cgi

rd-rebuild:
	(cd hs ; make rd-rebuild)

clean:
	cabal clean
	(cd hs ; make clean)
	rm -fR home news shows albums shop press photos contact
	rm -fR error forms songs
	rm -fR about audio bio photos reviews video
	for i in `find . -name index.html` ; do rm $$i; done
	rm -f administration.cgi
	rm -f lt.cgi
