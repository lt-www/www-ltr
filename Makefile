build-cgi-ghc:
	(cd hs ; make build-cgi-ghc)

build-static-ghc:
	(cd hs ; make build-static-ghc)

build-cabal:
	cabal configure --disable-optimisation
	cabal build
	cp dist/build/ltr-static/ltr-static administration.cgi
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
	rm -f index.cgi

push:
	git push sp
