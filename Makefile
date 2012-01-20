build-ghc:
	(cd hs ; make build-ghc)

build-cabal:
	cabal configure --disable-optimisation
	cabal build
	cp dist/build/ltr-cgi/ltr-cgi index.cgi
	strip -s index.cgi

clean:
	cabal clean
	(cd hs ; make clean)
	rm -f index.cgi

push:
	git push sp
