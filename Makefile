build-ghc:
	(cd hs ; make build-ghc)

build-cabal:
	cabal configure --disable-optimisation
	cabal build
	cp dist/build/ltr-cgi/ltr-cgi index.cgi
	strip -s index.cgi

resize-genera:
	sh sh/cover.sh
	(cd data/png/icon ; make all)

clean:
	cabal clean
	(cd hs ; make clean)
	rm -f index.cgi

push-sp:
	git push sp master

pull-sp:
	git pull sp master

add-remote-sp:
	git remote remove sp
	git remote add sp ssh://rd@slavepianos.org/~rd/ut/www-ltr.git

remote-update:
	ssh lucie@luciethorne.com "(cd luciethorne.com;make pull-sp)"

remote-rebuild:
	ssh lucie@luciethorne.com "(cd luciethorne.com;make pull-sp resize-genera build-ghc)"
