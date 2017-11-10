build-ghc:
	(cd hs ; make build-ghc)
	(rm -f bin/cmark ; mkdir -p bin ; ln -s ~/opt/bin/cmark bin/cmark)

resize-genera:
	sh sh/cover.sh
	(cd data/png/icon ; make all)

setup-editor:
	(cd py ; make all)


all: resize-genera build-ghc setup-editor

clean:
	(cd hs ; make clean)
	rm -f index.cgi

push-sp:
	git push ssh://rd@slavepianos.org/~rd/ut/www-ltr.git master

pull-sp:
	git pull ssh://rd@slavepianos.org/~rd/ut/www-ltr.git

remote-update:
	ssh lucie@luciethorne.com "(cd luciethorne.com;make pull-sp)"

remote-rebuild:
	ssh lucie@luciethorne.com "(cd luciethorne.com;make pull-sp all)"
