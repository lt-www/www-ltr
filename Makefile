build-ghc:
	(cd hs ; make build-ghc)
	(rm -f bin/cmark ; mkdir -p bin ; ln -s ~/opt/bin/cmark bin/cmark)

resize-genera:
	sh sh/cover.sh
	(cd data/png/icon ; make all)

setup-editor:
	mkdir -p e u
	rm -f e/index.cgi u/index.cgi
	cp $(HOME)/sw/www-minus/py/editor.py e/index.cgi
	cp $(HOME)/sw/www-minus/py/upload.py u/index.cgi

ln-config:
	ln -s data/config/htaccess .htaccess

ln-shows:
	(cd data/md ; ln -s tour.md shows.md)

all: resize-genera build-ghc setup-editor

clean:
	(cd hs ; make clean)
	rm -f index.cgi

pull-lt:
	git pull https://github.com/lt-www/www-ltr main

push-lt:
	git push git@github.com:lt-www/www-ltr main

push-rd:
	git push ssh://rd@rohandrape.net/~rd/ut/www-ltr.git master

remote-update:
	ssh lucie@luciethorne.com "(cd luciethorne.com;make pull-lt)"

remote-rebuild:
	ssh lucie@luciethorne.com "(cd luciethorne.com;make pull-lt all)"
