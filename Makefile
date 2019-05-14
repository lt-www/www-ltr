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

copy-config:
	cp data/config/htaccess .htaccess

all: resize-genera build-ghc setup-editor copy-config

clean:
	(cd hs ; make clean)
	rm -f index.cgi

push-rd:
	git push ssh://rd@rohandrape.net/~rd/ut/www-ltr.git master

pull-rd:
	git pull ssh://rd@rohandrape.net/~rd/ut/www-ltr.git

remote-update:
	ssh lucie@luciethorne.com "(cd luciethorne.com;make pull-rd)"

remote-rebuild:
	ssh lucie@luciethorne.com "(cd luciethorne.com;make pull-rd all)"

remote-fetch:
	ssh lucie@luciethorne.com "(cd luciethorne.com;make push-rd)"
	make pull-rd
