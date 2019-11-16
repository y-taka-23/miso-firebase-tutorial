.PHONY: build deploy

build:
	nix-build

deploy: build
	cp ./result/bin/lambdatalk.jsexe/all.js ./public/
	chmod 644 ./public/all.js
	firebase deploy
