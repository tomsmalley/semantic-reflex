docs: ghcjs
	chmod -R +w docs
	cp -r result/dist/js docs
	cp -r result/dist/semantic.min.css docs
	cp -r result/dist/themes docs

docs-nocc: ghcjs-nocc
	chmod -R +w docs
	cp -r result/dist/js docs
	cp -r result/dist/semantic.min.css docs
	cp -r result/dist/themes docs

ghcjs: semantic-reflex example
	nix-build --arg ghcjs true

ghcjs-nocc: semantic-reflex example
	nix-build --arg ghcjs true --arg runCC false
