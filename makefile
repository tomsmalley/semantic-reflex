docs: ghcjs
	chmod -R +w docs
	cp -r result/dist/js docs
	cp result/dist/semantic.min.css docs
	cp -r result/dist/themes docs
	cp -r semantic-reflex-example/resources/* docs

docs-nocc: ghcjs-nocc
	chmod -R +w docs
	cp -r result/dist/js docs
	cp result/dist/semantic.min.css docs
	cp -r result/dist/themes docs
	cp -r semantic-reflex-example/resources/* docs

ghcjs: semantic-reflex semantic-reflex-example
	nix-build --attr ghcjs.semantic-reflex-example

ghcjs-nocc: semantic-reflex semantic-reflex-example
	nix-build --attr ghcjs.semantic-reflex-example --arg runCC false
