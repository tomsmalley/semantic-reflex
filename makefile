docs: ghcjs
	chmod -R +w docs
	nix-shell -p closurecompiler --run 'closure-compiler \
		--js_output_file docs/js/all.js \
		--externs=./result/bin/semantic-reflex-example.jsexe/all.js.externs \
		./result/bin/semantic-reflex-example.jsexe/all.js \
		--jscomp_off=checkVars \
		-O ADVANCED -W QUIET'

docs-nocc: ghcjs
	chmod -R +w docs
	cp ./result/bin/semantic-reflex-example.jsexe/all.js docs/js/all.js

ghcjs: semantic-reflex semantic-reflex-example
	nix-build --attr ghcjs.semantic-reflex-example --option extra-binary-caches https://nixcache.reflex-frp.org
