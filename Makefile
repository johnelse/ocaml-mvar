dist/build/lib-mvar/mvar.cmxa:
	obuild configure --enable-tests
	obuild build

install:
	ocamlfind install mvar lib/META \
		$(wildcard dist/build/lib-mvar/*)

uninstall:
	ocamlfind remove mvar

.PHONY: clean test
clean:
	rm -rf dist

test:
	obuild test --output
