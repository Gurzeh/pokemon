SUBDIRS = bench src tests tools

all: dist/build/pokemon/pokemon
	$<

.PHONY: bench
bench: dist/build/pokemon/pokemon
	cabal bench

.PHONY: check
check: dist/build/pokemon/pokemon
	cabal test

dist/build/pokemon/pokemon: $(shell find $(SUBDIRS) -type f) dist/setup-config
	cabal build --jobs=12

dist/setup-config: pokemon.cabal protos/src/Pokemon.proto
	cabal install --only-dependencies --enable-tests --enable-benchmarks
	cabal configure --disable-profiling --enable-tests --enable-benchmarks

protos/src/Pokemon.proto: $(shell find protos/src/POGOProtos -name "*.proto")
	echo 'syntax = "proto3";' > $@
	(for i in $^; do cat $$i; echo; done) \
		| egrep -v '^(syntax|package|import)' \
		| sed -e 's/\.POGOProtos\.[^ ]*\.//g' \
		>> $@
