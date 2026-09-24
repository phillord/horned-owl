## The big OWL repository is available here
## https://github.com/phillord/big-owl. It's just a collection of
## big-owl test files. It can be checked out using the big-owl target,
## or just linked from here.

BIG_OWL=./big-owl

all: bubo
	cargo doc
	browser-reload.sh horned_owl

pre-commit: quick-test
pre-push: all-tests
	cargo bench --no-run



## Bench prepare
bench-prepare:
	mkdir --parents target/bench-data
	for i in 10 100 1000 2500 5000 10000;\
	do \
		cargo run --bin horned-big -- --format owl $$i > target/bench-data/o$$i.owl; \
		cargo run --bin horned-big -- --format owx $$i > target/bench-data/o$$i.owx; \
	done


## Build the Unit test Ontology code
##
## `test_resources` (the test-generator proc-macro driving src/ont/*
## fixture discovery) globs those directories at compile time with
## nothing telling Cargo to invalidate the build when files there
## change, so a stale test binary can silently miss new/changed/removed
## fixtures. `cargo clean` unconditionally after bubo runs so the next
## `cargo test` always sees current fixtures.
just-bubo:
	$(MAKE) -C src/ont/bubo

bubo: just-bubo test

clean:
	cargo clean

## Saves a bit of typing when switching back and forth
test:
	cargo test

quick-test:
	cargo test --lib --bins --tests -- --skip integration

tmp:
	mkdir tmp

all-tests: integration-prepare integration-clean ./tmp/ont-with-bfo.owl
	cargo test -- --include-ignored
	cargo test integration -- --include-ignored

integration-prepare: tmp

integration-clean:
	rm -f ./tmp/bfo.owl

## horned-bin's CLI tests (test_horned_*.rs) are mostly plain #[test]s named
## with an `integration_` prefix, not #[ignore]d, plus one genuinely
## #[ignore]d test (integration_ont_with_bfo) that needs the ./tmp/ fixture
## prepared above. `--include-ignored` (not bare `--ignored`, which would
## exclude every non-ignored one) runs both in a single invocation.
integration: integration-prepare integration-clean ./tmp/ont-with-bfo.owl
	cargo test integration -- --include-ignored


## A set of targets which run more interactive test
big-owl:
	git clone https://github.com/phillord/big-owl.git
	$(MAKE) -C big-owl

./tmp/go-plus.owl: $(BIG_OWL)/go/go-plus.owl
	cp $^ $@

./tmp/go.owl: $(BIG_OWL)/go/go.owl
	cp $^ $@

./tmp/ont-with-bfo.owl: ./src/ont/owl-rdf/ont-with-bfo.owl
	cp $^ $@

## Use a release build because we mostly save more time parsing go,
## than we loose do the release
summary-go: ./tmp/go.owl
	cargo run -p horned-bin --bin horned-summary --release -- $^

summary-go-plus: ./tmp/go-plus.owl
	cargo run -p horned-bin --bin horned-summary --release -- $^

materialize-with-bfo: ./tmp/ont-with-bfo.owl
	cargo run -p horned-bin  --bin horned-materialize -- $^

parse-go-snippet:
	cargo run -p horned-bin  --bin horned-parse -- ./dev/go-short.owl

materialize-dto:
	$(MAKE) -C ./big-owl/dto dto_complete.owl
	cargo run -p horned-bin  --bin horned-materialize -- ./big-owl/dto/dto_complete.owl

parse_dto_gpcr:
	cargo run -p horned-bin  --bin horned-parse -- ./big-owl/dto/dto_vocabulary_gpcr_gene.owl

triples-round-ont:
	cargo run -p horned-bin  --bin horned-triples -- --round ./src/ont/owl-rdf/ont.owl

triples-round-class:
	cargo run -p horned-bin  --bin horned-triples -- --round ./src/ont/owl-rdf/class.owl

triples-round-and:
	cargo run -p horned-bin  --bin horned-triples -- --round ./src/ont/owl-rdf/and.owl

triples-round-family:
	cargo run -p horned-bin  --bin horned-triples -- --round ./src/ont/owl-rdf/family.owl

triples-round-all:
	set -e; for i in ./src/ont/owl-rdf/*owl;\
	do cargo run -p horned-bin  --bin horned-triples -- --round $$i;\
	echo;echo;echo;\
	done

clippy:
	cargo clippy --workspace --all-targets

install:
	cargo install --path horned-bin

fetch_bubo:
	wget https://github.com/phillord/tawny-bubo/releases/download/0.4.0/bubo-0.4.0 -O dev/bubo-0.4.0
	chmod +x dev/bubo-0.4.0

-include makefile-local
