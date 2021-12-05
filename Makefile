## The big OWL repository is available here
## https://github.com/phillord/big-owl. It's just a collection of
## big-owl test files. It can be checked out using the big-owl target,
## or just linked from here.

BIG_OWL=./big-owl

all:
	cargo test
	cargo doc
	browser-reload.sh horned_owl

## Build the Unit test Ontology code
just-bubo:
	$(MAKE) -C src/ont/bubo

bubo: just-bubo test

## Saves a bit of typing when switching back and forth
test:
	cargo test

quick-test:
	cargo test --lib --bins --tests -- --skip integration

tmp:
	mkdir tmp

integration-prepare: tmp

integration-clean:
	- rm ./tmp/bfo.owl

## Main integration tests run as unit tests
integration: integration-prepare integration-clean ./tmp/ont-with-bfo.owl
	cargo test integration -- --ignored


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
	cargo run --bin horned-summary --release -- $^

summary-go-plus: ./tmp/go-plus.owl
	cargo run --bin horned-summary --release -- $^

materialize-with-bfo: ./tmp/ont-with-bfo.owl
	cargo run --bin horned-materialize -- $^

parse-go-snippet:
	cargo run --bin horned-parse -- ./dev/go-short.owl

materialize-dto:
	$(MAKE) -C ./big-owl/dto dto_complete.owl
	cargo run --bin horned-materialize -- ./big-owl/dto/dto_complete.owl

parse_dto_gpcr:
	cargo run --bin horned-parse -- ./big-owl/dto/dto_vocabulary_gpcr_gene.owl

triples-round-ont:
	cargo run --bin horned-triples -- --round ./src/ont/owl-rdf/ont.owl

triples-round-class:
	cargo run --bin horned-triples -- --round ./src/ont/owl-rdf/class.owl

triples-round-and:
	cargo run --bin horned-triples -- --round ./src/ont/owl-rdf/and.owl

triples-round-family:
	cargo run --bin horned-triples -- --round ./src/ont/owl-rdf/family.owl

triples-round-all:
	set -e; for i in ./src/ont/owl-rdf/*owl;\
	do cargo run --bin horned-triples -- --round $$i;\
	echo;echo;echo;\
	done
