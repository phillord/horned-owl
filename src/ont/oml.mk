## OML test fixtures, generated from the bubo ontologies with the OML
## reference tools. Run as `make -f oml.mk` from this directory.
##
##   bubo/X.clj  -> oml-annotated/X.ttl   (bubo; OML marker annotations added)
##   oml-annotated/X.ttl -> oml-from-bubo/X.oml  (owl2oml)
##   oml-from-bubo/X.oml -> owl-from-oml/X.ofn   (oml2owl; the expected OWL)
##
## Needs opencaesar/owl-adapter built under JDK 21 with
##   ./gradlew :owl2oml:installDist :oml2owl:installDist
## and a checkout of opencaesar/core-vocabularies for the W3C vocabularies
## that OML files import. The generated files are committed, so the tools
## are only needed to regenerate them. oml-annotated is build scratch.

BUBO ?= $(wildcard ../../dev/bubo-*)
OWL_ADAPTER ?= $(HOME)/subversion-checkout/owl-adapter
OWL2OML ?= $(OWL_ADAPTER)/owl2oml/build/install/owl2oml/bin/owl2oml
OML2OWL ?= $(OWL_ADAPTER)/oml2owl/build/install/oml2owl/bin/oml2owl
OML_CORE ?= $(HOME)/subversion-checkout/core-vocabularies/src/oml

## owl2oml produces no .oml for these.
##   data-property-range, suboproperty-top: use xsd:real / owl:topObjectProperty,
##     which core-vocabularies does not define.
##   object-has-value, happy_person: individuals, which a Vocabulary cannot hold.
##   import-and-annotation, ontology-annotation: owl2oml throws a
##     NullPointerException.
##   annotation-same-value-two-annotations, object-one-of, some-inverse: bubo
##     fails in the OWL API OBO writer, so there is no oml-annotated copy.
##   import, other-iri: bubo fails before saving anything.
##   ont-with-bfo: imports BFO over the network, which owl2oml cannot resolve
##     (it writes an empty .oml).
OML_EXCLUDE = data-property-range suboproperty-top object-has-value \
	happy_person import-and-annotation ontology-annotation \
	annotation-same-value-two-annotations object-one-of some-inverse \
	import other-iri ont-with-bfo

OML_STEMS = $(filter-out $(OML_EXCLUDE),$(basename $(notdir $(wildcard bubo/*.clj))))
## Support scripts that other scripts load; they save nothing themselves.
OML_STEMS := $(filter-out ontology other other-property save oml swrl_rule_support,$(OML_STEMS))

IN_CATALOG = <?xml version="1.0"?><catalog xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog" prefer="public"><rewriteURI uriStartString="http://www.example.com/" rewritePrefix="./"/></catalog>
TREE_CATALOG = <?xml version="1.0"?><catalog xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog" prefer="public"><rewriteURI uriStartString="http://" rewritePrefix="./"/></catalog>

## The generated files are committed, so a machine without the OML tools
## skips regeneration rather than failing.
OML_MISSING = $(strip $(if $(wildcard $(OWL2OML)),,$(OWL2OML)) \
	$(if $(wildcard $(OML2OWL)),,$(OML2OWL)) \
	$(if $(wildcard $(OML_CORE)/www.w3.org),,$(OML_CORE)))

## Hand-written OML, laid out by ontology IRI without the scheme.
OML_MANUAL = $(patsubst oml-manual/%.oml,owl-from-oml-manual/%.ofn,$(shell find oml-manual -name '*.oml'))

## Invalid OML. oml2owl either rejects each file or accepts it with warnings;
## its verdict is what the validator is tested against.
OML_INVALID = $(patsubst %.oml,%.oracle,$(wildcard oml-invalid/*.oml))

ifeq ($(OML_MISSING),)
oml: $(OML_STEMS:%=owl-from-oml/%.ofn) $(OML_MANUAL) $(OML_INVALID)
else
oml:
	@echo "oml: skipped, not found: $(OML_MISSING)"
endif

oml-annotated/%.ttl: bubo/%.clj
	mkdir -p oml-annotated
	cd bubo && $(abspath $(BUBO)) $*.clj

oml-from-bubo/%.oml: oml-annotated/%.ttl
	mkdir -p oml-from-bubo
	w=$$(mktemp -d); trap 'rm -rf "$$w"' EXIT; \
	mkdir -p $$w/in $$w/out; \
	cp $< $$w/in/iri.ttl; \
	printf '%s' '$(IN_CATALOG)' > $$w/in/catalog.xml; \
	printf '%s' '$(TREE_CATALOG)' > $$w/out/catalog.xml; \
	cp -r $(OML_CORE)/www.w3.org $(OML_CORE)/purl.org $$w/out/; \
	$(OWL2OML) -i $$w/in/catalog.xml -o $$w/out/catalog.xml -s $$w/out \
		-if ttl -of oml > $$w/log 2>&1; \
	test -f $$w/out/www.example.com/iri.oml || { cat $$w/log; exit 1; }; \
	cp $$w/out/www.example.com/iri.oml $@

owl-from-oml/%.ofn: oml-from-bubo/%.oml
	mkdir -p owl-from-oml
	w=$$(mktemp -d); trap 'rm -rf "$$w"' EXIT; \
	mkdir -p $$w/oml/www.example.com; \
	cp $< $$w/oml/www.example.com/iri.oml; \
	cp -r $(OML_CORE)/www.w3.org $(OML_CORE)/purl.org $$w/oml/; \
	printf '%s' '$(subst ./,oml/,$(TREE_CATALOG))' > $$w/catalog.xml; \
	$(OML2OWL) -i $$w/catalog.xml -o $$w/owl/catalog.xml -f fss > $$w/log 2>&1; \
	test -f $$w/owl/www.example.com/iri.fss || { cat $$w/log; exit 1; }; \
	cp $$w/owl/www.example.com/iri.fss $@

owl-from-oml-manual/%.ofn: oml-manual/%.oml
	mkdir -p $(dir $@)
	w=$$(mktemp -d); trap 'rm -rf "$$w"' EXIT; \
	mkdir -p $$w/oml; \
	cp -r oml-manual/. $$w/oml/; \
	cp -r $(OML_CORE)/www.w3.org $(OML_CORE)/purl.org $$w/oml/; \
	printf '%s' '$(subst ./,oml/,$(TREE_CATALOG))' > $$w/catalog.xml; \
	$(OML2OWL) -i $$w/catalog.xml -o $$w/owl/catalog.xml -f fss > $$w/log 2>&1; \
	test -f $$w/owl/$*.fss || { cat $$w/log; exit 1; }; \
	cp $$w/owl/$*.fss $@

## oml2owl validates everything in its input, so each file is converted alone,
## beside the hand-written vocabularies it may use. The first line is the
## verdict; the rest is what oml2owl said.
oml-invalid/%.oracle: oml-invalid/%.oml
	w=$$(mktemp -d); trap 'rm -rf "$$w"' EXIT; \
	mkdir -p $$w/oml/example.com/invalid; \
	cp -r oml-manual/. $$w/oml/; \
	cp $< $$w/oml/example.com/invalid/; \
	cp -r $(OML_CORE)/www.w3.org $(OML_CORE)/purl.org $$w/oml/; \
	printf '%s' '$(subst ./,oml/,$(TREE_CATALOG))' > $$w/catalog.xml; \
	if $(OML2OWL) -i $$w/catalog.xml -o $$w/owl/catalog.xml -f fss > $$w/log 2>&1; \
	then echo accepted > $@; else echo rejected > $@; fi; \
	sed -n '/IllegalStateException/,/^\sat /p' $$w/log | grep -v '^\sat ' | sed "s#$$w/oml/##" >> $@; true

## Keep oml-from-bubo, which make would otherwise delete as an intermediate.
.SECONDARY:

.PHONY: oml
