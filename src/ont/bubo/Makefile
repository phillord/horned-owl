## Use "&" to indicate a grouping target which is only available in
## make 4.3. It causes warnings in lower versions.

## Pick up the bubo jar from the dev directory rather than PATH, using
## a wildcard so this does not need updating when the version changes.
BUBO=$(wildcard ../../../dev/bubo-*)

define build_template
  ../owl-rdf/$(1).owl ../owl-xml/$(1).owx ../owl-ttl/$1.ttl ../owl-functional/$(1).ofn ../owl-manchester/$(1).omn&: $1.clj
	$(BUBO) $1.clj
	## clean to make sure that tests regenerate appropriately
	cargo clean

  $(2)+=../owl-rdf/$(1).owl ../owl-xml/$(1).owx ../owl-ttl/$(1).ttl ../owl-functional/$(1).ofn ../owl-manchester/$(1).omn
endef

## Exclude these because they will be generated when something else imports them
EXCLUDE=ontology.clj other.clj other-property.clj save.clj swrl_rule_support.clj

PREQ=$(wildcard *.clj)
PREQ_EXC=$(filter-out $(EXCLUDE),$(PREQ))

TARGETS=

PREQ_STEM=$(subst .clj,,$(PREQ_EXC))

#$(foreach p,$(PREQ_STEM),$(info $(call build_template,$(p),TARGETS)))
$(foreach p,$(PREQ_STEM),$(eval $(call build_template,$(p),TARGETS)))

AMBIG=
AMBIG_PREQ=$(wildcard ambiguous/*.clj)
AMBIG_STEM=$(subst .clj,,$(AMBIG_PREQ))

##$(foreach p,$(AMBIG_STEM),$(info $(call build_template,$(p),AMBIG)))
$(foreach p,$(AMBIG_STEM),$(eval $(call build_template,$(p),AMBIG)))

ambig: $(AMBIG)

WITHIMPORT=
WITHIMPORT_PREQ=$(wildcard withimport/*.clj)
WITHIMPORT_STEM=$(subst .clj,,$(WITHIMPORT_PREQ))

##$(foreach p,$(WITHIMPORT_STEM),$(info $(call build_template,$(p),WITHIMPORT)))
$(foreach p,$(WITHIMPORT_STEM),$(eval $(call build_template,$(p),WITHIMPORT)))

withimport:$(WITHIMPORT)

## withcatalog/generate-catalog.clj doesn't fit build_template -- it
## doesn't call save-all, and it produces one real OWLZipSaver-generated
## catalog-v001.xml (not five parallel OWL-format files), so it gets its
## own explicit rule rather than being folded into the WITHIMPORT loop.
../owl-rdf/withcatalog/catalog-v001.xml: withcatalog/generate-catalog.clj withimport/other-property.clj
	$(BUBO) script withcatalog/generate-catalog.clj

withcatalog: ../owl-rdf/withcatalog/catalog-v001.xml

all: $(TARGETS) ambig withimport withcatalog

.DEFAULT_GOAL=all

clean:
	-rm $(TARGETS) $(AMBIG) $(WITHIMPORT) ../owl-rdf/withcatalog/catalog-v001.xml

.PHONY: all clean withcatalog
