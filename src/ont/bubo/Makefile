## Use "&" to indicate a grouping target which is only available in
## make 4.3. It causes warnings in lower versions.
define build_template
  ../owl-rdf/$(1).owl ../owl-xml/$(1).owx ../owl-ttl/$1.ttl &: $1.clj
	bubo $1.clj

  $(2)+=../owl-rdf/$(1).owl ../owl-xml/$(1).owx ../owl-ttl/$(1).ttl
endef

## Exclude these because they are support files
EXCLUDE=ontology.clj save.clj

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

all: $(TARGETS) ambig withimport

.DEFAULT_GOAL=all

clean:
	-rm $(TARGETS) $(AMBIG) $(WITHIMPORT)

.PHONY: all clean
