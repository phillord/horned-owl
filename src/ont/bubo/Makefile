## Exclude these because they are support files
EXCLUDE=ontology.clj save.clj

PREQ=$(wildcard *.clj)
PREQ_EXC=$(filter-out $(EXCLUDE),$(PREQ))

TARGETS=

PREQ_STEM=$(subst .clj,,$(PREQ_EXC))

## Use "&" to indicate a grouping target which is only available in
## make 4.3. It causes warnings in lower versions.
define build_template
  ../owl-rdf/$(1).owl ../owl-xml/$(1).owx ../owl-ttl/$1.ttl &: $1.clj
	bubo $1.clj

  $(2)+=../owl-rdf/$(1).owl ../owl-xml/$(1).owx ../owl-ttl/$(1).ttl
endef

#$(foreach p,$(PREQ_STEM),$(info $(call build_template,$(p),TARGETS,)))
$(foreach p,$(PREQ_STEM),$(eval $(call build_template,$(p),TARGETS,)))

NONROUND=
NONROUND_PREQ=$(wildcard nonround/*.clj)
NONROUND_STEM=$(subst .clj,,$(NONROUND_PREQ))


##$(foreach p,$(NONROUND_STEM),$(info $(call build_template,$(p),NONROUND,nonround)))
$(foreach p,$(NONROUND_STEM),$(eval $(call build_template,$(p),NONROUND,nonround)))

nonround: $(NONROUND)

all: $(TARGETS) nonround

.DEFAULT_GOAL=all

clean:
	-rm $(TARGETS) $(NONROUND)

.PHONY: all clean
