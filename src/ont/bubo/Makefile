## Exclude these because they will be generated when something else imports them
EXCLUDE=ontology.clj other.clj other-property.clj

PREQ=$(wildcard *.clj)
PREQ_EXC=$(filter-out $(EXCLUDE),$(PREQ))

TARGETS=

PREQ_STEM=$(subst .clj,,$(PREQ_EXC))

## Use "&" to indicate a grouping target which is only available in
## make 4.3. It causes warnings in lower versions.
define build_template
  ../owl-rdf/$(1).owl ../owl-xml/$(1).owx ../owl-ttl/$1.ttl &: $1.clj
	bubo $1.clj

  TARGETS+=../owl-rdf/$(1).owl ../owl-xml/$(1).owx ../owl-ttl/$(1).ttl
endef

##$(foreach p,$(PREQ_STEM),$(info $(call build_template,$(p))))
$(foreach p,$(PREQ_STEM),$(eval $(call build_template,$(p))))

all: $(TARGETS)

.DEFAULT_GOAL=all

clean:
	-rm $(TARGETS)

.PHONY: all clean

