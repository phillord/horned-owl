##
## Generate the RDF files from our OWL presentation syntax files.
##
## This isn't something I plan to do regularly, so they will also get
## checked in.
OWL_FILES_RAW=$(addsuffix .owl, $(basename $(notdir $(shell find ../owl-xml -name *owx))))
OWL_DO_NOT=one-class-broken.owl family.owl family-other.owl import.owl
OWL_FILES=$(filter-out $(OWL_DO_NOT), $(OWL_FILES_RAW))

%.owl: ../owl-xml/%.owx
	robot convert --input $< --output $@

all: $(OWL_FILES)

clean:
	- rm $(OWL_FILES)

dump:
	echo $(OWL_FILES)
