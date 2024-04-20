all:
	$(MAKE) -C bubo

all-and-test:
	$(MAKE) -C bubo
	cargo test

clean:
	$(MAKE) -C bubo clean

force-clean:
	-rm owl-xml/*owx
	-rm owl-rdf/*owl
	-rm owl-ttl/*ttl
	-rm owl-functional/*ofn
