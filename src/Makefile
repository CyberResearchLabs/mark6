include ../Makefile.inc

all:	clean $(DIM) $(DOM)

$(DIM):
	cd dim; $(MAKE) $(DIM)

$(DOM):
	cd dom; $(MAKE) $(DOM)

tests:
	cd dim; $(MAKE) tests
	cd dom; $(MAKE) tests

clean:
	cd dim; $(MAKE) clean
	cd dom; $(MAKE) clean
