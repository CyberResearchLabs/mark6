include ../Makefile.inc

all:	clean $(EXECUTABLE) $(GEN)

run:
	${MARK6_APP} console

$(DIM):
	cd dim; $(MAKE) $(DIM)

$(GEN):
	cd gen; $(MAKE) 

tests:
	cd dim; $(MAKE) tests
	cd gen; $(MAKE) tests

clean:
	cd dim; $(MAKE) clean
	cd gen; $(MAKE) clean
