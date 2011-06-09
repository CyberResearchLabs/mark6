include Makefile.inc

# Ubuntu pre-requisite packages.
# PREQ="ncurses-dev libssl-dev build-essential git-core"

all:	clean $(DIM) $(DOM)

run:
	${MARK6_APP} console

preq:
	sudo /usr/bin/apt-get -y install $(PREQ)

install:	preq
	cd ${OTP_DIR}
	/bin/tar xzf ${OTP_SRC}.tar.gz
	cd ${OTP_SRC}
	./configure --prefix=${HOME}
	make install
	make clean
	cd ..
	rm -fr ${OTP_SRC}

$(DIM):
	cd src; $(MAKE) $(DIM)

$(DOM):
	cd src; $(MAKE) $(DOM)

tests:
	cd src; $(MAKE) tests

clean:
	cd src; $(MAKE) clean
