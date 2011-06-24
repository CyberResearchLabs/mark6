MAINTAINERCLEANFILES	=	Makefile.in
EXTRA_DIST		=	setup bin/dim6.sh
SUBDIRS			=	src doc
DISTCHECK_CONFIGURE_FLAGS=\
				BOOST_LIBDIR=/usr/lib64/boost141 \
				BOOST_INCDIR=/usr/include/boost141

distclean-local:
	-rm -rf Makefile.in \
		aclocal.m4 \
		autom4te.cache \
		config \
		config.h \
		config.h.in \
		config.log \
		config.status \
		stamp-h1 \
		src/Makefile \
		src/Makefile.in \
		src/dim/Makefile \
		src/extern/apache-log4cxx-0.10.0 \
		src/extern/apr-1.4.5 \
		src/extern/apr-util-1.3.12
