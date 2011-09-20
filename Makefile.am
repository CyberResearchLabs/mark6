MAINTAINERCLEANFILES	=	Makefile.in
EXTRA_DIST		=	setup
SUBDIRS			=	src doc
dist_bin_SCRIPTS	=	bin/net2disk.sh bin/setup.sh bin/mark6.sh bin/tstmark6.sh
sysconf_DATA		=	src/net2disk/net2disk-log.cfg
logdir 			= 	log
DISTCHECK_CONFIGURE_FLAGS=\
				BOOST_LIBDIR=/usr/lib64/boost141 \
				BOOST_INCDIR=/usr/include/boost141

mark6logdir		=	${prefix}/log
mark6log_DATA		=	log/.dummy
EXTRA_DIST		=	${mark6log_DATA}

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
