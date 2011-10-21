MAINTAINERCLEANFILES	=	Makefile.in
SUBDIRS			=	src doc
dist_bin_SCRIPTS	=	src/net2raid/net2raid-run \
				src/net2raid/net2raid-setup \
				src/net2raid/net2raid \
				src/net2raid/net2raid-graphs \
				src/disk2vlbi/disk2vlbi \
				src/scancheck/scancheck \
				bin/mark6 \
				bin/tstmark6 \
				src/vdif/bin/vdif \
				tools/disk/disktool \
				tools/disk/fio 
sysconf_DATA		=	src/net2raid/net2raid-log.cfg \
				tools/disk/disktool.fio \
				tools/disk/disktool.rc \
				rc.mark6
logdir 			= 	log
DISTCHECK_CONFIGURE_FLAGS=\
				BOOST_LIBDIR=/usr/lib64/boost141 \
				BOOST_INCDIR=/usr/include/boost141

mark6logdir		=	${prefix}/log
mark6log_DATA		=	log/.dummy
EXTRA_DIST		=	setup ${mark6log_DATA}


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
