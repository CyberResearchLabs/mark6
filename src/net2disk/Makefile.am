bin_PROGRAMS	=	net2disk

net2disk_SOURCES= 	main.cc

net2disk_LDADD	= 	-L. \
			-lnet2disk \
			-L@BOOST_LIBDIR@ \
			-lboost_program_options-mt \
			-lboost_system-mt \
			-lboost_thread-mt \
			-lm \
			-lpthread \
			@PFRING_ROOT@/userland/libpcap-1.1.1-ring/libpcap.a \
			@PFRING_ROOT@/userland/lib/libpfring.a



noinst_LIBRARIES=	libnet2disk.a

libnet2disk_a_SOURCES	= \
			pfr.cc \
			pfr.h \
			logger.cc \
			logger.h

INCLUDES	= 	-I@top_srcdir@/src/net2disk \
			-I/usr/include \
			-I@PFRING_ROOT@/kernel \
			-I@PFRING_ROOT@/kernel/plugins \
			-I@PFRING_ROOT@/userland/lib \
			-I@PFRING_ROOT@/userland/libpcap-1.1.1-ring \
			-I@BOOST_INCDIR@

<<<<<<< HEAD
AM_CXXFLAGS	= 	-O4
=======
AM_CXXFLAGS	= 	-O4 
>>>>>>> parent of 0237785... Lock free circular fifo.

TESTS_ENVIRONMENT=	LD_LIBRARY_PATH=/usr/local/lib:/usr/lib64/boost
