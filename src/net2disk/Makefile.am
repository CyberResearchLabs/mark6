bin_PROGRAMS	=	net2disk
check_PROGRAMS	=	net2disktest

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

net2disktest_SOURCES=	test_main.cc \
			buffer_pool.h \
			buffer_pool.cc \
			test_pool.h \
			test_pool.cc

net2disktest_LDADD= 	-L. \
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

AM_CXXFLAGS	= 	-O4

TESTS_ENVIRONMENT=	LD_LIBRARY_PATH=/usr/local/lib:/usr/lib64/boost
TESTS		=	net2disktest
