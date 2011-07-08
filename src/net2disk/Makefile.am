bin_PROGRAMS	=	net2disk

net2disk_SOURCES= 	main.cc \
			net2disk.h

net2disk_LDADD	= 	-L.\
			-lnet2disk \
			-L@PFRING_ROOT@/userland/lib \
			-lpfring \
			-L@PFRING_ROOT@/userland/libpcap-1.1.1-ring \
			-lpcap \
			-L@BOOST_LIBDIR@ \
			-lboost_program_options-mt \
			-lboost_system-mt \
			-lboost_thread-mt \
			-lm \
			-lpthread 



noinst_LIBRARIES=	libnet2disk.a

libnet2disk_a_SOURCES	= \
			pfring_util.cc \
			net2disk.cc \
			net2disk.h

INCLUDES	= 	-I@top_srcdir@/src/net2disk \
			-I/usr/include \
			-I@PFRING_ROOT@/kernel \
			-I@PFRING_ROOT@/kernel/plugins \
			-I@PFRING_ROOT@/userland/lib \
			-I@PFRING_ROOT@/userland/libpcap-1.1.1-ring \
			-I@BOOST_INCDIR@

AM_CXXFLAGS	= 	-O4

TESTS_ENVIRONMENT=	LD_LIBRARY_PATH=/usr/local/lib:/usr/lib64/boost
