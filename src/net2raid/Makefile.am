bin_PROGRAMS	=	net2raid
check_PROGRAMS	=	net2raidtest

net2raid_SOURCES= 	main.cc
net2raid_LDADD	= 	-L. \
			-lnet2raid \
			-lreadline \
			-L@BOOST_LIBDIR@ \
			-lboost_program_options-mt \
			-lboost_system-mt \
			-lboost_thread-mt \
			-lm \
			-lpthread \
			@PFRING_ROOT@/userland/libpcap-1.1.1-ring/libpcap.a \
			@PFRING_ROOT@/userland/lib/libpfring.a

net2raidtest_SOURCES=	test_main.cc \
			file_writer.h \
			file_writer.cc \
			net_reader.h \
			net_reader.cc \
			stats_writer.h \
			stats_writer.cc \
			test_file_writer.h \
			test_file_writer.cc \
			threaded.h

net2raidtest_LDADD= 	-L. \
			-lnet2raid \
			-L@BOOST_LIBDIR@ \
			-lboost_program_options-mt \
			-lboost_system-mt \
			-lboost_thread-mt \
			-lm \
			-lpthread \
			@PFRING_ROOT@/userland/libpcap-1.1.1-ring/libpcap.a \
			@PFRING_ROOT@/userland/lib/libpfring.a

noinst_LIBRARIES=	libnet2raid.a

libnet2raid_a_SOURCES	= \
			pfr.h \
			pfr.cc \
			file_writer.h \
			file_writer.cc \
			logger.h \
			logger.cc \
			net_reader.h \
			net_reader.cc \
			stats_writer.h \
			stats_writer.cc \
			threaded.h

INCLUDES	= 	-I@top_srcdir@/src/net2raid \
			-I/usr/include \
			-I@PFRING_ROOT@/kernel \
			-I@PFRING_ROOT@/kernel/plugins \
			-I@PFRING_ROOT@/userland/lib \
			-I@PFRING_ROOT@/userland/libpcap-1.1.1-ring \
			-I@BOOST_INCDIR@

AM_CXXFLAGS	= 	-O4

TESTS_ENVIRONMENT=	LD_LIBRARY_PATH=/usr/local/lib:/usr/lib64/boost
TESTS		=	net2raidtest
