bin_PROGRAMS	=	disk2vlbi

disk2vlbi_SOURCES	= 	main.cc
disk2vlbi_LDADD	= 	-lpcap \
			-L@BOOST_LIBDIR@ \
			-lboost_program_options-mt \
			-lboost_system-mt \
			-lboost_thread-mt

INCLUDES	= 	-I/usr/include  \
			-I@BOOST_INCDIR@

AM_CXXFLAGS	= 	-O4

