bin_PROGRAMS	=	e2v

e2v_SOURCES	= 	main.cc
e2v_LDADD	= 	-lpcap \
			-L@BOOST_LIBDIR@ \
			-lboost_program_options-mt \
			-lboost_system-mt \
			-lboost_thread-mt

INCLUDES	= 	-I/usr/include  \
			-I@BOOST_INCDIR@

AM_CXXFLAGS	= 	-O4

