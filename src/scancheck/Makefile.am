bin_PROGRAMS	=	scancheck

scancheck_SOURCES	= 	main.cc
scancheck_LDADD	= 	-L../common \
			-lcommon \
			-L@BOOST_LIBDIR@ \
			-lboost_program_options-mt \
			-lboost_system-mt 

INCLUDES	= 	-I/usr/include  \
			-I../common \
			-I@BOOST_INCDIR@

AM_CXXFLAGS	= 	-O4

