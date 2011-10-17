bin_PROGRAMS	=	splice

splice_SOURCES= 	main.cc
splice_LDADD	= 	-L. \
			-L../common \
			-lcommon \
			-lreadline \
			-L@BOOST_LIBDIR@ \
			-lboost_program_options-mt \
			-lboost_system-mt \
			-lboost_thread-mt \
			-lm \
			-lpthread 

INCLUDES	= 	-I../common \
			-I@top_srcdir@/src/splice \
			-I/usr/include \
			-I@BOOST_INCDIR@

AM_CXXFLAGS	= 	-O4

TESTS_ENVIRONMENT=	LD_LIBRARY_PATH=/usr/local/lib:/usr/lib64/boost
