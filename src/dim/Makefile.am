bin_PROGRAMS	=	dim6
check_PROGRAMS	=	dim6test

dim6_SOURCES	= 	main.cc

dim6_LDADD	= 	-L.\
			-ldim6 \
			-L@BOOST_LIBDIR@ \
			-lboost_program_options-mt \
			-lboost_system-mt \
			-lboost_thread-mt \
			-ldl \
			-llog4cxx \
			-lm \
			-lpthread 

dim6test_SOURCES =	test_file_manager.cc \
			test_file_writer.cc \
			test_writer_thread_pool.cc \
			test_main.cc

dim6test_LDADD	= 	-L. \
			-ldim6 \
			-L@BOOST_LIBDIR@ \
			-lboost_program_options-mt \
			-lboost_system-mt \
			-lboost_thread-mt \
			-ldl \
			-llog4cxx \
			-lm \
			-lpthread  \
			-lcppunit

noinst_LIBRARIES=	libdim6.a
libdim6_a_SOURCES	= \
			logger.cc \
			file_writer.cc \
			file_manager.cc \
			socket_buffer.cc \
			udp_socket.cc \
			writer_thread_pool.cc \
			io.cc

INCLUDES	= 	-I. \
			-I/usr/include \
			-I@BOOST_INCDIR@
			
AM_CXXFLAGS	= 	-O3

TESTS		=	dim6test
