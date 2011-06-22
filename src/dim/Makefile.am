bin_PROGRAMS	=	dim6
check_PROGRAMS	=	dim6test


dim6_SOURCES	= 	main.cc \
			mark6.h \
			configuration.h \
			receiver_thread.h \
			dim6.xml \
			dim6-log.cfg

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


dim6test_SOURCES =	test_main.cc test_writer_thread_pool.h \
			test_writer_thread_pool.cc

#			test_file_manager.cc \
#			test_file_manager.h \
#			test_file_writer.cc \
#			test_file_writer.h \
#			test_writer_thread_pool.cc \
#			test_writer_thread_pool.h \
#			test_main.cc 

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
			logger.h \
			file_writer.cc \
			file_writer.h \
			file_manager.cc \
			file_manager.h \
			socket_buffer.cc \
			socket_buffer.h \
			writer_thread_pool.cc \
			writer_thread_pool.h \
			io.cc \
			io.h


INCLUDES	= 	-I@top_srcdir@/src/dim \
			-I/usr/include \
			-I@BOOST_INCDIR@

AM_CXXFLAGS	= 	-g

TESTS_ENVIRONMENT=	LD_LIBRARY_PATH=/usr/local/lib:/usr/lib64/boost141
TESTS		=	dim6test
