bin_PROGRAMS	= dim6 dim6test
dim6_SOURCES	= 	main.cc \
			logger.cc \
			file_writer.cc \
			file_manager.cc \
			socket_buffer.cc \
			udp_socket.cc \
			writer_thread_pool.cc \
			io.cc

dim6_LDADD	= 	-L/usr/lib64/boost141 \
			-lpthread \
			-ldl \
			-lboost_program_options-mt \
			-lboost_system-mt \
			-lboost_thread-mt \
			-lm \
			-llog4cxx \
			-lcppunit

dim6test_SOURCES =	test_file_manager.cc \
			test_file_writer.cc \
			test_writer_thread_pool.cc \
			file_writer.cc \
			file_manager.cc \
			writer_thread_pool.cc \
			logger.cc \
			test_main.cc

dim6test_LDADD	= 	-L/usr/lib64/boost141 \
			-lpthread \
			-ldl \
			-lboost_program_options-mt \
			-lboost_system-mt \
			-lboost_thread-mt \
			-lm \
			-llog4cxx \
			-lcppunit

INCLUDES	= 	-I. \
			-I../include \
			-I/usr/include \
			-I/usr/include/boost141 
			
AM_CXXFLAGS	= -O3
