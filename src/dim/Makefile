include ../../Makefile.inc

LIBS	=	-lpthread \
		-ldl \
		-lboost_program_options-mt \
		-lboost_system-mt \
		-lboost_thread-mt \
		-lm \
		-llog4cxx \
		-lcppunit

SOURCES	=	main.cc \
		logger.cc \
		file_writer.cc \
		file_manager.cc \
		socket_buffer.cc \
		udp_socket.cc \
		writer_thread_pool.cc \
		io.cc
OBJECTS=$(SOURCES:.cc=.o)

TEST_DIM =	test_$(DIM)
TEST_SOURCES =	test_file_manager.cc \
		test_file_writer.cc \
		test_writer_thread_pool.cc \
		file_writer.cc \
		file_manager.cc \
		writer_thread_pool.cc \
		logger.cc \
		test_main.cc
TEST_OBJECTS =	$(TEST_SOURCES:.cc=.o)


all:	$(SOURCES) $(DIM) $(TEST_DIM)

$(DIM):	$(OBJECTS)
	$(ECHO) building $(DIM)
	$(CC) $(LDFLAGS) $(INCLUDES) $(OBJECTS) $(LIBS) -o $@

$(TEST_DIM):	$(TEST_OBJECTS)
	$(ECHO) building $(TEST_DIM)
	$(CC) $(LDFLAGS) $(INCLUDES) $(TEST_OBJECTS) $(LIBS) -o $@

test:	$(TEST_DIM)
	$(ECHO) running unit tests
	./$(TEST_DIM)

.cc.o:
	$(CC) $(CFLAGS) $(INCLUDES) $(DEFINES) $< -o $@

tar: $(SOURCES)
	tar cf src.tar $(SOURCES) Makefile

clean:
	$(ECHO) cleaning up in src
	-$(RM) -f $(DIM) $(TEST_DIM) $(OBJECTS) $(TEST_OBJECTS)
	-$(RM) -f *.log
	-$(RM) -f *.dat

dist:	
	$(ECHO) creating distribution
	$(CP) $(DIM) $(DISTRIB)
# scp $(DIM) oper@mark554:
