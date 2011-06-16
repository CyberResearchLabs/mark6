include ../../Makefile.inc

LIBS	=	-lpthread \
		-ldl \
		-lboost_program_options-mt \
		-lboost_system-mt \
		-lboost_thread-mt \
		-lm \
		-lcppunit \
		-llog4cxx 

SOURCES	=	main.cc \
		parser.cc
OBJECTS	=	$(SOURCES:.cc=.o)

TEST_DOM =	test_$(DOM)
TEST_SOURCES =	test_parser.cc \
		test_main.cc
TEST_OBJECTS =	$(TEST_SOURCES:.cc=.o)


all:	clean $(SOURCES) $(DOM) $(TEST_DOM)

$(DOM):	$(OBJECTS)
	$(ECHO) building $(DOM)
	$(CC) $(LDFLAGS) $(INCLUDES) $(OBJECTS) $(LIBS) -o $@

$(TEST_DOM):	$(TEST_OBJECTS)
	$(ECHO) building $(TEST_DOM)
	$(CC) $(LDFLAGS) $(INCLUDES) $(TEST_OBJECTS) $(LIBS) -o $@

test:	$(TEST_DIM)
	$(ECHO) running unit tests
	./$(TEST_DOM)

.cc.o:
	$(CC) $(CFLAGS) $(INCLUDES) $(DEFINES) $< -o $@

tar: $(SOURCES)
	tar cf src.tar $(SOURCES) Makefile

clean:
	$(ECHO) cleaning up in src
	-$(RM) -f $(DOM) $(OBJECTS)
	-$(RM) -f *.log
	-$(RM) -f *.dat

dist:	
	$(ECHO) creating distribution
	$(CP) $(EXECUTABLE) $(DISTRIB)
	scp $(EXECUTABLE) oper@mark554:
