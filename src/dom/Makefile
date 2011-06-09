include ../../Makefile.inc

LIBS	=	-lpthread \
		-ldl \
		-lboost_program_options-mt \
		-lboost_system-mt \
		-lboost_thread-mt \
		-lm \
		-llog4cxx 

SOURCES	=	main.cc

OBJECTS=$(SOURCES:.cc=.o)

all:	$(SOURCES) $(DOM)

$(DOM):	$(OBJECTS)
	$(ECHO) building $(DOM)
	$(CC) $(LDFLAGS) $(INCLUDES) $(OBJECTS) $(LIBS) -o $@

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
