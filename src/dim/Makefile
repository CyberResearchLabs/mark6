include ../../Makefile.inc

LIBS	=	-lpthread \
		-ldl \
		-lboost_program_options-mt \
		-lboost_system-mt \
		-lboost_thread-mt \
		-lm \
		-llog4cxx 

SOURCES	=	main.cc \
		SocketBuffer.cc \
		UDPSocket.cc \
		IO.cc

OBJECTS=$(SOURCES:.cc=.o)

all:	$(SOURCES) $(DIM)

$(DIM):	$(OBJECTS)
	$(ECHO) building $(DIM)
	$(CC) $(LDFLAGS) $(INCLUDES) $(OBJECTS) $(LIBS) -o $@

.cc.o:
	$(CC) $(CFLAGS) $(INCLUDES) $(DEFINES) $< -o $@

tar: $(SOURCES)
	tar cf src.tar $(SOURCES) Makefile

clean:
	$(ECHO) cleaning up in src
	-$(RM) -f $(DIM) $(OBJECTS)
	-$(RM) -f *.log
	-$(RM) -f *.dat

dist:	
	$(ECHO) creating distribution
	$(CP) $(DIM) $(DISTRIB)
# scp $(DIM) oper@mark554:
