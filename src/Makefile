include ../Makefile.inc

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

all:	$(SOURCES) $(EXECUTABLE)

$(EXECUTABLE):	$(OBJECTS)
	$(ECHO) building $(EXECUTABLE)
	$(CC) $(LDFLAGS) $(INCLUDES) $(OBJECTS) $(LIBS) -o $@

.cc.o:
	$(CC) $(CFLAGS) $(INCLUDES) $(DEFINES) $< -o $@

tar: ${SOURCES} ${HDR}
	tar cf src.tar $(SOURCES) $(HDR) Makefile

clean:
	$(ECHO) cleaning up in src
	-$(RM) -f $(EXECUTABLE) $(OBJECTS)
	-$(RM) -f *.log
	-$(RM) -f *.dat

dist:	
	$(ECHO) creating distribution
	$(CP) $(EXECUTABLE) $(DISTRIB)
	scp $(EXECUTABLE) oper@mark554:
