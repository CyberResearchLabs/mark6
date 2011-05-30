
CC=		g++
CFLAGS=		-c \
		-Wall
INCLUDES=	-I/opt/src/mark6/include \
		-I/opt/local/include
DEFINES=	-DLINUX
LDFLAGS=	-L/opt/local/lib
LIBS=		-lpthread \
		-ldl \
		-lboost_program_options-mt \
		-lboost_system-mt \
		-lboost_thread-mt \
		-lm \
		-llog4cxx 
EXECUTABLE=vrtp


SOURCES=	main.cc

OBJECTS=$(SOURCES:.cpp=.o)

all:	$(SOURCES) $(EXECUTABLE)

$(EXECUTABLE):	$(OBJECTS)
	$(CC)  $(LDFLAGS) $(INCLUDES) $(OBJECTS) $(LIBS) -o $@

.cpp.o:
	$(CC) $(CFLAGS) $(INCLUDES) $(DEFINES) $< -o $@

tar: ${SOURCES} ${HDR}
	tar cf src.tar $(SOURCES) $(HDR) Makefile

clean:
	rm $(EXECUTABLE) *.o

dist:	
	cp $(EXECUTABLE) /mnt/data1
	scp $(EXECUTABLE) oper@mark554:
