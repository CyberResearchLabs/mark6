
CC=g++
CFLAGS=-c -Wall 
INCLUDES=-I/home/dlapsley/proj/vrtp
DEFINES=-DLINUX
LDFLAGS=
LIBS=-lpthread -L. -ludt
EXECUTABLE=vrtp
HDR=	Client.h \
		Logger.h \
		Mark5IfStream.h \
		Mark5OfStream.h \
		MyException.h \
		Object.h \
		Options.h \
		RTP.h \
		RTPIfStream.h \
		RTPIsStream.h \
		RTPIuStream.h \
		RTPOfStream.h \
		RTPOsStream.h \
		RTPOuStream.h \
		RTPSession.h \
		RTPStream.h \
		Server.h \
		Socket.h \
		SocketBuffer.h \
		TCPSocket.h \
		Test.h \
		TestIfStream.h \
		TestOfStream.h \
		Thread.h \
		UDPSocket.h \
		UDTSocket.h \
		Utils.h

SOURCES=Client.cpp \
		Logger.cpp \
		Mark5IfStream.cpp \
		Mark5OfStream.cpp \
		Object.cpp \
		Options.cpp \
		RTP.cpp \
		RTPIfStream.cpp \
		RTPIsStream.cpp \
		RTPIuStream.cpp \
		RTPOfStream.cpp \
		RTPOsStream.cpp \
		RTPOuStream.cpp \
		RTPSession.cpp \
		RTPStream.cpp \
		Server.cpp \
		SocketBuffer.cpp \
		TCPSocket.cpp \
		Test.cpp \
		TestIfStream.cpp \
		TestOfStream.cpp \
		Thread.cpp \
		UDPSocket.cpp \
		UDTSocket.cpp \
		Utils.cpp \
		main.cpp

OBJECTS=$(SOURCES:.cpp=.o)

all:	$(SOURCES) $(EXECUTABLE)

$(EXECUTABLE):	$(OBJECTS)
	$(CC) $(LDFLAGS) $(OBJECTS) $(LIBS) -o $@

.cpp.o:
	$(CC) $(CFLAGS) $(INCLUDES) $(DEFINES) $< -o $@

tar: ${SOURCES} ${HDR}
	tar cf src.tar $(SOURCES) $(HDR) Makefile

clean:
	rm $(EXECUTABLE) *.o

dist:	
	cp $(EXECUTABLE) /mnt/data1
	scp $(EXECUTABLE) oper@mark554:
	
