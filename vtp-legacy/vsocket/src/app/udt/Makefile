C++ = g++

ifndef os
   os = LINUX
endif

ifndef arch
   arch = IA32
endif

CCFLAGS = -Wall -D$(os) -I. -finline-functions -O2 -DSOCKET -g
#-DTRACE

ifeq ($(arch), IA32)
   CCFLAGS += -DIA32 #-mcpu=pentiumpro -march=pentiumpro -mmmx -msse
endif

ifeq ($(arch), POWERPC)
   CCFLAGS += -mcpu=powerpc
endif

ifeq ($(arch), IA64)
   CCFLAGS += -DIA64
endif

LDFLAGS = -lstdc++ -lpthread -lm -L. -ludt

ifeq ($(os), UNIX)
   LDFLAGS += -lsocket
endif

OBJS = appserver.o appclient.o sendfile.o recvfile.o tcpsocket.o udpsocket.o options.o

DIR = $(shell pwd)

all: appserver appclient sendfile recvfile 

%.o: %.cpp
	$(C++) $(CCFLAGS) $< -c

appserver: appserver.o tcpsocket.o udpsocket.o options.o
	$(C++) $^ -o $@ $(LDFLAGS)
appclient: appclient.o tcpsocket.o udpsocket.o options.o
	$(C++) $^ -o $@ $(LDFLAGS)
sendfile: sendfile.o
	$(C++) $^ -o $@ $(LDFLAGS)
recvfile: recvfile.o
	$(C++) $^ -o $@ $(LDFLAGS)

clean:
	rm -f *.o
	rm -f appserver appclient sendfile recvfile 

install:
	export PATH=$(DIR):$$PATH
