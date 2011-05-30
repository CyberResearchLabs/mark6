INCLUDES=\
-I$(top_srcdir)/src/dutils \
-I$(top_srcdir)/src/vsocket \
-I$(top_srcdir)/src/include
lib_LIBRARIES = libvsocket.a
libvsocket_a_SOURCES= \
File.h \
Socket.h \
TCPSocket.h \
UDPSocket.h \
VRSocketThread.h \
VRSocket.h \
VRServerSocket.h  \
VRClientSocket.h  \
RTP.h  \
RTPSession.h  \
Scan.h \
File.cpp \
Socket.cpp \
TCPSocket.cpp \
UDPSocket.cpp \
VRSocketThread.cpp  \
VRSocket.cpp \
VRServerSocket.cpp  \
VRClientSocket.cpp  \
RTPSession.cpp  
libvsocket_a_CXXFLAGS= @CXXFLAGS@ -Wall  -O3
libvsocket_a_LDDFLAGS= -O3
