INCLUDES=\
-I$(top_srcdir)/src/dutils \
-I$(top_srcdir)/src/vsocket \
-I$(top_srcdir)/src/app \
-I$(top_srcdir)/src/include
bin_PROGRAMS=vrsocket
vrsocket_SOURCES = \
Options.h\
Options.cpp\
XMLParser.h \
XMLParser.cpp \
main.cpp
vrsocket_CXXFLAGS = -I/home/dlapsley/build/xerces-c-src_2_5_0/include  \
						  -I. \
						  -DLINUX \
						  -D_REENTRANT \
						  -DAPP_NO_THREADS \
						  -DXML_USE_NO_THREADS -O3
vrsocket_LDFLAGS = -L../dutils -L../vsocket  \
						-L/home/dlapsley/build/xerces-c-src_2_5_0/lib  -O3
vrsocket_LDADD = -lvsocket -ldutils -lpthread -lxerces-c -lc 
