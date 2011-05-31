# Source root.
MARK6_ROOT=/opt/src/mark6
VERSION=0.1.0
DISTRIB=$(MARK6_ROOT)/mark6-$(VERSION).tar.gz

CP=/bin/cp
RM=/bin/rm
ECHO=/bin/echo
EXECUTABLE=mark6

# Ubuntu pre-requisite packages.
# PREQ="ncurses-dev libssl-dev build-essential git-core"

CC=		g++
CFLAGS=		-c \
		-Wall
INCLUDES=	-I/opt/src/mark6/include \
		-I/opt/local/include
DEFINES=	-DLINUX
LDFLAGS=	-L/opt/local/lib
