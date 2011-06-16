# Source root.

VERSION		=	0.1.0
DISTRIB		=	$(MARK6_ROOT)/mark6-$(VERSION).tar.gz

# Useful command macros.
CP		=	/bin/cp
RM		=	/bin/rm
ECHO		=	/bin/echo
DIM		=	dim6
DOM		=	dom6

# Ubuntu pre-requisite packages.
PREQ		=	liblog4cxx10-dev \
			libboost1.40-all-dev \
			build-essential \
			git-core

# Compiler macros.
CC		=   g++
CFLAGS		=   -c \
		    -Wall \
		    -O4
INCLUDES	=   -I$(MARK6_ROOT)/include \
		    -I$(MARK6_ROOT)/src/include \
		    -I/usr/include \
		    -I/opt/local/include \
		    -I.

LDFLAGS		=   -L/usr/lib
LDFLAGS		=   -L/opt/local/lib
DEFINES		=   -DLINUX
