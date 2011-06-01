# Source root.

MARK6_ROOT	=	/home/dlapsley/mark6
VERSION		=	0.1.0
DISTRIB		=	$(MARK6_ROOT)/mark6-$(VERSION).tar.gz

# Useful command macros.
CP		=	/bin/cp
RM		=	/bin/rm
ECHO		=	/bin/echo
EXECUTABLE	=	mark6

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
		    -I$(MARK6_ROOT)/src \
		    -I/usr/include
		    # MAC OS X -I/opt/local/include
# MAC OS  X LDFLAGS		=   -L/opt/local/lib
LDFLAGS		=   -L/usr/lib
DEFINES		=   -DLINUX
