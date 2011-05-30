
OBJS	=	client.o main.o mysock.o server.o vtp.o
HDRS	=	client.h mysock.h server.h vtp.h
SRCS	=	client.c main.c mysock.c server.c vtp.c
LIBS	=	-lc
CC	=	gcc

vtp	:	${OBJS}
	${CC} -o $@ ${OBJS} ${LIBS}

dist:	
	tar cf vtp.tar ${SRCS} ${HDRS} Makefile

clean:
	rm *.o vtp

