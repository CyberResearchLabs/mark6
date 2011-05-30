/*
 *  Vsocket.h
 *  vrtp
 *
 *  Created by David Lapsley on Thu Mar 18 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

/*
 * Copyright (c) 2003 MIT, Haystack Observatory
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction,including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 *
 */

#ifndef _VSOCKET_H_
#define _VSOCKET_H_
#include <Object.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/select.h>

/* specialtype for VLBI STREAM */
#define SOCK_VSTREAM    6

typedef struct {
    unsigned char hdra;
    unsigned char hdrb;
    unsigned int length;
    unsigned int seq_num;
    unsigned char flags;
} vsocket_hdr_t;

typedef enum {
    INIT=0x1, 
    BOUND=0x2, 
    LISTENING=0x3, 
    /*    ACCEPTED=0x4, */ 
    CONNECTED=0x4,
    SHUTDOWN=0x5,
    DESTROYED=0x6
} vsocket_state_t;

#ifdef ATM_FORUM
typedef struct {
    uint8_t OAM;            /* IMA Version 1.0/1.1 */
    uint8_t CellIDLinkID;   /* IMA OAM Cell Type (0: filler cell, 1: ICP cell,
        * 2: data cell) */
    uint8_t IMAFrameSeqNumber;  /* IMA frame sequence number (0 to 255) */
    uint8_t ICPCellOffset;  /* 0..M-1 position of ICP cell within the IMA 
        * frame.
        */
    uint8_t LinkStuffIndication;
    uint8_t StatusAndCtrlChangeIndication;
    uint8_t IMAID;          /* indicates id of connection */
    uint8_t GroupStatusAndControl;
    /* group status and conrol */
} filler_cell_t;
#endif /* ATM_FORUM */


typedef struct {
    int ref_sock;          /* Used as a placeholder..
    */
    int accept_sock;        /* Used by client/server to accept incoming 
        * connections.
        */
    int ctrl_sock;          /* Used to transfer control information. Also
        * used to interface to select() function.
        */
    int* connsock_list;     /* Used by both client and server once connection 
        * established 
        */
    unsigned char* rcvbuf;
    unsigned char* sndbuf;
    unsigned int num_streams;
    int type;
    unsigned int block_size;
    unsigned int hdr_size;
    unsigned int pkt_size;
    unsigned int seq_num;
    vsocket_state_t state;
} vsocket_t;

class Vsocket: public Object {
private:
    static Vsocket* _instance;
protected:
    Vsocket();
public:
    static Vsocket* Instance();
    const Vsocket& operator<<(const string& ss) const;
};

// This it the entry point into the VSOCKET module.
extern "C" {
    int vsocket_accept(int s, struct sockaddr *addr, socklen_t *addrlen);
    int vsocket_bind(int s, const struct sockaddr *name, int namelen);
    int vsocket_connect(int s, const struct sockaddr *name, int namelen);
    extern int vsocket_getsockopt(int s, int level, int optname, 
                                  void *optval, int *optlen);
    int vsocket_listen(int s, int backlog); 
    ssize_t vsocket_recv(int s, void *buf, size_t len, int flags);
    ssize_t vsocket_send(int s, const void *msg, size_t len, int flags);
    int vsocket_setsockopt(int s, int level, int optname, 
                           const void *optval,
                           int optlen);
    int vsocket_shutdown(int s, int how);
    int vsocket(int domain, int type, int protocol);
    int vsocket_select(int n, fd_set *readfds, fd_set *writefds, fd_set *exceptfds,
                       struct timeval *timeout);
    
#ifdef FINAL
    int vgetpeername(int, struct sockaddr *, socklen_t *);
    int vgetsockname(int, struct sockaddr *, socklen_t *);
    ssize_t vrecvmsg(int, struct msghdr *, int);
    ssize_t vrecvfrom(int, void *, size_t, int, struct sockaddr *, socklen_t *);
    ssize_t vsendto(int, const void *,
                    size_t, int, const struct sockaddr *, socklen_t);
    ssize_t vsendmsg(int, const struct msghdr *, int);
    int vsocketpair(int, int, int, int *);
#endif /* FINAL */
}  // End of extern "C".

#ifdef DEBUG
void* vsocket_test(void* parm);
#endif
#endif /* _VSOCKET_H_ */
