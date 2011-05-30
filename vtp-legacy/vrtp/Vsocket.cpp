

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
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <stdio.h>
#include <sys/time.h>
#include <Vsocket.h>

Vsocket* Vsocket::_instance = 0;

Vsocket::Vsocket() {
    
}  // End logger().

Vsocket* Vsocket::Instance()
{
    if (_instance==0) {
        _instance=new Vsocket;
    }
    return(_instance);
}  // End Instance().

int vsocket_init(vsocket_t* vsock)
{
    int ret=0;
    Vsocket::Instance()->debug_logger() << "vsocket_init()\n";
    Vsocket::Instance()->debug_logger() << "-vsocket_init()\n";
    return(ret);
}

int vsocket_destroy(vsocket_t* vsock)
{
    int ret=0;
    Vsocket::Instance()->debug_logger() << "vsocket_destroy()\n";
    Vsocket::Instance()->debug_logger() << "-vsocket_destroy()\n";
    return(ret);
}

int vsocket_accept(int s, struct sockaddr *addr, socklen_t *addrlen)
{
    int ret=0;
    Vsocket::Instance()->debug_logger() << "vsocket_accept()\n";
    Vsocket::Instance()->debug_logger() << "-vsocket_accept()\n";
    return(ret);
}

int vsocket_bind(int s, const struct sockaddr *name, int namelen)
{
    int ret=0;
    Vsocket::Instance()->debug_logger() << "vsocket_bind()\n";
    Vsocket::Instance()->debug_logger() << "-vsocket_bind()\n";
    return(ret);
}

int vsocket_connect(int s, const struct sockaddr *name, int namelen)
{
    int ret=0;
    Vsocket::Instance()->debug_logger() << "vsocket_connect()\n";
    Vsocket::Instance()->debug_logger() << "-vsocket_connect()\n";
    return(ret);
}

int vsocket_getsockopt(int s, int level, int optname, void *optval, int *optlen)
{
    int ret=0;
    Vsocket::Instance()->debug_logger() << "vsocket_getsockopt()\n";
    Vsocket::Instance()->debug_logger() << "-vsocket_getsockopt()\n";
    return(ret);
}

int vsocket_listen(int s, int backlog)
{
    int ret=0;
    Vsocket::Instance()->debug_logger() << "vsocket_listen()\n";
    Vsocket::Instance()->debug_logger() << "-vsocket_listen()\n";
    return(ret);
}

ssize_t vsocket_recv(int s, void *buf, size_t len, int flags)
{
    int ret=0;
    Vsocket::Instance()->debug_logger() << "vsocket_recv()\n";
    Vsocket::Instance()->debug_logger() << "-vsocket_recv()\n";
    return(ret);
}

ssize_t vsocket_send(int s, const void *msg, size_t len, int flags)
{
    int ret=0;
    Vsocket::Instance()->debug_logger() << "vsocket_send()\n";
    Vsocket::Instance()->debug_logger() << "-vsocket_send()\n";
    return(ret);
}

int vsocket_setsockopt(int s, int level, int optname, const void *optval,
                       int optlen)
{
    int ret=0;
    Vsocket::Instance()->debug_logger() << "vsocket_setsockopt()\n";
    Vsocket::Instance()->debug_logger() << "-vsocket_setsockopt()\n";
    return(ret);
}

int vsocket_shutdown(int s, int how)
{
    int ret=0;
    Vsocket::Instance()->debug_logger() << "vsocket_shutdown()\n";
    Vsocket::Instance()->debug_logger() << "-vsocket_shutdown()\n";
    return(ret);
}

int vsocket(int domain, int type, int protocol)
{
    int ret=0;
    Vsocket::Instance()->debug_logger() << "vsocket_vsocket()\n";
    Vsocket::Instance()->debug_logger() << "-vsocket_vsocket()\n";
    return(ret);
}

int vsocket_select(int n, fd_set *readfds, fd_set *writefds, fd_set *exceptfds,
                   struct timeval *timeout)
{
    int ret=0;
    Vsocket::Instance()->debug_logger() << "vsocket_select()\n";
    Vsocket::Instance()->debug_logger() << "-vsocket_select()\n";
    return(ret);
}

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

#ifdef DEBUG

static int vsocket_client_test(vsocket_options_t *options)
{
    g_message("vsocket_client_test()");
    
    static vsocket_hdr_t hdr = {
        'v',
        'h',
        0,
        0
    };
    hdr.hdra='u';
    int ret=0;
    
    /** Create local address for connecting */
    struct sockaddr_in remote_addr;
    remote_addr.sin_family = AF_INET;
    remote_addr.sin_port = options->remote_address.sin_port;
    remote_addr.sin_addr.s_addr = options->remote_address.sin_addr.s_addr;
    
    int ref_sock=vsocket(AF_INET, SOCK_VSTREAM, 0);
    
    int ctrl_sock=vsocket_connect(ref_sock, 
                                  (const struct sockaddr*)&remote_addr, 
                                  sizeof(remote_addr));
    if (ctrl_sock<0) {
        g_message("    client: error connecting=\"%s\"", strerror(errno));
    }
    
    const unsigned int BUF_SIZE=8192;
    unsigned char buf[BUF_SIZE];
    
    /** Receive data and store in file */
    FILE* out=fopen("client.dat", "w+");
    g_assert(out);
    ssize_t read_bytes=0;
    while ((read_bytes=vsocket_recv(ref_sock, buf, BUF_SIZE, MSG_WAITALL))>0) {
        /* Store data */
        g_message("    client: received %d bytes", read_bytes);
        ssize_t written_bytes=fwrite((void*)buf, sizeof(unsigned char), read_bytes, 
                                     out);
        g_message("    client: wrote %d bytes to file", written_bytes);
        g_assert(written_bytes==read_bytes);
        g_assert(read_bytes==BUF_SIZE);
    }
    fflush(out);
    fclose(out);
    
    ret=vsocket_shutdown(ref_sock, 2);
    
    
#if 0
    /* Miscellaneous */
    ret=vsocket_getsockopt(ref_sock, 1, 2, NULL, NULL);
    ret=vsocket_setsockopt(0, 1, 2, NULL, 0);
    ret=vsocket_shutdown(0, 1);
    /** This does not need to be done by the API user */
    vsocket_t* vsock=(vsocket_t*)malloc(sizeof(vsocket_t));
    ret=vsocket_init(vsock);
#endif
    
    g_message("    returning...");
    return(1);
}

static int vsocket_server_test(vsocket_options_t *options)
{
    g_message("vsocket_server_test()");
    
    static vsocket_hdr_t hdr = {
        'v',
        'h',
        0,
        0
    };
    hdr.hdra='u';
    int ret=0;
    
    /** Create local address for binding */
    struct sockaddr_in local_addr;
    local_addr.sin_family = AF_INET;
    local_addr.sin_port = htons(49999);
    local_addr.sin_addr.s_addr = htonl(INADDR_ANY);
    
    /* Simple socket test */
    int mysock=socket(AF_INET, SOCK_STREAM, 0);
    int flag=1;
    ret=setsockopt(mysock, SOL_SOCKET, SO_REUSEADDR, &flag, sizeof(flag));
    g_message("    server: mysock=%d", mysock);
    ret=bind(mysock, (const struct sockaddr*)&local_addr, sizeof(local_addr));
    g_message("    server: ret=%d", ret);
    g_message("    server: errstring=%s", strerror(errno));
    g_message("    server: sizeof(local_addr)=%lu", sizeof(local_addr));
    
    /* Set the correct port for vsocket test */
    local_addr.sin_port = options->local_address.sin_port;
    
    /* Create vsocket */
    int ref_sock=vsocket(AF_INET, SOCK_VSTREAM, 0);
    g_message("    server: created vsocket. ref_sock=%d", ref_sock);
    
    if (ref_sock<=0) {
        g_message("    server: error creating socket. message=\"%s\"", strerror(errno));
    }
    
    /* Set socket options */
    flag=1;
    ret=vsocket_setsockopt(ref_sock, SOL_SOCKET, SO_REUSEADDR, &flag, 
                           sizeof(flag));
    if (ret<0)
        g_message("    server: error setting options=\"%s\"", strerror(errno));
    else 
        g_message("    server: setsockopt return=%d", ret);
    
    ret=vsocket_bind(ref_sock, (struct sockaddr*)&local_addr, sizeof(local_addr));
    if (ret<0) {
        g_message("    server: error binding=\"%s\"", strerror(errno));
    }
    struct sockaddr_in addr;
    int addrlen;
    /** Prepare sockets for accepting connections */
    ret=vsocket_listen(ref_sock, 10); 
    if (ret<0) {
        g_message("    server: error listening=\"%s\"", strerror(errno));
    }
    /** Accept connections */
    ret=vsocket_accept(ref_sock, (struct sockaddr*)&addr, &addrlen);
    if (ret<0) {
        g_message("    server: error accepting=\"%s\"", strerror(errno));
    }
    
    /** Transmit data from file */
    FILE* in=fopen("server.dat", "r");
    g_assert(in);
    ssize_t read_bytes=0;
    const unsigned int BUF_SIZE=8192;
    unsigned char buf[BUF_SIZE];
    while ((read_bytes=fread(buf, sizeof(unsigned char), BUF_SIZE, in))>0) {
        /* Transmit data */
        ssize_t ssize_ret=0;
        ssize_ret=vsocket_send(ref_sock, buf, read_bytes, 0);
        g_message("    server: sent %d bytes", ssize_ret);
    }
    ret=vsocket_shutdown(ref_sock, 2);
    
#if 0   
    /* Miscellaneous */
    /** Test allocation of sockets */
    int sock_array[5];
    sock_array[0]=vsocket(AF_INET, SOCK_VSTREAM, 0);
    sock_array[1]=vsocket(AF_INET, SOCK_STREAM, 0);
    sock_array[2]=vsocket(AF_INET, SOCK_DGRAM, 0);
    sock_array[3]=vsocket(AF_INET, SOCK_VSTREAM, 0);
    sock_array[4]=vsocket(AF_INET, SOCK_DGRAM, 0);
    for (i=0; i<5; i++) {
        g_message("    sock_array[%d]=%d", i, sock_array[i]);
    }
    for (i=0; i<5; i++) {
        g_assert(vsocket_destroy(sock_array[i]));
    }
    
    /** This does not need to be done by the API user */
    vsocket_t* vsock=(vsocket_t*)malloc(sizeof(vsocket_t));
    ret=vsocket_init(vsock);
    g_assert(ret>=0);
    ret=vsocket_getsockopt(ref_sock, 1, 2, NULL, NULL);
    g_assert(ret>=0);
    ret=vsocket_setsockopt(0, 1, 2, NULL, 0);
    g_assert(ret>=0);
#endif
    
    g_message("    returning...");  
    return(1);
}

void* vsocket_test(void* parm)
{
    g_message("********************");
    g_message("vsocket_test()");
    
    int ret=0;
    
    /* Initialize system */
    /** Vsocket table must be initialized before anything else is done */
    /** Quick way out of race condition XXXX */
    ret=vsocket_table_init(8, 100, 16384, 16384, 1024);
    
    vsocket_options_t* opts=(vsocket_options_t*)parm;
    switch (opts->mode) {
        case CLIENT:
            vsocket_client_test(opts);
            break;
        case SERVER:
            vsocket_server_test(opts);
            break;
        case TEST:
            break;
    }
    
#if 0
    pid_t child_pid;
    switch (child_pid=fork()) {
        case 0:
            /* This is the child process
            */
            vsocket_server_test();
            break;
        case -1:
            g_error("    fork error=\"%s\"", strerror(errno));
            break;
        default:
            /* This is the parent process
            * child pid is stored in child_pid
            */
            vsocket_client_test();
            break;
    }
#endif
    
    /* Shut down */
    ret=vsocket_table_destroy();
    g_assert(ret>=0);
    
    g_message("    returning....");
    return(0);
}
#endif
