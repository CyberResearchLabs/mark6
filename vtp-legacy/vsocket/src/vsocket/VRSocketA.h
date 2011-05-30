/*
 *  VRSocketA.h
 *  vrtp
 *
 *  Created by David Lapsley on Mon Feb 23 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef VRSOCKETA_H
#define VRSOCKETA_H

#include <common.h>
#include <Socket.h>
#include <UDPSocket.h>
#include <TCPSocket.h>
#include <string>
#include <iostream>
#include <Thread.h>
#include <UDPSocket.h>

/** VRSocketA class
  * @doc This class encapsulates a UDP stream and implements the VLBI Real-time
  * protocol on top of it.
  */

struct TSpec {
	unsigned int _peak_rate;
};

#ifdef LINUX

/** Definition of packet types */
typedef enum { 
	PT_DATA=0x1, 
	PT_ACK=0x2, 
	PT_FIN=0x3 
} RTPPacketType;

/** RTPHdr struct
  * @doc This class provides the format for the RTP header.
  */
// RTP data packet header format.
//    0                   1                   2                   3
//  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
// |V=2|P|X|  CC   |M|     PT      |       sequence number         |
// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
// |                           timestamp                           |
// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
// |           synchronization source (SSRC) identifier            |
// +=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
// |            contributing source (CSRC) identifiers             |
// |                             ....                              |
// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
struct RTPHdr {
        uint8_t _cc:4;        	// CSRC count 
        uint8_t _x:1;         	// header extension flag 
        uint8_t _p:1;         	// padding flag 
        uint8_t _v:2;   		// protocol version 
        uint8_t _pt:7;        	// payload type
        uint8_t _m:1;         	// marker bit
        uint16_t _sn;      		// sequence number
        uint32_t _ts;           // timestamp
        uint32_t _ssrc;         // synchronization source
        uint32_t _csrc;         // optional CSRC list
};      

// Output from TCPdump 
// 21:40:39.177421 voyager.32813 > argus.egaedata: udp 1356 (DF) 
// (ttl 64, id 41284, len 1384)
// 4500 0568 a144 4000 4011 0d61 c0a8 02c7
// c0a8 02c8 802d bf68 0554 e4fa 8001 2b78
//								 ^  ^  ^
//					v/p/x/cc   --/  |  |
//					m/pt     -------/  |
//					sn	     ----------/
// 0000 2b78 0000 0003 4c2e 1000 1c00 0000
// --------- --------- -------------------
//     ^         ^              ^
//     |         |              |
// ts--/         |              |
// ssrc ---------/              |
// data ------------------------/

#else
typedef enum { 
	PT_DATA=0x1, 
	PT_ACK=0x2, 
	PT_FIN=0x3 
} RTPPacketType;

/** RTPHdr struct
  * @doc This class provides the format for the RTP header.
  */ 
struct RTPHdr {
        uint8_t _v:2;   		// protocol version 
        uint8_t _p:1;         	// padding flag 
        uint8_t _x:1;         	// header extension flag 
        uint8_t _cc:4;        	// CSRC count 
        uint8_t _m:1;         	// marker bit
        uint8_t _pt:7;        	// payload type
        uint16_t _sn;      		// sequence number
        uint32_t _ts;           // timestamp
        uint32_t _ssrc;         // synchronization source
        uint32_t _csrc;         // optional CSRC list
};      
#endif

/** VRSocketA class
  * This class encapsulates a socket processing thread. It spawns a new thread
  * of execution for asynchronously processing send and receive requests.
  * This class is responsible for reading from/writing to a socket. 
  * Once "thread_create()" is called, the VRSocketA object will spawn a thread, 
  * which in turn will execute the thread_func() method. This method continually 
  * reads/writes to a socket. Data is read from/written to the send/recv buffers.
  */
class VRSocketA: public Thread
{

	/** Definition of packet types */
	typedef enum { 
		PS_INIT, PS_SELECT, PS_TX_DATA, PS_RX_DATA, PS_FIN, PS_APP_FIN
	} ProcStateType;

	/** State variable.
	  * Maintains the state for the processing loop (either recv_proc()
	  * or send_proc()).
	  */
	ProcStateType _state;
	/** Data transmission.
	  * Used for receiving/transmitting data.
	  */
	UDPSocket _udp_sock;
	/** Control transmission.
	  * All control information is transmitted in one direction along this 
	  * socket.
	  */
	TCPSocket _tcp_sock;
	/** Accept sock.
	  * Used to accept incoming connections.
	  */
	TCPSocket _tcp_accept_sock;
	/** SockProc mode.
	  * This determines which direction data is flowing: out of the object
	  * (send) or into the object(recv). For this implementation, the mode
	  * is fixed for the duration of the object's life. In the future, it
	  * may be possible to dynamically change it. In any case, a full
	  * duplex implementation is the ultimate aim.
	  */
	int _mode;
	/** MTU size.
	  * MTU size for data transmission, receptions.
	  */
	int _mtu;
	/** Thread callback.
	  * This function is called when the base class method "thread_create()" is
	  * called. This results in thread_func() being executed in it's own
	  * thread. Depending on the mode, either recv_proc() or send_proc() will
	  * be called by thread_func(). This means that recv_proc() or send_proc()
	  * will be running in their own thread of execution.
	  */
	void* thread_func();
	/** Processing function.
	  * This function does all of the processing when the object is in recv
	  * mode.
	  */
	void recv_proc();
	/** Processing function.
	  * This function does all of the processing when the object is in send
	  * mode.
	  */
	void send_proc();
	/** @doc Used for storing buffers for transmission. */
	SocketBufferCtrlBlkList _sockbufctrlblk_list;
	/** Mutex.
	  * Controls multi-threaded access to send data members.
	  */
	Mutex _send_mutex;
	/** Traffic Parameter.
	  * Controls traffic parameters.
	  */
	TSpec _tspec;
	/** Address parameter.
	  * UDP port for TCP socket.
	  */
	int _udp_port;
	/** Address parameter.
	  * TCP port for TCP socket.
	  */
	int _tcp_port;
	/** Address parameter.
	  * Remote IP address.
	  */
	string _remote_ip;
	/** Test parameter.
	  * Whether or not to generate tvg test data.
	  */
	bool _tvg;
	/** Test parameter.
	  * How  much test data to generate (in bytes).
	  */
	int _tvg_sz;
public:
	/** Mode constant. */
	static const int receiver;
	/** Mode constant. */
	static const int sender;
	/** Default constructor.
	  * This calls the base constructor to create a UDP socket and
	  * initialize a thread to service it.
	  * @param mode The mode for this socket (either send or receive).
	  */
    VRSocketA(const int& mode);
	/** Constructor.
	  * This constructor creates a socket and sets the internal MTU member
	  * to be equal to the supplied parameter.
	  * @param mode The mode for this socket (either send or receive).
	  * @param m MTU for this socket.
	  * @param t Traffic specification.
	  * @param ip Remote IP address.
	  * @param uport UDP socket port.
	  * @param tport TCP socket port.
	  * @param tvg Test Vector Generator switch.
	  * @param tvg_sz Amouunt of tvg data (in bytes). Defaults to 0.
	  */
    VRSocketA(const int& mode, const int &m, const TSpec& t,
			const string& ip, const int& uport, const int& tport, 
			const bool& tvg, const int& tvg_sz);
	/** Copy constructor.
	  * Copies values of other VRSocketA into this object.
	  * @param s VRSocketA to copy.
	  */
    VRSocketA(const VRSocketA &s);
	/** Constructor.
	  * This constructor is provided three initialization parameters.
	  * @param mode The mode for this socket (either send or receive).
	  * @param s socket descriptor to encapsulate.
	  * @param m MTU for this socket.
	  * @param pr Peak Rate(bps).
	  * @param uport UDP socket port.
	  * @param tport TCP socket port.
	  * @param tvg Test Vector Generator switch.
	  * @param tvg_sz Amouunt of tvg data (in bytes). Defaults to 0.
	  */
    VRSocketA(const int& mode, const int& s, const int& m, const TSpec& t,
			const bool& tvg, const int& tvg_sz);
	/** Destructor. */
    virtual ~VRSocketA();
	/** Assigment operator.
	  * Copies supplied socket fields into this object.
	  * @param s socket to copy.
	  */
    VRSocketA& operator=(const VRSocketA& s);
    /** Socket API method.
      * Binds socket to supplied address.
      * @param ip IP address to bind to.
      * @param dport data port to bind to.
      * @param cport control port to bind to.
      * @return 0: on success, -1 on failure.
      */
    void bind(const string& ip, const int& dport, const int& cport);
    /** Socket API method.
      * Attempt to establish a connection to the supplied address.
      * Note that UDP is connectionless and that in this case "connection" 
      * refers to the fact that the destination address fields are 
      * set to the parameters supplied in order that subsequent calls to
      * send() use these parameters. Otherwise, an address must be supplied
      * with each call to sendto().
      * @param ip IP address to connect to.
      * @param dport data port to bind to.
      * @param cport control port to bind to.
      * @return 0: on success, -1 on failure.
      */
    void connect(const string& ip, const int& dport, const int& cport);
	/** Socket API method.
	  * Place holder. Doesn't actually do anything. 
	  */
	void listen();
	/** Socket API method.
	  * Waits for incoming connection and then assigns _tcp_sock to it.
	  */
	void accept();
    /** Socket API method.
      * @doc shutdown connection as per standard socket library.
      * @param how method to use to shutdown socket: SHUT_RD, SHUT_WR, 
      * SHUT_RDWR.
      */    
	int shutdown(int howto);
    /** Socket API method.
	  * @doc close socket connection.      
	  * @return 0: on success, -1: on failure.
      */
    void close();
	/** Access method.
	  * Returns the size of send_buf (in number of buffers). This 
	  * provides a method for the caller to determine the progress of their 
	  * request.
	  * @return The number of buffers awaiting transmission.
	  */
	int get_send_buf_size();
	/**  Utility function.
	  *  Prints a packet header to debug stream.
	  */
	void print_pkt_hdr(const SocketBuffer& sb);
	/** Utility function.
	  * Builds an ACK packet header and stores it in the supplied SocketBuffer
	  * @param sb SocketBuffer to store ack header in.
	  * @param sn Sequence number to be stored in the acknowledgement field.
	  * @param sz Size of the header.
	  * @return 0: on success, -1 on error.
	  */
	int build_ack_hdr(SocketBuffer& sb, const int& sn, const int& sz);
	/** Utility function.
	  * Builds a DATA packet header and stores it in the supplied SocketBuffer
	  * @param sb SocketBuffer to store data header in.
	  * @param sn Sequence number to be stored in the acknowledgement field.
	  * @param sz Size of the header.
	  * @param length Length of the data field.
	  * @return 0: on success, -1 on error.
	  */
	int build_data_hdr(SocketBuffer& sb, const int& sn, const int& sz,
						const int& length);
	/** Utility function.
	  * Builds a DATA packet header and stores it in the supplied SocketBuffer
	  * @param sb array to store data header in.
	  * @param pt Payload type.
	  * @param sn Sequence Number.
	  * @param ts Timestamp.
	  * @param ssrc Syncrhonization Source.
	  * @param sz Size of the header.
	  * @return 0: on success, -1 on error.
	  */
	inline int build_data_hdr(uint8_t* sb, const uint32_t& pt, 
                            const uint16_t& sn, const uint32_t& ts,
                            const uint32_t& ssrc, const int& sz);
	/** Utility function.
	  * Builds a FIN packet header and stores it in the supplied SocketBuffer
	  * @param sb SocketBuffer to store data header in.
	  * @param sz Size of the header.
	  * @return 0: on success, -1 on error.
	  */
	int build_fin_hdr(SocketBuffer& sb, const int& sz);
	/** Utility function.
	  * Builds a FIN packet header and stores it in the supplied SocketBuffer
	  * @param sb array to store data header in.
	  * @param ssrc Syncrhonization Source.
	  * @param sz Size of the header.
	  * @return 0: on success, -1 on error.
	  */
	inline int build_fin_hdr(uint8_t* sb, const uint32_t& ssrc, const int& sz);
	/** 
	  * @doc Test method.
	  */
    static bool test();
	inline int writev(const int& fd, const SocketBufferCtrlBlk& data);
	inline int writev(const int& fd, const SocketBufferCtrlBlk& data,	
					 const SocketBufferCtrlBlk& hdr);
	inline int readv(const int& fd, const SocketBufferCtrlBlk& data);
	inline int readv(const int& fd, const SocketBufferCtrlBlk& data,	
					 const SocketBufferCtrlBlk& hdr);
};

ostream& operator<<(ostream& os, const VRSocketA& s);

#endif

