#include <iostream>
#include <exception>
#include <TCPSocket.h>
#include <UDPSocket.h>
#include <Options.h>
#include <SocketBuffer.h>
#include <Client.h>
#include <Server.h>
#include <RTP.h>

using namespace std;

void run_test(Options o);

int 
main (int argc, char * const argv[]) {
    // insert code here...
    std::cout << "Hello, World!\n";
    u_int8_t data[]= {
         0x3
    };
    cout << "Data[0] = " << (int)data[0] << endl;
    RTPPacket p(1024, 
                2,
                0,
                1,
                0,
                1,
                3,
                100,
                0,
                1,
                NULL,
                data,
                1);
    p.dump(cout);
    
    try {
        Options o(argc, argv);
        cout << o.get_local_ip() << endl;
        cout << o.get_remote_ip() << endl;
        switch (o.get_mode()) {
            case CLIENT:
                {
                    Client c;
                    c.run(o);        
                }
                break;
            case SERVER:
                {
                    Server s;
                    s.run(o);                
                }
                break;
            case TEST:
                break;
        }
        string my_ip;
        // Socket s(10);
        // s.bind(my_ip, 10);
    } catch (SocketException e) {
        cout << "Bugger" << endl;
        cout << e.what() << endl;
    }
    return 0;
}


