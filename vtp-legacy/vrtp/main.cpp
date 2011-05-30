#include <iostream>
#include <exception>
#include <TCPSocket.h>
#include <UDPSocket.h>
#include <Options.h>
#include <SocketBuffer.h>
#include <Client.h>
#include <Server.h>
#include <Test.h>
#include <RTP.h>
#include <RTPSession.h>
#include <Logsystem.h>
#include <Logger.h>

using namespace std;

void run_test(Options o);

int 
main (int argc, char * const argv[]) {
    try {
        // Parse cmd line args and store in o.
        Options o(argc, argv);
        
        // Init debugging.
        switch (o.get_debug_level()) {
            case 3:
                Logsystem::debug_logger()->enable_logging();
            case 2:
                Logsystem::info_logger()->enable_logging();
            case 1:
                Logsystem::warning_logger()->enable_logging();
            case 0:
                Logsystem::error_logger()->enable_logging();
        }        

        // Run...
        RTPSession r;
        switch (o.get_mode()) {
            case CLIENT:
                {
                    string name("Client");
                    Client c(name);
                    c.run(o);
                    c.join();
                    //r.AddClient(c);
                    //r.run(o);
                    //r.join();
                }
                break;
            case SERVER:
                {
                    string name("Server");
                    Server s(name);
                    s.run(o);
                    s.join();
                    //r.AddServer(s);
                    //r.run(o);
                    //r.join();
                }
                break;
            case TEST:
                {
                    Test t;
                    t.run(o);
                }
                break;
        }
        string my_ip;
    } catch (SocketException e) {
        cout << "Bugger" << endl;
        cout << e.what() << endl;
    }
    return 0;
}


