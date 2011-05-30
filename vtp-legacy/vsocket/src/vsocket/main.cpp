#include <Logger.h>
#include <Logsystem.h>
#include <TCPSocket.h>
#include <UDPSocket.h>
#include <PSocket.h>
#include <sys/types.h>
#include <unistd.h>

int parent();
int child();

int main(int argc, char** argv) 
{
	Logsystem::debug_logger()->enable_logging();

	// TCPSocket test.
	bool ret=TCPSocket::test();
	cout << "TCPSocket: test ";
	if (ret==false)
		cout << "failed";
	else 
		cout << "passed";
	cout << endl;

	usleep(1000000);

	// UDPSocket test.
	ret=UDPSocket::test();
	cout << "UDPSocket: test ";
	if (ret==false)
		cout << "failed";
	else 
		cout << "passed";
	cout << endl;

	// PSocket test.
	ret=PSocket::test();
	cout << "PSocket: test ";
	if (ret==false)
		cout << "failed";
	else 
		cout << "passed";
	cout << endl;
	return(1);
}
