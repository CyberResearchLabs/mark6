DATE 2/29 

Added Logger class. Singleton for implementing logging subsystem.

Added logger functions to Object class to simplify calling interface.

Added Thread class (subclass of Object). Thread class encapsulated pthread
functionality.

Made Client and Server both subclasses of Thread.

Added Test class. Initially for testing Thread class. This is a generic
class that is instantiated and run from the main program with the "-t" 
option is used. Useful for testing.

DATE 2/29 22:32

Added RTPStream class and subclassses:
    RTPIStream
    RTPOStream
    RTPIfStream
    RTPOfStream
    Mark5IfStream
    Mark5OfStream
    TestIfStream
    TestOfStream
    
This encapsulates the RTP data source/sink. The source can either be from/to
file (the "If"stream classes) or some other source (e.g. socket - not
yet implemented).

Added logging functions to Object class. Subclassed many classes from this
in order to implement the simplified logging interface.

DATE 3/2

Added RTPIStream, RTPOStream classes and implemented them.

Restructured Client and Server to make it easier to add functionality to them.
Removed all functionality from thread_func() function. Now this function simply
acts as the thread execution entry point. All of the functionality is implemented
in another member function (e.g. bit_sink, bit_source, vsi_source, vsi_sink, etc.).

Tested succesfully.

Now to port to Linux :-)

Done!

Ported back...

3/6/2004 - Ported UDT to Linux

Need to turn Client/Server into "Strategy Class".
Need common RTPIStream, RTPOStream class levels.
Client and Server will have pointers to RTPIStream and RTPOStream...
They will invoke operations on these pointers.
Much more general approach...


Implement RUDP
Clean up loggin subsystem 
    done
Add per method debug loggin to:
    RTPStream subsystem...
    
Add exceptions
Add documentation

3/21/04

Have to be careful with set_req_size() on SocketBuffers. The caller needs
to set them, they are never set by the transmisison functions themselves.
