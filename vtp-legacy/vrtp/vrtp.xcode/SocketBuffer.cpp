
#include <SocketBuffer.h>

SocketBuffer::SocketBuffer(int s) {
    _max_size=s;
    _buf=new char(_max_size);
    _size=0;
}

SocketBuffer::~SocketBuffer() {
    delete(_buf);
}

int SocketBuffer::get_size() {
    return(_size);
}

int SocketBuffer::get_max_size() {
    return(_max_size);
}

void SocketBuffer::set_size(int s) {
    _size=s;
}

SocketBuffer::operator char*() {
    if (_max_size>0) {
        return(_buf);
    } else {
        return(0);
    }
}
