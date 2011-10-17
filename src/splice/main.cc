/*
 * Created by David Lapsley on Mon Jun 6 2011.
 *
 * Copyright 2011 MIT Haystack Observatory 
 *  
 * This file is part of mark6.
 *
 * mark6 is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * mark6 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with mark6.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

// C includes.
#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#include <linux/falloc.h>
// #include <sys/select.h>
// #include <sys/resource.h>
#include <sys/time.h>
// #include <sys/wait.h>
// #include <signal.h>
// #include <sched.h>
#include <unistd.h>

#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/types.h>
#include <sys/socket.h>

// C++ includes.
#include <iostream>
#include <iomanip>
#include <string>

// Framework includes.
#include <boost/foreach.hpp>
#include <boost/program_options.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/algorithm/string.hpp> 

// Local includes.
#include <mark6.h>
#include <logger.h>

// Namespaces.
namespace po = boost::program_options;
using namespace std; // Clean up long lines.

// sysctl -w net.core.rmem_max=<value>
// sysctl -w net.core.rmem_default=<value>
// int n = 1024 * 1024;
// if (setsockopt(socket, SOL_SOCKET, SO_RCVBUF, &n, sizeof(n)) == -1) {
  // deal with failure, or ignore if you can live with the default size
//  }

int pipefd[2];

ssize_t do_recvfile(int out_fd, int in_fd, off_t offset, size_t count) {
  ssize_t bytes, bytes_sent, bytes_in_pipe;
  size_t total_bytes_sent = 0;

  cout << offset << endl;
  cout << out_fd << ":" << in_fd << ":" << offset << ":" << count << endl;

  // Splice the data from in_fd into the pipe
  while (total_bytes_sent < count) {
    if ((bytes_sent = splice(in_fd, NULL, pipefd[1], NULL,
			     count - total_bytes_sent,
			     SPLICE_F_MOVE)) <= 0) {
      if (errno == EINTR || errno == EAGAIN) {
	// Interrupted system call/try again
	// Just skip to the top of the loop and try again
	continue;
      }
      perror("splice from");
      exit(1);
    }

    // Splice the data from the pipe into out_fd
    bytes_in_pipe = bytes_sent;
    while (bytes_in_pipe > 0) {
      if ((bytes = splice(pipefd[0], NULL, out_fd, &offset, bytes_in_pipe,
			  SPLICE_F_MORE | SPLICE_F_MOVE)) <= 0) {
	if (errno == EINTR || errno == EAGAIN) {
	  // Interrupted system call/try again
	  // Just skip to the top of the loop and try again
	  continue;
	}
	perror("splice to");
	exit(-1);
      }
      bytes_in_pipe -= bytes;
    }
    total_bytes_sent += bytes_sent;
  }
  return total_bytes_sent;
}


int main(int argc, char** argv) {
  if ( pipe(pipefd) < 0 ) {
    perror("pipe");
    exit(1);
  }

  off_t offset = 0;
  // const int len = 8224;
  const int len = 16384;
  int file_fd;
  int socket_fd;
  // Setup socket.
  const int BUFLEN(8224);
  const int PORT(4201);
  const off_t FILE_LENGTH(20000000000);
  struct sockaddr_in local_addr, remote_addr;
  int local_addr_len(sizeof(remote_addr));
  if ((socket_fd=socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) == -1) {
    cout << "Unable to create socket: " << strerror(errno) << endl;
    exit(1);
  }

  memset((char*)&local_addr, 9, sizeof(local_addr));
  local_addr.sin_family = AF_INET;
  local_addr.sin_port = htons(PORT);
  local_addr.sin_addr.s_addr = htonl(INADDR_ANY);
  
  if (bind(socket_fd, (const sockaddr*)&local_addr, sizeof(local_addr)) < 0) {
    cout << "Unable to bind socket: " << strerror(errno) << endl;
    exit(1);
  }

  file_fd = ::open("/mnt/disk0/disk0.m6", O_WRONLY | O_CREAT, S_IRWXU);
  if (file_fd < 0) {
    cout << "Unable to create file: " << strerror(errno) << endl;
    exit(1);
  }

#if 0
  int myerrno = fallocate(file_fd, FALLOC_FL_KEEP_SIZE, 0, FILE_LENGTH);
  if (myerrno != 0) {
    cout << "Fallocate failed: " << strerror(errno) << endl;
    exit(1);
  }

  if (::lseek(file_fd, 0, SEEK_SET) < 0) {
    cout << "Seek failed: " << strerror(errno) << endl;
    exit(1);
  }
#endif

  while (true) {
    // Send 'len' bytes from 'socket_fd' to 'offset' in 'file_fd'
    do_recvfile(file_fd, socket_fd, offset, len);
    offset += len;
  }

}
