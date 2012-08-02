#define _XOPEN_SOURCE 500

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

#include <fcntl.h>
#include <unistd.h>


int main(int argc, char** argv)
{
    int s;
    struct sockaddr_un remote;
    char* sock_path = 0;
   
    // Get socket address
    if( argc != 2 )
        return 1;
    sock_path = argv[1];

    // Create socket
    if ((s = socket(AF_UNIX, SOCK_STREAM, 0)) == -1) {
        perror("socket");
        exit(1);
    }
    // Connect
    remote.sun_family = AF_UNIX;
    strcpy(remote.sun_path, sock_path); // FIXME: Buffer overrun!
    int len = strlen(remote.sun_path) + sizeof(remote.sun_family);
    if (connect(s, (struct sockaddr *)&remote, len) == -1) {
        perror("connect");
        exit(1);
    }

    // Replace stdin & stdout with socket
    close( STDIN_FILENO  );
    close( STDOUT_FILENO );

    if( dup( s ) != 0 )
        exit(100);
    if( dup( s ) != 1 )
        exit(101);
    execlp( "wish", "wish", 0 );
    return 1;
}
