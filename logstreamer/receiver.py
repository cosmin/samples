import sys
import socket

def main(port):
    server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_socket.bind(('0.0.0.0', port))
    server_socket.listen(1)

    print "TCPServer Waiting for client on port %s" % port

    while 1:
        client_socket, address = server_socket.accept()
        print "I got a connection from ", address
        try:
            while 1:
                try:
                    data = client_socket.recv(512)
                    sys.stdout.write(data)
                    client_socket.sendall('ok\n')
                except:
                    break
        finally:
            client_socket.close()
        print "Connection closed."

if __name__ == "__main__":
    port = int(sys.argv[1])
    main(port)
