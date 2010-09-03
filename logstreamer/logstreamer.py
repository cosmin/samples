# Based on pytailer from http://code.google.com/p/pytailer/

import os
import sys
import socket
import time

line_terminators = ('\r\n', '\n', '\r')

def read(file, read_size=None):
    if read_size:
        read_str = file.read(read_size)
    else:
        read_str = file.read()
    return len(read_str), read_str

def find_last_line(file):
        pos = end_pos = file.tell()
        read_size = 300
        if pos > read_size:
            pos -= read_size
        else:
            pos = 0
            read_size = end_pos

        file.seek(pos)

        bytes_read, read_str = read(file, read_size)
        if bytes_read and read_str[-1] in line_terminators:
            # The last charachter is a line terminator, don't count this one
            bytes_read -= 1
            if read_str[-2:] == '\r\n' and '\r\n' in line_terminators:
                # found crlf
                bytes_read -= 1

        while bytes_read > 0:
            # Scan backward, counting the newlines in this bufferfull
            i = bytes_read - 1
            while i >= 0:
                if read_str[i] in line_terminators:
                    file.seek(pos + i + 1)
                    return file.tell()
                i -= 1

            if pos == 0 or pos - read_size < 0:
                # Not enought lines in the buffer, send the whole file
                file.seek(0)
                return None

            pos -= read_size
            file.seek(pos)

            bytes_read, read_str = read(file, read_size)

        return None

def main(filename, remote_ip, remote_port):
    client_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    client_socket.connect((remote_ip, remote_port))
    f = open(filename, 'r')
    find_last_line(f)
    try:
        while True:
            where = f.tell()
            line = f.readline()
            if line:
                client_socket.sendall(line)
            else:
                f.seek(where)
                time.sleep(0.01)
    finally:
        f.close()
        client_socket.close()

if __name__ == "__main__":
    filename = sys.argv[1]
    remote_ip = sys.argv[2]
    remote_port = int(sys.argv[3])
    main(filename, remote_ip, remote_port)

