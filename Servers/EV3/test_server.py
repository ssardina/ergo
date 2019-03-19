# This is a sampler TCP server from any machine on port 8123
# It accepts numbers n, and returns n+7

import socket
import sys

# Create a TCP/IP socket
sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

# Bind the socket to the port
server_address = ('0.0.0.0', 8123)
print ('starting up on %s port %s' % server_address)
sock.bind(server_address)

# Listen for incoming connections
sock.listen(1)

# Wait for a connection
try:
    print ('listening for a connection on port 8123')
    connection, client_address = sock.accept()
    print ('connection from %s' % str(client_address))
    # Receive the data as a seq of lines ending with \n 
    for data in connection.makefile():
        print('received {!r}'.format(data.rstrip()))
        # process data here
        out = str(int(data)+7)+'\n'
        connection.sendall(out.encode())
except:
    print('error in data received')
finally:
    print ('closing connection')
    connection.close()
