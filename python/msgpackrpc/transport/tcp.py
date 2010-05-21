import socket

class TCPTransport(object):
    def __init__(self, host, port):
        self._socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self._socket.connect((host, port))

    def try_close(self):
        pass

    def try_recv(self):
        return self._socket.recv(4096)

    def try_send(self, data):
        self._socket.sendall(data)
