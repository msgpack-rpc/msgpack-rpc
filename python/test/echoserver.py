#!/usr/bin/env python
# coding: utf-8

"""Echo service.
This server doesn't use msgpackrpc.server.
"""

import SocketServer
from msgpack import packs, Unpacker

class EchoHandler(SocketServer.BaseRequestHandler):

    def handle(self):
        unpacker = Unpacker()

        while 1:
            data = self.request.recv(4096)
            if len(data) == 0:
                break
            unpacker.feed(data)
            for msg in unpacker:
                print msg
                assert len(msg) == 4
                assert msg[0] == 0
                assert msg[2] == "echo"
                sdata = packs((1, msg[1], None, msg[-1]))
                self.request.sendall(sdata)

def serve_background(server, daemon=True):
    import threading
    t = threading.Thread(target=server.serve_forever)
    t.setDaemon(daemon)
    t.start()

def serve(daemon=True):
    """serve echo server in background on localhost.
    return port number in integer.
    """
    for port in xrange(9000, 10000):
        try:
            server = SocketServer.TCPServer(('localhost', port), EchoHandler)
            serve_background(server, daemon)
            return port
        except Exception:
            pass

if __name__ == '__main__':
    port = serve(False)
    print "Serving on localhost:%d\n" % (port,)
