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

def serve_background(server, daemon=False):
    import threading
    t = threading.Thread(target=server.serve_forever)
    t.setDaemon(daemon)
    t.start()

def serve(daemon=False):
    """Serve echo server in background on localhost.
    This returns (server, port). port is number in integer.

    To stop, use ``server.shutdown()``
    """
    for port in xrange(9000, 10000):
        try:
            server = SocketServer.TCPServer(('localhost', port), EchoHandler)
            serve_background(server, daemon)
            return server, port
        except Exception:
            pass

if __name__ == '__main__':
    server, port = serve(False)
    print "Serving on localhost:%d\n" % (port,)
