import sys

from twisted.internet import protocol, reactor

class TCPSocket(object):
    """
    TCPSocket uses Twisted framework to actually establish the connection, and
    send/recv the data buffer.
    """
    def __init__(self, addr, loop, transport):
        self._addr = addr
        self._loop = loop
        self._transport = transport
        self._client_factory = None

    def try_connect(self):
        if self._client_factory != None:
            raise Exception("already connected")
        self._client_factory = _TwistedClientFactory(self)
        host = self._addr.get_host()
        port = self._addr.get_port()
        reactor.connectTCP(host, port, self._client_factory)

    def try_send(self, buf):
        self._client_factory.try_send(buf)

    def try_close(self):
        if (self._client_factory != None):
            self._client_factory.try_close()
        self._client_factory = None

    def _cb_connected(self):
        self._transport._cb_connected()

    def _cb_connect_failed(self, reason):
        self.try_close()
        self._transport._cb_connect_failed(reason)

    def _cb_msg_received(self, buf):
        self._transport._cb_msg_received(buf)

    def _cb_closed(self, reason):
        self.try_close()
        self._transport._cb_closed(reason)

    def _cb_failed(self, error):
        self.try_close()
        self._transport._cb_failed()

class _TwistedClientFactory(protocol.ClientFactory):
    """Twisted ClientFactory implementation"""
    def __init__(self, sock):
        self._sock = sock

    def buildProtocol(self, addr):
        self._protocol = _TwistedProtocol(self._sock)
        return self._protocol

    def clientConnectionLost(self, connector, reason):
        try:
            self._sock._cb_closed(reason)
        except:
            print sys.exc_info()[0]
            self._sock._cb_failed(sys.exc_info()[0])

    def clientConnectionFailed(self, connector, reason):
        try:
            self._sock._cb_connect_failed(reason)
        except:
            print sys.exc_info()[0]
            self._sock._cb_failed(sys.exc_info()[0])

    def try_send(self, buf):
        try:
            self._protocol.transport.write(buf)
        except:
            print sys.exc_info()[0]
            self._sock._cb_failed(sys.exc_info()[0])

    def try_close(self):
        try:
            self._protocol.transport.loseConnection()
        except:
            print sys.exc_info()[0]
            self._sock._cb_failed(sys.exc_info()[0])

class _TwistedProtocol(protocol.Protocol):
    """Twisted Protocol implementation"""
    def __init__(self, sock):
        self._sock = sock
        
    def connectionMade(self):
        try:
            self._sock._cb_connected()
        except:
            print sys.exc_info()[0]
            self._sock._cb_failed(sys.exc_info()[0])

    def connectionLost(self, reason):
        try:
            self._sock._cb_closed(reason)
        except:
            print sys.exc_info()[0]
            self._sock._cb_failed(sys.exc_info()[0])

    def dataReceived(self, buf):
        try:
            self._sock._cb_msg_received(buf)
        except:
            print sys.exc_info()[0]
            self._sock._cb_failed(sys.exc_info()[0])
