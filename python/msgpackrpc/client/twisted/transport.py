import sys
import msgpack
from twisted.internet import protocol, reactor

__all__ = ['TCPTransport']


class TCPTransport(object):
    """
    TCPTransport sends/receives the data, by using underlying socket layer.
    Before sending the data, it serializes the data into MessagePack format.
    As same, it deserializes the data when it receives the data.

    This class also hides the latency of establishing the connection. If the
    connection is not established. the sending messages are temporarily queued.
    Then, they are actually sent to the network when it's connected.
    """

    def __init__(self, session, loop):
        self._session = session
        self._loop = loop

        self._packer = msgpack.Packer()
        self._unpacker = msgpack.Unpacker()
        self._is_connecting = False
        self._is_connected = False
        self._socket = socket.TCPSocket(self._session.get_addr(), loop, self)
        self._pending_msgs = []

    def send_message(self, message):
        packed_msg = self._packer.pack(message)
        if (self._is_connected):
            self._socket.try_send(packed_msg)
        else:
            if not self._is_connecting:
                self._socket.try_connect()
                self._socket.is_connecting = False
            self._pending_msgs.append(packed_msg)

    def try_send_pending(self):
        for msg in self._pending_msgs:
            self._socket.try_send(msg)
        self._pending_msgs = []

    def try_close(self):
        if (self._socket != None):
            self._socket.try_close()
        self._is_connecting = False
        self._is_connected = False
        self._socket = None
        self._pending_msgs = []

    def _cb_connected(self):
        """The callback called when the connection failed.
        Called by the socket layer.
        """
        self._is_connecting = False
        self._is_connected = True
        self.try_send_pending()

    def _cb_connect_failed(self, reason):
        """The callback called when the connection failed.
        Called by the socket layer.
        """
        self.try_close()
        self._session._cb_connect_failed(reason)

    def _cb_msg_received(self, buf):
        """The callback called when the message arrives.
        Called by the socket layer.
        """
        self._unpacker.feed(buf)
        for msg in self._unpacker:
            self._session._cb_msg_received(msg)

    def _cb_closed(self, reason):
        """The callback called when the connection closed.
        Called by the socket layer.
        """

        self.try_close()
        self._session._cb_closed(reason)

    def _cb_failed(self):
        """The callback called when the error occurred.
        Called by the socket layer.
        """
        self.try_close()
        self._session._cb_failed()


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
