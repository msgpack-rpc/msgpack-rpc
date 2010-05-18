from msgpackrpc.client.twisted import socket

import msgpack

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
