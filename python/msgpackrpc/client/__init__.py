# coding: utf-8

from msgpack import packs, Unpacker

__all__ = ['ProtocolError', 'TransportError', 'Client']

class ProtocolError(Exception):
    pass

class TransportError(Exception):
    pass

def msgidgen():
    """Generator that generates msgid.

    NOTE: Don't use in multithread. If you want use this
    in multithreaded application, use lock.
    """
    counter = 0
    while True:
        yield counter
        counter += 1
        if counter > (1 << 30):
            counter = 0

class Future(object):
    """Future object.
    """

    def __init__(self, client):
        self._done = False
        self._result = None
        self._error = None
        self._client = client
        self._done_callbacks = None

    def add_done_callback(fn):
        if self._done_callbacks is None:
            self._done_callbacks = []
        self._done_callbacks.append(fn)

    def remove_done_callback(fn):
        self._done_callbacks.remove(fn)

    def _wait(self):
        # TODO: implement timeout.
        while not self._done:
            self._client.try_recv()

    def result(self):
        self._wait()
        return self._result

    def error(self):
        self._wait()
        return self._error

    def done(self):
        return self._done

    def _exec_callbacks(self):
        if self._done_callbacks is not None:
            for fn in self._done_callbacks:
                fn(self)
            self._done_callbacks = None

    def set_result(self, result):
        self._result = result
        self._done = True

    def set_error(self, error):
        self._error = error
        self._done = True


class Client(object):
    """Simple client.
    This class is not thread safe."""
    _transport = None

    def __init__(self, transport_factory, args, kwargs):
        self._transport_factory = transport_factory
        self._transport_args = args
        self._transport_kwargs = kwargs
        self._msgidgen = msgidgen()
        self._req_table = {}
        self._unpacker = Unpacker()

    def _get_transport(self):
        if not self._transport:
            self._transport = self._transport_factory(
                    *self._transport_args,
                    **self._transport_kwargs)
        return self._transport

    def _send_msg(self, msg):
        msg = packs(msg)
        transport = self._get_transport()
        transport.try_send(msg)

    def send_request(self, method, args, callback=None):
        msgid = self._msgidgen.next()
        future = Future(self)
        self._req_table[msgid] = future
        try:
            self._send_msg((0, msgid, method, args))
            if callback is not None:
                future.add_done_callback(callback)
            return future
        except:
            del self._req_table[msgid]
            raise

    def call_request(self, method, args):
        """Call request synchronous.
        Return (error, result) tuple."""
        future = self.send_request()
        return future.error(), future.result()

    def try_close(self):
        if self._transport:
            self._transport.try_close()
        self._transport = None

    def try_recv(self):
        data = self._get_transport().try_recv()
        self._unpacker.feed(data)
        for msg in self._unpacker:
            self._recv_msg(msg)

    def _recv_msg(self, msg):
        if len(msg) != 4:
            raise ProtocolError('Invalid msgpack-rpc protocol')
        msgtype, msgid, msgerr, msgret = msg
        if msgtype != 1:
            raise ProtocolError('Invalid msgpack-rpc protocol')

        try:
            future = self._req_table.pop(msgid)
        except KeyError:
            raise ProtocolError('Unknown msgid: %r' % (msgid,))

        if msgerr is not None:
            future.set_error(msgerr)
        else:
            future.set_result(msgret)

    def _connection_closed(self, reason):
        for msgid, future in self._req_table.iteritems():
            future.set_error(TransportError(reason))
        self._req_table.clear()
        self.try_close()
