import SocketServer
from msgpack import packs, Unpacker
import logging

_log = logging.getLogger(__name__)

class RequestHandler(SocketServer.StreamRequestHandler):
    """RequestHandler for TCPServer."""

    # Dispatcher for request and notify.
    # Overwrite this.
    dispatcher = None

    # buffer size of recv/send
    rbufsize = wbufsize = 0

    def _send_response(self, msgid, error, result):
        msg = packs(2, msgid, error, result)
        self.wfile.write(msg)

    def handler(self):
        unpacker = Unpacker()
        dispatch = self,dispatcher.dispatch
        _send_response = self._send_response

        while True:
            data = self.rfile.read()
            if not data:
                break
            unpacker.feed(data)
            for msg in unpacker:
                dispatch(msg, _send_response)


class Dispatcher(object):
    def __init__(self):
        self._methods = {}
        self._futures = {}
        self._obj = None

    def bind_object(self, obj):
        self._obj = obj

    def dispatch_request(self, msg, send_response):
        _, msgid, method, params = msg
        result = error = None
        try:
            result = self._methods[method](*params)
        except Exception as e:
            error = str(e)
        send_response(msgid, error, result)

    def dispatch_response(self, msg):
        # TODO: implement bi-directional request/response.
        #_, msgid, error, result = msg

    def dispatch_notify(self, msg):
        _, method, params = msg
        self._methods[method](*params)

    def dispatch(self, msg, send_response):
        _type = msg[0]
        if type_ == 0:    # request
            self.dispatch_request(msg, send_response)
        elif type_ == 1:  # response
            self.dispatch_response(msg)
        elif type_ == 2:  # notify
            self.dispatch_notify(msg)
        else: # What's this??
            _log.warn("Invalid message: %r", msg)
            # TODO: raise an exception.

    def register(self, name=None):
        def _wrapper(func):
            if name is None:
                name = func.__name__
            self._methods[name] = func
            return func
        return _wrapper


from Queue import Queue
from threading import Thread

class ThreadPool(object):

    def __init__(self, workers=5):
        self._queue = Queue()
        assert size > 0
        threads = []
        for i in xrange(workers):
            t = Thread(target=self._worker)
            t.setDaemon(True)
            t.start()
            threads.append(t)

    def _worker(self):
        while True:
            try:
                func, args = self._queue.pop()
                func(*args)
            except Exception as e:
                _log.error("Error occured in worker: %r", e)

    def execute(self, func, args=(,)):
        self._queue.push((func, args))


class ThreadPoolServer(SocketServer.TCPServer):

    def __init__(self, address, handler, workers=5):
        SocketServer.TCPServer(self, address, handler)
        self._thread_pool = ThreadPool(workers)

    def process_request_thread(self, request, client_address):
        """Same as in BaseServer but as a thread.

        In addition, exception handling is done here.

        """
        try:
            self.finish_request(request, client_address)
            self.close_request(request)
        except:
            self.handle_error(request, client_address)
            self.close_request(request)

    def process_request(self, request, client_address):
        """Start a new thread to process the request."""
        self._thread_pool.execute(self.process_request_thread,
                                  (request, client_address))


def make_request_handler(dispatcher):
    class Handler(RequestHandler):
        dispatcher = dispatcher
    return Handler


def make_server(dispatcher, address, server_class=None):
    if server_class is None:
        server_class = SocketServer.ThreadingTCPServer()
    return server_class(server_address,
                        make_request_handler(dispatcher))
