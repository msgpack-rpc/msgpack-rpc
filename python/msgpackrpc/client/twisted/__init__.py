from msgpackrpc.client.address import Address
from msgpackrpc.client.twisted import session, loop

__all__ = ['Client']

class Client(session.Session):
    """
    The RPC client class, which exposes the API to the users.
    The folowing code shows how to use the asynchronous API.

    from msgpackrpc.client.twisted import Client
    c = client.Client("127.0.0.1", 1985)
    f = c.send("hello1", [1])
    f.join()
    print f.get_result()
    c.close()
    """

    def __init__(self, host, port):
        session.Session.__init__(self, Address(host, port), loop.Loop())

    def send(self, method, args):
        """
        This is the asynchronous API, which returns Future class.
        To get the result from the Future, you need to future.join() like this:

        future = client.send("method", args)
        future.join()
        ret = future.get_result()
        """
        return self.send_request(method, args)

    def call(self, method, args):
        """Synchronous call"""
        future = self.send(method, args)
        future.join()
        return future.get_result()

    def close(self):
        self.try_close()
