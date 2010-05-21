#!/usr/bin/env python
# coding: utf-8

from msgpackrpc.client import *
from msgpackrpc.transport import tcp

import echoserver

def test_client():
    port = echoserver.serve()
    client = Client(tcp.TCPTransport, ('localhost', port), {})

    f1 = client.send_request('echo', 'foo')
    f2 = client.send_request('echo', 'bar')
    f3 = client.send_request('echo', 'baz')

    assert f2.result() == 'bar'
    assert f1.result() == 'foo'
    assert f3.result() == 'baz'

    print f1.result()
    print f2.result()
    print f3.result()

if __name__ == '__main__':
    test_client()
