#!/usr/bin/env python
# coding: utf-8

from msgpackrpc.client import *
from msgpackrpc.transport import tcp

import echoserver

SERVER = PORT = None

def setup():
    global SERVER, PORT
    SERVER, PORT = echoserver.serve()

def teardown():
    global SERVER, PORT
    SERVER.shutdown()
    SERVER = PORT = None

def test_client():
    client = Client(tcp.TCPTransport, ('localhost', PORT), {})

    f1 = client.send_request('echo', 'foo')
    f2 = client.send_request('echo', 'bar')
    f3 = client.send_request('echo', 'baz')

    assert f2.result() == 'bar'
    assert f1.result() == 'foo'
    assert f3.result() == 'baz'

if __name__ == '__main__':
    setup()
    test_client()
    teardown()
