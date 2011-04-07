MessagePack-RPC Erlang
======================

This code is in alpha-release. Synchronous RPC seems working.

# prequisites
Erlang runtime system (http://erlang.org/)

# client

## usage

1. connect to server with specifying address and port.
2. append mp_client after some supervisor if you want to keep connection.
3. close it after RPC call ends.

## supervision tree

  (your supervisor) - mp_client

mp_client is implemented over gen_server, so you can
link mp_client under your supervisor

# server

## usage

1. write a module that behaves as a mp_session.
2. call mp_server:start/1 with your setting:

   [{module, Mod}, {addr, Address}, {port, Port}].


## supervision tree

   (your supervisor) - mp_server_sup -+- mp_server_srv
                                      +- mp_server_sup2 -+- mp_session


see sample_app.erl and sample_srv.erl for detailed usages.
the latter is a sample implementation of RPC callbacks, overriding mp_session

# install

 //checkout 
1. $ git clone git//github.com/msgpack/msgpack-rpc.git
2. $ cd msgpack-rpc/erlang
3. $ omake ct  # if you have omake
4. $ make ct   # else
5. # escript scripts/setup.es install

## TODO

- error handling 
-- what if happens when badarg/noproc/bad_clause, and exceptions.
- (client) automatic random session-id generator
- (server) multiple identifier (is it needed?)
- asynchronous-RPC
