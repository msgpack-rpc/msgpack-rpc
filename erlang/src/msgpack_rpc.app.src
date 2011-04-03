{application, msgpack_rpc,
 [{description, "MessagePack RPC server application"},
  {vsn, "0.0.1"},
  {modules, 
   [mp_server, msgpack, mp_server_app,
    mp_server_sup, mp_server_listener_sup, 
    mp_server_session_sup, mp_server_srv, mp_rpc ]
  },
  {registered, []},
  {mod, {mp_server_app, []}},
  {env, []},
  {applications, [kernel, stdlib]}]}.
