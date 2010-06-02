{application, mp_server,
 [{description, "MessagePack RPC server"},
  {vsn, "0.0.1"},
  {modules, 
   [mp_server, msgpack,	
    mp_server_sup, mp_server_app,
    mp_server_srv, mp_server_sup2]
  },
  {registered, []},
  {mod, {mp_server_app, []}},
  {env, []},
  {applications, [kernel, stdlib]}]}.
