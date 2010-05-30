{application, mp_server,
 [{description, "MessagePack RPC server"},
  {vsn, "0.0.1"},
  {modules, 
   [mp_server, msgpack,	
    mp_server_sup, mp_server_app]
  },
  {registered, []},
  {mod, {, []}},
  {env, []},
  {applications, [kernel, stdlib]}]}.
