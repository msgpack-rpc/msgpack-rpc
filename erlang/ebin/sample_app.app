{application, sample_app,
 [{description, "MessagePack RPC test server"},
  {vsn, "0.0.1"},
  {modules, 
   [mp_server, msgpack,	
    mp_server_sup, sample_app, sample_srv,
    mp_server_srv, mp_server_sup2]
  },
  {registered, []},
  {mod, {sample_app, []}},
  {env, []},
  {applications, [kernel, stdlib]}]}.
