{application, mp_testapp,
 [{description, "MessagePack RPC test server"},
  {vsn, "0.0.1"},
  {modules, 
   [mp_server, msgpack,	
    mp_server_sup, mp_test_app,
    mp_server_srv, mp_server_sup2]
  },
  {registered, []},
  {mod, {mp_test_app, []}},
  {env, []},
  {applications, [kernel, stdlib]}]}.
