lib_LTLIBRARIES = libmsgpack-rpc.la

libmsgpack_rpc_includedir = $(includedir)/msgpack/rpc

libmsgpack_rpc_la_CPPFLAGS = -I../..

libmsgpack_rpc_la_SOURCES = \
		address.cc \
		client.cc \
		exception.cc \
		future.cc \
		reqtable.cc \
		request.cc \
		server.cc \
		session.cc \
		session_pool.cc \
		transport/tcp.cc

libmsgpack_rpc_include_HEADERS = \
		address.h \
		caller.h \
		client.h \
		dispatcher.h \
		exception.h \
		future.h \
		impl_fwd.h \
		loop.h \
		loop_util.h \
		message.h \
		option.h \
		request.h \
		server.h \
		session.h \
		session_pool.h \
		types.h

noinst_HEADERS = \
		future_impl.h \
		request_impl.h \
		session_impl.h \
		reqtable.h \
		message_sendable.h \
		transport/base.h \
		transport/listener.h \
		transport/tcp.h

EXTRA_DIST = \
		caller.hmpl

