lib_LTLIBRARIES = libmsgpack-rpc.la

# -version-info CURRENT:REVISION:AGE
libmsgpack_rpc_la_LDFLAGS = -version-info 1:0:0

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
		transport/tcp.cc \
		transport/udp.cc \
		transport/unix.cc \
		version.c

nobase_libmsgpack_rpc_include_HEADERS = \
		address.h \
		caller.h \
		client.h \
		exception.h \
		future.h \
		impl_fwd.h \
		loop.h \
		loop_util.h \
		protocol.h \
		request.h \
		server.h \
		session.h \
		session_pool.h \
		types.h \
		transport.h \
		transport/tcp.h \
		transport/udp.h \
		transport/unix.h \
		version.h

noinst_HEADERS = \
		exception_impl.h \
		future_impl.h \
		request_impl.h \
		server_impl.h \
		session_impl.h \
		session_pool_impl.h \
		reqtable.h \
		message_sendable.h \
		transport_impl.h \
		transport/base.h

EXTRA_DIST = \
		caller.hmpl \
		version.h.in

