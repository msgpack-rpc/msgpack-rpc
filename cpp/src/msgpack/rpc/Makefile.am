lib_LTLIBRARIES = libmsgpack-rpc.la

libmsgpack_rpc_includedir = $(includedir)/msgpack/rpc

libmsgpack_rpc_la_CPPFLAGS = -I../..

libmsgpack_rpc_la_SOURCES = \
		address.cc \
		client.cc \
		future.cc \
		reqtable.cc \
		request.cc \
		server.cc \
		session.cc \
		session_pool.cc \
		tcp.cc

libmsgpack_rpc_include_HEADERS = \
		address.h \
		caller.h \
		client.h \
		dispatcher.h \
		future.h \
		future_impl.h \
		impl_fwd.h \
		loop.h \
		loop_util.h \
		message.h \
		message_sendable.h \
		option.h \
		reqtable.h \
		request.h \
		request_impl.h \
		server.h \
		session.h \
		session_impl.h \
		session_pool.h \
		tcp.h \
		transport.h \
		types.h

noinst_HEADERS =

PREP_SOURCE = \
		caller.hmpl

PREP_TARGET = $(PREP_SOURCE:mpl=)
EXTRA_DIST = $(PREP_SOURCE)

%.h: %.hmpl
	$(MPLEX) $< -o $@

%.cc: %.ccmpl
	$(MPLEX) $< -o $@

prep: $(PREP_TARGET)

prepc:
	rm -f $(PREP_TARGET)

