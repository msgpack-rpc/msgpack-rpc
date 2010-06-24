
AM_CPPFLAGS   = -I../src
AM_C_CPPFLAGS = -I../src

AM_LDFLAGS = \
		../src/cclog/libcclog.la \
		../src/msgpack/rpc/libmsgpack-rpc.la \
		-lmpio -msgpack

check_HEADERS = \
		echo_server.h

check_PROGRAMS = \
		sync_call \
		async_call \
		callback \
		notify \
		async_server \
		error \
		udp

TESTS = $(check_PROGRAMS)

sync_call_SOURCES = sync_call.cc

async_call_SOURCES = async_call.cc

callback_SOURCES = callback.cc

notify_SOURCES = notify.cc

async_server_SOURCES = async_server.cc

error_SOURCES = error.cc

udp_SOURCES = udp.cc

