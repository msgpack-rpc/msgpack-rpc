
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
		notify

TESTS = $(check_PROGRAMS)

sync_call_SOURCES = sync_call.cc

async_call_SOURCES = async_call.cc

callback_SOURCES = callback.cc

notify_SOURCES = notify.cc

