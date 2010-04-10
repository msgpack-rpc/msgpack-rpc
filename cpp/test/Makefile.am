ldflags = \
		-L../src/cclog/.libs \
		-L../src/msgpack/rpc/.libs \
		-lmpio -lcclog -lmsgpack -lmsgpack-rpc

AM_CPPFLAGS   = -I../src
AM_C_CPPFLAGS = -I../src

check_HEADERS = \
		echo_server.h

check_PROGRAMS = \
		sync_call \
		async_call \
		callback

sync_call_SOURCES = sync_call.cc
sync_call_LDFLAGS = $(ldflags)

async_call_SOURCES = async_call.cc
async_call_LDFLAGS = $(ldflags)

callback_SOURCES = callback.cc
callback_LDFLAGS = $(ldflags)

TESTS = $(check_PROGRAMS)

