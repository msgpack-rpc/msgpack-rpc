#include "version.h"

const char* msgpack_rpc_version(void)
{
	return MSGPACK_RPC_VERSION;
}

int msgpack_rpc_version_major(void)
{
	return MSGPACK_RPC_VERSION_MAJOR;
}

int msgpack_rpc_version_minor(void)
{
	return MSGPACK_RPC_VERSION_MINOR;
}

