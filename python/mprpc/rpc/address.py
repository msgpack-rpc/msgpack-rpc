class Address:
    """
    The class to represent the RPC address.
    Currently, only IPV4 is supported in this version.
    
    TODO(kzk): support IPV6, and UnixDomainSocket
    """

    def __init__(self, host, port):
        self._host = host
        self._port = port

    def get_host(self):
        return self._host

    def get_port(self):
        return self._port
