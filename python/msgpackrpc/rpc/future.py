class Future(object):
    """
    This class is used as the result of asynchronous call.
    By using join(), the caller is able to wait for the completion.
    """

    def __init__(self, loop):
        self._error = None
        self._result = None
        self._loop = loop

    def join(self):
        # TODO(kzk): implement client-side timeout
        while (self._error == None and self._result == None):
            self._loop.run()

    def get_result(self):
        return self._result

    def set_error(self, error):
        self._error = error

    def set_result(self, result):
        self._result = result
