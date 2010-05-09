from twisted.internet import reactor, error

class Loop:
    """
    An I/O loop class which wraps the Twisted reactor.
    """
    def __init__(self):
        self._reactor = reactor
    
    def run(self):
        """
        Enter the IO loop. The caller thread is blocked until someone calls
        the stop() function.
        """
        if not self._reactor.running:
            self._reactor.run()

    def stop(self):
        """
        Stopping the twisted reactor if it's running
        This is necessary to wakeup the process which calls reactor.run().
        That is called by Future::join()
        """
        if self._reactor.running:
            try:
                """
                We call crash() instead of stop() here. This is because
                stop() fires 'shutdown' event, and close the connection.

                TODO: The documents of Twisted says the crash() has some
                possibilities to lose the internal data. Is there any way to
                *just stop* the ioloop, by not using crash()?
                """
                self._reactor.crash()
                self._reactor._justStopped = False
            except:
                return
