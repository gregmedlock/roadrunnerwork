import libhello as h

__all__ = ('Greeter')

class Greeter(object):
    def __init__(self):
        self.ptr = h.create_greeter()

    def __del__(self):
        h.delete_greeter(self.ptr)

    @property
    def count(self):
        return h.get_count(self.ptr)

    def __str__(self):
        return '<%s count:%s>' % (self.__class__.__name__, self.count)

    def greet(self, name):
        h.print_greeting(self.ptr, name)

