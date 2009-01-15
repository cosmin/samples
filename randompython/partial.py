# Python 2.5 has support for partial function evaluation in the functools module.
# Here's is something I hacked out to do the same in Python 2.4

# This is not as complete as the wrapper in Python 2.5:
# it doesn't attempt to preserve doc strings, etc. But it should work just fine for most cases.
# Be warned however that it doesn't know how many parameters the function being decorated
# is supposed to take so if you end up passing it more than necessary you'll never a result.
# Also if your function raises a TypeError for other reasons it won't work as well.

# If you have any suggestions or improvements you'd like to share post them below.

def partial(f, *args, **kw):
    def newf(*nargs, **nkw):
        targs = list(args)
        targs.extend(nargs)
        tkw = kw.copy()
        for k in nkw:
            tkw[k] = nkw[k]
        try:
            return f(*targs, **tkw)
        except TypeError, te:
            return partial(f, *targs, **tkw)
    return newf

def partial_decorator(f):
    """
    >>> @partial_decorator
    ... def f(x, y):
    ...    return x + y
    >>> g = f(2)
    >>> g(3)
    5
    """
    def evaluate(*args, **kw):
        try:
            return f(*args, **kw)
        except TypeError:
            return partial(f, *args, **kw)

    return evaluate
