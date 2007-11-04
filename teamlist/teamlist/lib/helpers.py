"""Helper functions

Consists of functions to typically be used within templates, but also
available to Controllers. This module is available to both as 'h'.
"""
from genshi.core import Markup
from genshi.builder import tag
from webhelpers import *

def url_for_filename(filename):
    '''Return the URL for the given filename, the /uploads/ part is hardcoded and should be moved to the config file'''
    return '/uploads/' + filename

def wrap_helpers(localdict):
    def helper_wrapper(func):
        def wrapper_helper(*args, **kw):
            return Markup(func(*args, **kw))
        wrapper_helper.__name__ = func.__name__
        return wrapper_helper
    for name, func in localdict.iteritems():
        if not callable(func) or not func.__module__.startswith('webhelpers.rails'):
            continue
        localdict[name] = helper_wrapper(func)

wrap_helpers(locals())