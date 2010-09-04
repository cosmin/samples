class MoreThanOneRet(object):
    pass

class ReturnList(MoreThanOneRet):
    def __init__(self, *args):
        self.value = args

class ReturnIterator(MoreThanOneRet):
    def __init__(self, iterator):
        self.value = iterator

class MockCall(object):
    def __init__(self, name, *args, **kw):
        self.name = name
        self.args = args
        self.kw = kw

    def __str__(self):
        return 'MockCall: %s: args %s: kw %s' % (self.name, self.args, self.kw)

class MockReturn(object):
    def __init__(self, name, parent, real = None, ret = None):
        self.name = name
        self.real = real
        self.ret = ret
        self.parent = parent

    def __call__(self, *args, **kw):
        self.parent._log_call(MockCall(self.name, *args,**kw))
        if self.real:
            return self.real(*args, **kw)
        else:
            return self.ret
            if self.ret:
                return self.ret
            else:
                return self.iterator.next()

class ClassMock(object):
    def __init__(self):
        self.__setattrs = {}
        self.__returnvals = {}
        self.__callinfo = []
        self.__skipattrs = []

    def __getattribute__(self, attrname):
        if (attrname.startswith('__') or
            attrname.startswith('_ClassMock') or
            attrname in ('_add_return',
                         '_add_attribute',
                         '_log_call',
                         '_calls',
                         '_skip',
                         )):
            return object.__getattribute__(self, attrname)

        sa = self.__setattrs
        skip = self.__skipattrs
        retvals = self.__returnvals

        real = object.__getattribute__(self, attrname)

        if attrname in skip:
            return real
        elif attrname in sa:
            return sa[attrname]
        else:
            if attrname in retvals:
                (iter, ret) = retvals[attrname]
                if iter:
                    return MockReturn(attrname, self, ret=ret.next())
                else:
                    return MockReturn(attrname, self, ret=ret)
            else:
                return MockReturn(attrname, self, real=real)

    def _add_return(self, name, ret = None):
        retvals = self.__returnvals
        if isinstance(ret, MoreThanOneRet):
            retvals[name] = (True, ret.value.__iter__())
        else:
            retvals[name] = (False, ret)

    def _add_attribute(self, attrname, value):
        sa = self.__setattrs
        sa[attrname] = value

    def _skip(self, *attrnames):
        self.__skipattrs.extend(attrnames)

    def _log_call(self, mock_call):
        callinfo = self.__callinfo
        callinfo.append(mock_call)

    @property
    def _calls(self):
        callinfo = self.__callinfo
        return callinfo

