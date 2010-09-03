import functools

class Container(object):
    _components = {}

    def _register(self, component_name, component, singleton = False, args = (), kwargs = {}):
        """Register a component for the given component_name. If singleton is set to true only one instace
           will be returned. Otherwise an instance will be created each time locate is called"""
        assert not self._components.has_key(component_name), "Duplicate component_name: %r" % component_name

        if singleton:
            if callable(component):
                instance = component(*args, **kwargs)
            else:
                instance = component

            def call():
                return instance
        else:
            if callable(component):
                def call(): return component(*args, **kwargs)
            else:
                def call(): return component

        self._components[component_name] = call

    def _contains(self, component_name):
        return self._components.has_key(component_name)

    def _locate(self, component_name):
        """Find the provide for the given component_name"""
        try:
            component = self._components[component_name]
        except KeyError:
            raise KeyError, "Unknown component_name named %r" % component_name
        return component()


class Inject(object):
    '''This class is used to specify required components for an IOC managed class'''
    provider = Container()

    def __init__(self, component_name):
        '''RequiredComponent_name(component_name, assertion)
        component_name is the string KEY for the name of the required component_name,
        assertion is an optional assertion to test for a specified attribute or method'''

        self.component_name = component_name

    def __get__(self, obj, T):
        '''will request the component_name upon first call'''
        return self.result

    def __getattr__(self, name):
        '''This should only be called with name=result from the __get__ call, it will return the implementation of the required component_name'''
        assert name == 'result', "Unexpected attribute request other then 'result'"
        self.result = Inject.provider._locate(self.component_name)
        return self.result

def inject(**inject_kw):
    def deco(fn):
        def new_fn(*args, **kw):
            new_args = dict((k, Container()._locate(v)) for k,v in inject_kw.items() if k not in kw)
            new_args.update(kw)
            return fn(*args, **new_args)
        functools.update_wrapper(new_fn, fn)
        return new_fn
    return deco

def component(name, singleton=False):
    def classdeco(cls):
        Container()._register(name, cls, singleton=singleton)
        return cls
    return classdeco
