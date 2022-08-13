#!/usr/bin/env python3

from importlib import import_module
from functools import wraps

def withmodule(mod, a=None):
    def decoratormodule(f):
        @wraps(f)
        def wrapper(*args, **kwargs):
            gl = globals()
            name = a if a is not None else mod
            if name not in gl or gl[name] is None:
                gl[name] = import_module(mod)
            return f(*args, **kwargs)
        return wrapper
    return decoratormodule
