import multiprocessing as mp
import sys
from time import time
from numpy import empty, mean

def subprocess_call(fn, *args, **kwargs):
    """Executes a function in a forked subprocess"""

    ctx = mp.get_context('fork')
    q = ctx.Queue(1)
    is_error = ctx.Value('b', False)

    def target():
        try:
            q.put(fn(*args, **kwargs))
        except BaseException as e:
            is_error.value = True
            q.put(e)

    ctx.Process(target=target).start()
    result = q.get()
    if is_error.value:
        raise result

    return result

class subprocess:
    """Decorate a function to hint that it should be run in a forked subprocess"""
    def __init__(self, fn):
        self.fn = fn
    def __call__(self, *args, **kwargs):
        return subprocess_call(self.fn, *args, **kwargs)


@subprocess
def bench(x, y, t=1000):
    assert 'textop' not in sys.modules.keys()
    from textop import isrelevant
    return run(x, y, isrelevant)

@subprocess
def runnim(x, y):
    import textop
    return textop.isrelevant(x, y)

# from main import get_kw_sources
# res = get_kw_sources("vps", remove=False)
# from articles import goose
# g = goose(res[0]["url"])

def run(x, y, f, t=100):
    tries = empty(t)
    for n in range(t):
        start = time()
        f(x, y)
        tries[n] = time() - start
    return mean(tries)

# from yattag import Doc
# def buildHtml():
#     doc, tag, text = Doc().tagtext()
#     with tag("html"):
#         with tag("head"):
#             doc.stag("meta", charset="UTF-8")
#             doc.stag("meta", name="viewport", content="width=device-width, initial-scale=1")
#             with tag("title"):
#                 text("hello")
#             doc.stag("meta", name="description", content="")
#             doc.stag("script", src="")
#         with tag("body"):
#             text("Wow")

import gumbo
