#!/usr/bin/env python3

import json
import os
from pathlib import Path

proxies = Path(f"{os.environ['HOME']}") / "tmp" / "proxies" / "proxies.json"

with open(proxies, "r") as f:
    try:
        prx = json.load(f)
    except:
        # fix json
        f.seek(0)
        txt = f.read()
        txt = str.join("", [txt.rpartition(",")[0], "]"])
        prx = json.loads(txt)

selected = []

for p in prx:
    selected.append(f"{p['host']}:{p['port']}")

print(str.join("\n", selected))
