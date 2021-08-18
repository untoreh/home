#!/usr/bin/env python3

import json
import os
from pathlib import Path

proxies = Path(f"{os.environ['HOME']}") / "tmp" / "proxies" / "proxies.json"

with open(proxies, "r") as f:
    prx = json.load(f)

selected = []

for p in prx:
    selected.append(f"{p['host']}:{p['port']}")

print(str.join("\n", selected))
