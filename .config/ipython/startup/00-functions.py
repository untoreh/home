#!/usr/bin/env python3

def toggle_debug():
    import logging
    if logging.root.level == logging.DEBUG:
        logging.root.level = logging.WARNING
    else:
        logging.root.level = logging.DEBUG
