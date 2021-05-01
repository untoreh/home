#!/usr/bin/env fish

set -q THEFUCK_DEF || set -U THEFUCK_DEF (thefuck --alias)
string split '\n' $THEFUCK_DEF | source
