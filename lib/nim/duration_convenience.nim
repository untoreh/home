import std/times

template ddays(n: int): Duration  = initDuration(days = n)
template dseconds(n: int): Duration  = initDuration(seconds = n)
template dminutes(n: int): Duration  = initDuration(minutes = n)
