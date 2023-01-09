
template cptr(s: string): ptr[char] = if likely(s.len > 0): s[0].unsafeAddr else: emptyCStr
