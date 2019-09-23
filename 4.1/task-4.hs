cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Error = EQ
cmp Warning Warning = EQ
cmp Info Info = EQ
cmp Error _ = GT
cmp _ Error = LT
cmp Warning _ = GT
cmp Info _ = LT
