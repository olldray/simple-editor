There can be at most 10^6 commands. Of which half can be `undo()`.

There can be at most 10^6 characters of input.

Which means if I tried to store the full content of `S` for each level of
 undo, I'm looking at an upper bound of .5 Terrabytes of storage
 (probably closer to .25T, but still too big).
 So that's out.

But, I can easily store 10^6 characters so long as I only store them once.
 Which means I need to store append _actions_ and delete _actions_ for
 use when undoing.
