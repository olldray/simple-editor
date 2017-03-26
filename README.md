Build with `stack build` or `stack install`

Run tests with `stack test`

## Brief Description
`simple-editor` provides an implementation of the specifications
 layed out in https://www.hackerrank.com/challenges/simple-text-editor


## Basic design:

The main idea is to process the commands one at a time.

For `append` and `delete` actions, perform the action on the internal
 representaion of S and then store the inverse action in the undo
 stack. For an `undo`, pop one action off the undo stack and apply
 it to S.


## Notes on the design process:

There can be at most 10^6 commands. Of which half can be `undo()`.

There can be at most 10^6 characters of input.

Which means if I tried to store the full content of `S` for each level of
 undo, I'm looking at an upper bound of .5 Terrabytes of storage
 (probably closer to .25T, but still too big).
 So that's out.

But, I can easily store 10^6 characters so long as I only store them once.
 Which means I need to store append _actions_ and delete _actions_ for
 use when undoing.

It became clear that a quite nice solution could be had by storing the
 undo stack as the list of actions required to actually undo the work.
 This way, all the information needed to undo an action is stored
 in the stack, and the same logic can be used to do actions and undo
 them. We are also guarenteed that the maximum characters stored in
 undo actions cannot be larger than (2 * 10^6) since that is the
 maximum sum of all k in `delete k`. Though it really cannot be
 larger than 10^6 at any given time, since that is the total of
 all appended characters and deleting more than that would require
 undoing previous deletes.


## Where to go from here:

The problem statement was quite clear that it was trying to let us
 off the hook for input validation. I attempted to take advantage of
 that assurance, but it felt pretty unnatural in Haskell. I ended up
 with most of the functions in this project being partial, which
 sucks. So most things could afford to be wrapped in `Either`s or
 `ExceptT`s.

I did not do any sort of command reduction. Theoretically, since
 all the commands are read in before producing any output, one could
 look at the `print` commands and try to only do the work necessary
 to print accurately. For example, don't do any actions that will be
 undone before printing, or don't do any appends that affect parts
 of `S` that are after the largest `print`. Basically, make the
 actions and the internal state lazy. Though to be honest, I am not
 sure about the best way approach this work.
