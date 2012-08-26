Machines
========

Machines are demand-driven input sources like pipes, conduits, or iteratees, but can support inputs of arbitrary complexity.

You design a `Machine` by writing a `Plan`. You then `compile` the machine if it is to run once to completion, or designate it to run `repeatedly`.

Simple machines that take one input are called a `Process` and processes form a `Category`. More generally you can attach a `Process` to the output of any type of `Machine`, yielding a new `Machine`.

More complicated machines provide other ways of connecting to them.

Typically the use of machines proceeds by using simple plans into machine `Tee`s and `Wye`s, `cap`ping many of the inputs to those with possibly monadic sources, feeding the input (possibly repeatedly).

Machines are entirely pure. They embed no monadic effects. An effectful `Driver` can be used to drive a machine. Such a driver can have effects when requesting the input and emitting the output.

