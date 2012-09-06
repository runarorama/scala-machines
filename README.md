Machines
========

Machines are demand-driven coroutines. They are similar to Pipes, Conduits, or Iteratees, but can support inputs of arbitrary complexity and are entirely pure. Instead of having monadic effects of its own, a `Machine` can be driven by a `Driver` which can have effects when feeding the `Machine` input or when reading its output.

You design a `Machine` by writing a `Plan`. You then `compile` the machine if it is to run once to completion, or designate it to run `repeatedly`. `Plan` provides a monadic API for building machines, but `Machines` themselves do not form a monad.

Simple machines that read from one input source are called a `Process` and processes form a `Category`. More generally you can attach a `Process` to the output of any type of `Machine`, yielding a new `Machine`.

More complicated machines provide other ways of connecting to them.
