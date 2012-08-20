package com.clarifi.machines

/** A natural transformation from `K[O, _]` to `L[I, _]` */
trait Fitting[-K[-_, +_], +L[-_, +_], -I, +O] {
  def apply[R](f: K[O, R]): L[I, R]
}

