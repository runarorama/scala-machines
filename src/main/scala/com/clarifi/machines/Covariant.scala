package com.clarifi.machines

trait Covariant {
  type Ty
  def map[U](h: Ty => U): Covariant { type Ty >: U }
}

