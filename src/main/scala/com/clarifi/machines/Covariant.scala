package com.clarifi.machines

trait Covariant {
  type X
//  def map[Y](h: X => Y): Covariant { type X >: Y }
}

