package com.clarifi

package object machines {
  type Process[-I, +O] = Machine[Function1, I, O]
}

