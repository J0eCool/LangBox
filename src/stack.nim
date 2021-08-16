type Stack*[T] = distinct seq[T]

func newStack*[T](): Stack[T] =
  Stack[T](@[])
proc top*[T](stack: var Stack[T]): var T =
  seq[T](stack)[seq[T](stack).len - 1]
proc push*[T](stack: var Stack[T], item: T) =
  seq[T](stack).add(item)
proc pop*[T](stack: var Stack[T]): T =
  assert stack.len > 0
  result = stack.top()
  seq[T](stack).del(stack.len - 1)
func len*[T](stack: Stack[T]): int =
  seq[T](stack).len
