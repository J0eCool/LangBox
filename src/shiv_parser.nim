# Parser for shiv-expressions

import sequtils
import strutils

import shiv_lexer

type
  SExprKind* = enum
    sAtom
    sList
  SExpr* = object
    case kind*: SExprKind
    of sAtom:
      name*: string
    of sList:
      elems*: seq[SExpr]

func sa*(name: string): SExpr =
  SExpr(kind: sAtom, name: name)
func sa*(x: SExpr): SExpr =
  x
func ssx*(elems: seq[SExpr]): SExpr =
  SExpr(kind: sList, elems: elems)
func ss*(elems: varargs[SExpr, sa]): SExpr =
  ssx(@elems)

type Stack[T] = distinct seq[T]
func newStack*[T](): Stack[T] =
  Stack[T](@[])
proc top*[T](stack: var Stack[T]): var T =
  seq[T](stack)[seq[T](stack).len - 1]
proc push*[T](stack: var Stack[T], item: T) =
  seq[T](stack).add(item)
proc pop*[T](stack: var Stack[T]): T =
  result = stack.top()
  seq[T](stack).del(stack.len - 1)
func len*[T](stack: Stack[T]): int =
  seq[T](stack).len

proc parse*(contents: string): SExpr =
  var stack = newStack[seq[SExpr]]()
  var parenStack = newStack[BraceKind]()
  var prevToken = Token(kind: tSpace)
  var nextIsNegative = false
  stack.push(@[])
  stack.push(@[])
  parenStack.push(bCurly)
  parenStack.push(bCurly)

  for token in lex(contents):
    if nextIsNegative and token.kind != tNumber:
      nextIsNegative = false
      stack.top().add(SExpr(kind: sAtom, name: "-"))
    case token.kind
    of tIdentifier:
      if token.value == "-" and prevToken.kind in [tSpace, tBraceOpen, tLine]:
        nextIsNegative = true
      else:
        stack.top().add(SExpr(kind: sAtom, name: token.value))
    of tNumber:
      var name = token.value
      if nextIsNegative:
        name = "-" & name
        nextIsNegative = false
      stack.top().add(SExpr(kind: sAtom, name: name))
    of tString:
      stack.top().add(SExpr(kind: sAtom, name: '"' & token.value & '"'))
    of tLine, tEof:
      if parenStack.top() == bCurly:
        let curList = stack.pop()
        if curList.len == 1 and curList[0].kind == sList:
          stack.top().add(curList[0])
        elif curList.len > 0:
          stack.top().add(SExpr(kind: sList, elems: curList))
        stack.push(@[])
    of tBraceOpen:
      case token.brace
      of bParen:
        stack.push(@[])
        parenStack.push(bParen)
      of bCurly:
        stack.push(@[])
        stack.push(@[])
        parenStack.push(bCurly)
        parenStack.push(bCurly)
      of bSquare:
        stack.push(@[])
        if prevToken.kind in [tSpace, tLine, tBraceOpen]:
          parenStack.push(bSquare)
        else:
          parenStack.push(bIndex)
      of bIndex:
        assert false
    of tBraceClose:
      case token.brace
      of bParen:
        let last = stack.pop()
        assert parenStack.pop() == bParen
        stack.top().add(SExpr(kind: sList, elems: last))
      of bCurly:
        let last = stack.pop()
        assert parenStack.pop() == bCurly
        if last.len > 0:
          stack.top().add(SExpr(kind: sList, elems: last))
        let l2 = stack.pop()
        assert parenStack.pop() == bCurly
        stack.top().add(SExpr(kind: sList, elems: l2))
      of bSquare:
        let last = stack.pop()
        let paren = parenStack.pop()
        if paren == bSquare:
          stack.top().add(ssx(@[sa("__list")] & last))
        else:
          assert paren == bIndex
          let prev = stack.top().pop()
          let lastSexpr = if last.len == 1: sa(last[0]) else: ssx(last)
          stack.top().add(ss("__index", prev, lastSexpr))
      of bIndex:
        assert false
    else:
      discard
    prevToken = token
  assert parenStack.top() == bCurly
  discard stack.pop()
  let curList = stack.pop()
  assert stack.len == 0
  SExpr(kind: sList, elems: curList)

func `==`*(a, b: SExpr): bool =
  if a.kind != b.kind:
    return false
  case a.kind
  of sAtom:
    a.name == b.name
  of sList:
    a.elems == b.elems

func `$`*(sexpr: SExpr): string =
  case sexpr.kind
  of sAtom:
    sexpr.name
  of sList:
    "(" & sexpr.elems.join(" ") & ")"

# A SExpr is complex if it is a list that contains at least one list as an element
func isComplex(sexpr: SExpr): bool =
  if sexpr.kind == sAtom:
    false
  else:
    sexpr.elems.anyIt(it.kind == sList)

const tabStr = "  "
proc pretty*(sexpr: SExpr, indent = 0): string =
  case sexpr.kind
  of sAtom:
    result = sexpr.name
  of sList:
    if not isComplex(sexpr):
      return $sexpr
    result = "("
    var didSeeList = false
    for i in 0..<sexpr.elems.len:
      let child = sexpr.elems[i]
      if not didSeeList:
        if child.kind == sList:
          didSeeList = true
        elif i != 0:
          result &= " "
      if didSeeList:
        result &= "\n" & tabStr.repeat(indent + 1)
      result &= pretty(child, indent + 1)
    result &= "\n" & tabStr.repeat(indent) & ")"
