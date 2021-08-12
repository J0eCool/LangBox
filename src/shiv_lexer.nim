# Lexer for shiv-expressions

import strutils

type
  Location* = tuple[row, col: int]
  TokenKind* = enum
    tSpace
    tLine
    tNumber
    tIdentifier
    tString
    tBraceOpen
    tBraceClose
    tEof
  BraceKind* = enum
    bParen
    bCurly
    bSquare
    bIndex # used for parenStack tracking
  Token* = object
    case kind*: TokenKind
    of tNumber, tIdentifier, tString:
      value*: string
    of tBraceOpen, tBraceClose:
      brace*: BraceKind
    else:
      discard
    location*: Location

func isIdentifierChar(c: char): bool =
  c == '_' or c.isAlphaNumeric()

func `==`*(a, b: Token): bool =
  if a.kind != b.kind:
    false
  else:
    case a.kind
    of tNumber, tIdentifier, tString:
      a.value == b.value
    of tBraceOpen, tBraceClose:
      a.brace == b.brace
    else:
      true

# helper token constructors to query equality
func ident*(name: string): Token =
  Token(kind: tIdentifier, value: name)
func token*(kind: TokenKind): Token =
  Token(kind: kind)
func openBrace*(kind: BraceKind): Token =
  Token(kind: tBraceOpen, brace: kind)
func closeBrace*(kind: BraceKind): Token =
  Token(kind: tBraceClose, brace: kind)

iterator lex*(input: string): Token =
  var i = 0
  var row = 1
  var col = 1

  while i < input.len:
    let start = i
    let loc = (row, col)
    let c = input[i]
    var next: Token

    i += 1
    case c
    of '\n': next = Token(kind: tLine)
    of '\r': continue # just ignore these damn things
    of ' ', '\t':
      while i < input.len and input[i] in " \t":
        i += 1
      next = Token(kind: tSpace)
    of '(': next = Token(kind: tBraceOpen, brace: bParen)
    of ')': next = Token(kind: tBraceClose, brace: bParen)
    of '{': next = Token(kind: tBraceOpen, brace: bCurly)
    of '}': next = Token(kind: tBraceClose, brace: bCurly)
    of '[': next = Token(kind: tBraceOpen, brace: bSquare)
    of ']': next = Token(kind: tBraceClose, brace: bSquare)
    of '"':
      var str = ""
      while i < input.len and input[i] != '"':
        str &= input[i]
        i += 1
      i += 1
      next = Token(kind: tString, value: str)
    of ';':
      while i < input.len and input[i] != '\n':
        i += 1
    else:
      var ident = $c
      let breakSet = "(){}[] \n\r\t"
      if c.isDigit():
        var sawDot = false
        while i < input.len:
          if input[i] in breakSet: break
          if input[i] == '.':
            if sawDot:
              break
            sawDot = true
            ident &= input[i]
          elif not input[i].isDigit():
            break
          else:
            # is digit
            ident &= input[i]
          i += 1
        if ident[^1] == '.':
          # Ignore trailing decimal, e.g. "1." -> ["1", "."]
          ident = ident[0..^2]
          i -= 1
        next = Token(kind: tNumber, value: ident)
      else:
        let isAlpha = c.isIdentifierChar()
        while i < input.len:
          if isAlpha != input[i].isIdentifierChar(): break
          if input[i] in breakSet: break
          ident &= input[i]
          i += 1
        next = Token(kind: tIdentifier, value: ident)
    next.location = loc
    yield next

    col += i - start
    if c == '\n':
      row += 1
      col = 1

  yield Token(kind: tEof)

func lexEager*(input: string): seq[Token] =
  for tok in lex(input):
    result.add tok
