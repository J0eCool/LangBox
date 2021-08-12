# Driver for Shenk mini-language

import os
# import sequtils
import strutils

import logger
import shiv_lexer

#-------------------------------------------------------------------------------
# Shenk AST

type
  ExprKind = enum
    exNumber
    exString
    exIdentifier
    exCall
  Expr = object
    case kind: ExprKind
    of exNumber:
      num: float
    of exString:
      str: string
    of exIdentifier:
      name: string
    of exCall:
      callee: string
      args: seq[Expr]

  Func = object
    name: string
    args: seq[string]
    body: seq[Expr]

  Ast = object
    funcs: seq[Func]
    exprs: seq[Expr]

#-------------------------------------------------------------------------------
# Shenk parser

type
  Parser = object
    tokens: seq[Token]
    index: int
    log: Logger

proc error(parser: Parser, args: varargs[string, `$`]) =
  parser.log "[Parse Error]: ", args.join("")
  echo "[Parse Error]: ", args.join("")
  quit(1)
proc errorIf(parser: Parser, cond: bool, args: varargs[string, `$`]) =
  if cond:
    parser.error(args)

func peek(parser: Parser): Token =
  parser.tokens[parser.index]
proc pop(parser: var Parser): Token =
  result = parser.peek()
  parser.log("popping ", result)
  parser.index += 1

# consumes token that is expected to be there
proc expect(parser: var Parser, val: string) =
  let tok = parser.pop()
  parser.errorIf(tok != ident(val), "Expected the identifier ", val, ", but got ", tok)
proc expect(parser: var Parser, kind: TokenKind) =
  let tok = parser.pop()
  parser.errorIf(tok.kind != kind, "Expected a token of kind ", kind, ", but got ", tok)
proc expect(parser: var Parser, expected: Token) =
  let tok = parser.pop()
  parser.errorIf(tok != expected, "Expected ", expected, ", but got ", tok)

func isDone(parser: Parser): bool =
  parser.index >= len(parser.tokens) or parser.peek() == token(tEof)

# eliminate any non-significant whitespace
proc space(parser: var Parser) =
  while parser.peek().kind == tSpace:
    discard parser.pop()
# eliminate any whitespace; used where no space is ever significant
proc blankLines(parser: var Parser) =
  while parser.peek.kind in {tSpace, tLine}:
    discard parser.pop()
proc newline(parser: var Parser) =
  parser.space()
  parser.expect(tLine)

# checks what the next token is without consuming it; also skips any whitespace
proc nextIs(parser: var Parser, val: string): bool =
  # convenience method for identifiers
  parser.space()
  parser.peek() == ident(val)
proc nextIs(parser: var Parser, kind: TokenKind): bool =
  parser.space()
  parser.peek().kind == kind
proc nextIs(parser: var Parser, expected: Token): bool =
  parser.space()
  parser.peek() == expected

# checks the next token kind against a set of options
proc nextIn(parser: var Parser, kinds: set[TokenKind]): bool =
  parser.space()
  parser.peek().kind in kinds

proc identifier(parser: var Parser): string =
  parser.space()
  let tok = parser.pop()
  parser.errorIf(tok.kind != tIdentifier, "Expected an identifier, but got ", tok)
  tok.value

proc callExpr(parser: var Parser): Expr
proc expr(parser: var Parser): Expr =
  if parser.nextIs(tNumber):
    result = Expr(kind: exNumber, num: parseFloat(parser.pop().value))
  elif parser.nextIs(tString):
    result = Expr(kind: exString, str: parser.pop().value)
  elif parser.nextIs(tIdentifier):
    result = Expr(kind: exIdentifier, name: parser.pop().value)
  elif parser.nextIs(openBrace(bParen)):
    discard parser.pop()
    result = parser.callExpr()
    parser.expect(closeBrace(bParen))
  else:
    parser.error("unknown expr")

# expr that is guaranteed to be a function call, e.g. for use in stmts
proc callExpr(parser: var Parser): Expr =
  parser.space()
  let callee = parser.identifier()
  var args: seq[Expr]
  while not parser.nextIn({tLine, tBraceClose}):
    args.add parser.expr()
  Expr(kind: exCall, callee: callee, args: args)

proc stmt(parser: var Parser): Expr =
  parser.callExpr()
proc stmtList(parser: var Parser): seq[Expr] =
  parser.expect(openBrace(bCurly))
  parser.newline()
  while true:
    parser.blankLines()
    if parser.nextIs(closeBrace(bCurly)):
      break
    result.add parser.stmt()
    parser.newline()

proc function(parser: var Parser): Func =
  parser.log("function declaration")
  parser.expect("fn")
  result.name = parser.identifier()
  while not parser.nextIs(openBrace(bCurly)):
    result.args.add parser.identifier()
  result.body = parser.stmtList()

proc parseProgram*(input: string): Ast =
  let tokens = lexEager(input)
  var parser = Parser(tokens: tokens, log: logger("parser"))
  while not parser.isDone():
    parser.log("top level: ", parser.peek())
    if parser.nextIs("fn"):
      result.funcs.add parser.function()
    else:
      # just ignore it i'm sure it's fine
      discard parser.pop()

#-------------------------------------------------------------------------------
# Arg parser

type
  BuildKind = enum
    buildCompile
    buildRun
  Options = object
    input: string
    output: string
    build: BuildKind
    debug: bool
    tempDir: string
    useSdl: bool

func parseOptions(args: seq[string]): Options =
  result.build = buildRun
  result.debug = true
  result.tempDir = ".temp"

  var i = 0
  proc pop(): string =
    result = args[i]
    i += 1
  while i < args.len:
    let arg = pop()
    case arg
    of "--output", "-o": result.output = pop()
    of "--compile": result.build = buildCompile
    of "--run": result.build = buildRun
    of "--sdl": result.useSdl = true
    else: result.input = arg

#-------------------------------------------------------------------------------
# Main

proc doCompile(opt: Options): int =
  let log = logger("shenk main")
  log "Parsed options: ", opt
  let filename = opt.input
  assert filename != "", "Must specify an input filename"

  let tempDir = opt.tempDir
  let includeDirs = @[
    filename.parentDir(),
    getAppFilename().parentDir().parentDir() / "include",
  ]
  log "Include dirs: ", $includeDirs
  let contents = open(filename).readAll()

  if opt.debug:
    let file = open(tempDir / "debug_lex.txt", fmWrite)
    for token in lex(contents):
      file.writeLine($token)
    file.close()

  # log "Parsing Sexprs..."
  # var sexpr = parse(contents)
  # if opt.debug:
  #   let file = open(tempDir / "debug_parse_sexpr.shv", fmWrite)
  #   for elem in sexpr.elems:
  #     file.writeLine(pretty(elem))
  #   file.close()

  # var ast = parseProgram(sexpr)
  var ast = parseProgram(contents)
  echo ast

  return 0

proc main(args: seq[string]): int =
  let options = parseOptions(args)
  createDir(options.tempDir)
  if options.debug:
    initLog(options.tempDir)
  else:
    silenceLog()
  defer: closeLog()
  return doCompile(options)

when isMainModule:
  quit(main(commandLineParams()))
