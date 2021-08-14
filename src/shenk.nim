# Driver for Shenk mini-language

import algorithm
import os
# import sequtils
import strutils
import tables

import logger
import shiv_lexer
import stack

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

  StmtKind = enum
    stCall
    stReturn
  Stmt = object
    case kind: StmtKind
    of stCall, stReturn:
      ex: Expr

  Func = object
    name: string
    args: seq[string]
    body: seq[Stmt]

  Ast = object
    funcs: seq[Func]

#-------------------------------------------------------------------------------
# Shenk AST pretty-printer

proc pretty(ex: Expr): string =
  case ex.kind
  of exNumber:
    $ex.num
  of exString:
    '"' & ex.str & '"'
  of exIdentifier:
    ex.name
  of exCall:
    var args = ""
    for arg in ex.args:
      args &= " " & arg.pretty()
    "(" & ex.callee & args & ")"
proc pretty(st: Stmt): string =
  case st.kind
  of stCall:
    "Call " & st.ex.pretty()
  of stReturn:
    "Return " & st.ex.pretty()

proc pprint(log: Logger, fn: Func) =
  log "Func ", fn.name
  var args = ""
  for arg in fn.args:
    if args != "":
      args &= ", "
    args &= arg
  log "Args: ", args
  log "Body:"
  for st in fn.body:
    log "  ", st.pretty()

proc pprint(log: Logger, ast: Ast) =
  log "Functions"
  log ""
  for fn in ast.funcs:
    pprint(log, fn)

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
  parser.log "popping ", result
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

proc stmt(parser: var Parser): Stmt =
  if parser.nextIs(tIdentifier):
    case parser.peek().value
    of "return":
      discard parser.pop()
      return Stmt(kind: stReturn, ex: parser.callExpr())
  Stmt(kind: stCall, ex: parser.callExpr())
proc stmtList(parser: var Parser): seq[Stmt] =
  parser.expect(openBrace(bCurly))
  parser.newline()
  while true:
    parser.blankLines()
    if parser.nextIs(closeBrace(bCurly)):
      break
    result.add parser.stmt()
    parser.newline()
  parser.expect(closeBrace(bCurly))

proc function(parser: var Parser): Func =
  parser.log "function declaration"
  parser.expect("fn")
  result.name = parser.identifier()
  while not parser.nextIs(openBrace(bCurly)):
    result.args.add parser.identifier()
  result.body = parser.stmtList()

proc parseProgram*(input: string): Ast =
  let tokens = lexEager(input)
  var parser = Parser(tokens: tokens, log: logger("parser"))

  parser.blankLines()
  var topLevelStmts: seq[Stmt]
  while not parser.isDone():
    parser.log "top level: ", parser.peek()
    if parser.nextIs("fn"):
      result.funcs.add parser.function()
    else:
      topLevelStmts.add parser.stmt()
    parser.blankLines()
  result.funcs.add Func(name: "_main", body: topLevelStmts)

  parser.log "Done"

#-------------------------------------------------------------------------------
# Shenk Interpreter

type
  ValueKind = enum
    tyVoid
    tyNum
    tyStr
  Value = object
    case kind: ValueKind
    of tyVoid:
      discard
    of tyNum:
      num: float
    of tyStr:
      str: string

  InstrKind = enum
    iValue
    iLoad
    iCall
    iReturn
  Instr = object
    case kind: InstrKind
    of iValue:
      val: Value
    of iLoad:
      name: string
    of iCall:
      callee: string
      nargs: int
    else:
      # todo
      discard

  # codegen'd function
  IFunc = object
    name: string
    args: seq[string]
    instrs: seq[Instr]

  Scope = object
    instrs: seq[Instr]
    pc: int
    vars: Table[string, Value]
    vals: Stack[Value]

  Interpreter = object
    log: Logger
    funcs: Table[string, IFunc]

    # Runtime state
    scopes: Stack[Scope]

func toString(val: Value): string =
  case val.kind
  of tyVoid:
    "<void>"
  of tyNum:
    $val.num
  of tyStr:
    val.str

# Bytecode generation

proc codegen(ex: Expr): seq[Instr] =
  case ex.kind
  of exNumber:
    result.add Instr(kind: iValue, val: Value(kind: tyNum, num: ex.num))
  of exString:
    result.add Instr(kind: iValue, val: Value(kind: tyStr, str: ex.str))
  of exIdentifier:
    result.add Instr(kind: iLoad, name: ex.name)
  of exCall:
    for arg in ex.args:
      result &= codegen(arg)
    result.add Instr(kind: iCall, callee: ex.callee, nargs: ex.args.len)

proc codegen(st: Stmt): seq[Instr] =
  case st.kind
  of stCall:
    codegen(st.ex)
  of stReturn:
    codegen(st.ex) & @[Instr(kind: iReturn)]

proc codegen(fn: Func): IFunc =
  result.name = fn.name
  result.args = fn.args
  for st in fn.body:
    result.instrs &= codegen(st)

proc newInterpreter(ast: Ast): Interpreter =
  result.log = logger("interpreter")
  for fn in ast.funcs:
    let f = codegen(fn)
    result.funcs[fn.name] = f
    result.log f.name
    for instr in f.instrs:
      result.log instr

proc lookup(ctx: var Interpreter, name: string): var Value =
  # todo: also check global scope
  ctx.scopes.top().vars[name]

proc run(ctx: var Interpreter) =
  while ctx.scopes.len > 0:
    let cur = addr ctx.scopes.top()
    if cur.pc >= cur.instrs.len:
      discard ctx.scopes.pop()
      continue

    let instr = cur.instrs[cur.pc]
    cur.pc += 1
    ctx.log "running: ", instr
    case instr.kind
    of iValue:
      cur.vals.push(instr.val)
    of iLoad:
      cur.vals.push(ctx.lookup(instr.name))
    of iCall:
      var args: seq[Value]
      assert cur.vals.len >= instr.nargs
      for _ in 0..<instr.nargs:
        args.add cur.vals.pop()
      reverse(args)
      case instr.callee
      # builtins
      of "print":
        var line = ""
        for arg in args:
          line &= toString(arg)
        echo line
      of "+":
        assert instr.nargs == 2
        cur.vals.push(Value(kind: tyNum, num: args[0].num + args[1].num))
      else:
        # user funcs
        let fn = ctx.funcs[instr.callee]
        assert fn.args.len == instr.nargs
        var scope: Scope
        scope.instrs = fn.instrs
        for i in 0..<instr.nargs:
          scope.vars[fn.args[i]] = args[i]
        ctx.scopes.push(scope)
    of iReturn:
      let ret = cur.vals.pop()
      discard ctx.scopes.pop()
      ctx.scopes.top().vals.push(ret)

proc interpret(ast: Ast) =
  var ctx = newInterpreter(ast)
  var globalScope: Scope
  globalScope.instrs.add Instr(kind: iCall, callee: "_main", nargs: 0)
  ctx.scopes.push(globalScope)

  ctx.run()

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

  var ast = parseProgram(contents)
  pprint(log, ast)

  interpret(ast)

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
