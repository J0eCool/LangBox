# Driver for Shenk mini-language

import os
import sequtils
import strutils

import logger
import shiv_lexer
import shiv_parser

#-------------------------------------------------------------------------------
# Shenk AST

type
  ExprKind = enum
    exLiteral
    exIdentifier
    exCall
  Expr = object
    case kind: ExprKind
    of exLiteral:
      val: float
    of exIdentifier:
      name: string
    of exCall:
      callee: string
      args: seq[Expr]
  Ast = object
    exprs: seq[Expr]

#-------------------------------------------------------------------------------
# Shenk parser

proc parseExpr*(sexpr: SExpr): Expr =
  if sexpr.kind == sAtom:
    if sexpr.name[0].isDigit():
      Expr(kind: exLiteral, val: parseFloat(sexpr.name))
    else:
      Expr(kind: exIdentifier, name: sexpr.name)
  else:
    assert sexpr.elems[0].kind == sAtom
    Expr(kind: exCall, callee: sexpr.elems[0].name, args: sexpr.elems[1..^1].mapIt(parseExpr(it)))

proc parse*(topLevel: SExpr): Ast =
  assert topLevel.kind == sList
  for sexpr in topLevel.elems:
    assert sexpr.kind == sList
    assert sexpr.elems.len != 0
    let head = sexpr.elems[0]
    if head == sa("fn"):
      discard
    else:
      result.exprs.add parseExpr(sexpr)


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
  let log = logger("shivc main")
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

  log "Parsing Sexprs..."
  var sexpr = shiv_parser.parse(contents)
  if opt.debug:
    let file = open(tempDir / "debug_parse_sexpr.shv", fmWrite)
    for elem in sexpr.elems:
      file.writeLine(pretty(elem))
    file.close()

  var ast = parse(sexpr)
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
