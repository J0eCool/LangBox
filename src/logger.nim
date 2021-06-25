import os

var logFile = stdout

proc closeLog*() =
  if logFile == nil: return
  logFile.close()
  logFile = nil

proc initLog*(tempDir: string) =
  if logFile != nil and logFile != stdout:
    closeLog()
  logFile = open(tempDir / "debug_log.txt", fmWrite)

proc silenceLog*() =
  logFile = nil

proc logKind*(kind: string, msgs: varargs[string, `$`]) =
  if logFile == nil: return
  logFile.write("[")
  logFile.write(kind)
  logFile.write("] ")
  for msg in msgs:
    logFile.write(msg)
  logFile.writeLine("")

proc logger*(kind: string): proc(msgs: varargs[string, `$`]) =
  return proc(msgs: varargs[string, `$`]) =
    logKind(kind, msgs)
