import unittest

import ../src/shiv_parser

proc test(input: string): seq[SExpr] =
  parse(input).elems

suite "Sexpr Parser":
  test "ToString":
    check:
      $(sa("foo")) == "foo"
      $(ss("foo", "bar")) == "(foo bar)"
      $(ss("foo", ss("bar", "baz"), "feef")) == "(foo (bar baz) feef)"
      # $(sa("foo bar")) == "`foo bar`"
      $(sa("\"foo bar\"")) == "\"foo bar\""
      $(ss()) == "()"
      $(ss(ss(), ss())) == "(() ())"
      $(sa("'f")) == "'f"
      # $(sa("foo\nbar")) == "foo\\nbar"
      $(sa("foo\\bar")) == "foo\\bar"

  test "Single atoms":
    check:
      test("foo") == @[ss("foo")]
      test("123") == @[ss("123")]

  test "String literals":
    check:
      test("\"foo\"") == @[ss("\"foo\"")]
      test("\"foo bar\"") == @[ss("\"foo bar\"")]
      test("\"\"") == @[ss("\"\"")]
      # test("\"\\\"foo\\\"\"") == @[(ss("\"\"foo\"\""))]
      # test("\"foo\\nbar\"") == @[(ss("\"foo\nbar\""))]
      # test("\"foo\\\\bar\"") == @[(ss("\"foo\\bar\""))]

  # test "Char literals":
  #   check:
  #     test("'a") == @[ss("'a")]
  #     test("'a 'b") == @[ss("'a", "'b")]
  #     test("'a'b'c") == @[ss("'a", "'b", "'c")]
  #     test("'\\n") == @[ss("'\n")]
  #     test("foo'a") == @[ss("foo", "'a")]

  test "Flat lists":
    check:
      test("") == newseq[SExpr]()
      test("()") == @[ss()]
      test("(foo)") == @[ss("foo")]
      test("(foo bar)") == @[ss("foo", "bar")]
      test("(foo bar baz)") == @[ss("foo", "bar", "baz")]

  test "Whitespace":
    check:
      test("( foo bar )") == @[ss("foo", "bar")]
      test("(\n  foo\n  bar\n)") == @[ss("foo", "bar")]

  test "Nested lists":
    check:
      test("( foo (bar seek) baz )") == @[ss(
        "foo", ss("bar", "seek"), "baz")]

  test "Comments":
    check:
      test("foo;") == @[ss("foo")]
      test("foo;\n") == @[ss("foo")]
      test(";foo") == newseq[SExpr]()
      test("; foo") == newseq[SExpr]()
      test("foo ; bar baz") == @[ss("foo")]
      test("foo;bar") == @[ss("foo")]
      test("foo ; bar\nbaz") == @[ss("foo"), ss("baz")]


suite "Flat-sexpr Parser":
  test "Single line":
    check:
      test("foo") == @[ss("foo")]
      test("foo bar baz") == @[ss("foo", "bar", "baz")]
      test("  foo  ") == @[ss("foo")]

  test "Newlines":
    check:
      test("foo\nbar") == @[ss("foo"), ss("bar")]
      test("foo\nbar\n") == @[ss("foo"), ss("bar")]
      test("foo bar\nbar bar baz\n") == @[
        ss("foo", "bar"),
        ss("bar", "bar", "baz"),
      ]
      test("\nfoo\n\n\nbar\n") == @[ss("foo"), ss("bar")]

  test "Sub-sexprs":
    check:
      test("foo (bar baz)") == @[ss("foo", ss("bar", "baz"))]
      test("(foo bar) baz") == @[ss(ss("foo", "bar"), "baz")]
      test("foo (bar\n  baz)") == @[ss("foo", ss("bar", "baz"))]
      test("foo\n(bar baz)") == @[ss("foo"), ss("bar", "baz")]
      test("(foo bar)\nbaz") == @[ss("foo", "bar"), ss("baz")]
      test("foo(bar)") == @[ss("foo", ss("bar"))]

  test "Sub-flats":
    check:
      test("{}") == @[ss()]
      test("{foo bar}") == @[ss(ss("foo", "bar"))]
      test("{\nfoo bar\n}") == @[ss(ss("foo", "bar"))]
      test("{\nfoo\nbar\n}") == @[ss(ss("foo"), ss("bar"))]
      test("{\n  foo\n  bar\n}") == @[ss(ss("foo"), ss("bar"))]
      test("foo{bar}") == @[ss("foo", ss(ss("bar")))]

  test "If stmt":
    check:
      test("""
        if (x > 5) {
          print x
          print "hi"
        }
        if (x < 10) {
          print x
        } else {
          print "Too big"
        }
      """) == @[
        ss("if", ss("x", ">", "5"), ss(
          ss("print", "x"),
          ss("print", "\"hi\""),
        )),
        ss("if", ss("x", "<", "10"), ss(
          ss("print", "x"),
        ), "else", ss(
          ss("print", "\"Too big\""),
        ))
      ]

  test "Empty if-clauses":
    check:
      test("if (foo) {} else { print bar }") == @[
        ss("if", ss("foo"), ss(), "else", ss(ss("print", "bar")))]
      test("if (foo) {} else { }") == @[
        ss("if", ss("foo"), ss(), "else", ss())]
      test("if (foo) () else ()") == @[
        ss("if", ss("foo"), ss(), "else", ss())]
      test("foo () (bar baz)\n") == @[ss("foo", ss(), ss("bar", "baz"))]
      test("foo {} {bar baz}\n") == @[ss("foo", ss(), ss(ss("bar", "baz")))]
      test("foo () ()\n") == @[ss("foo", ss(), ss())]
      test("fee {\nfoo {} {\nbar baz\n}}\n") == @[ss("fee", ss(ss("foo", ss(), ss(ss("bar", "baz")))))]

  test "Array literals":
    check:
      test("[array literal]") == @[ss("__list", "array", "literal")]
      test("[foo [bar baz]]") == @[ss("__list", "foo", ss("__list", "bar", "baz"))]
      test("[]") == @[ss("__list")]
      test("[[foo bar]]") == @[ss("__list", ss("__list", "foo", "bar"))]

  test "Array indexing":
    check:
      test("foo[idx]") == @[ss("__index", "foo", "idx")]
      test("foo[bar[baz]]") == @[ss("__index", "foo", ss("__index", "bar", "baz"))]
      test("foo[bar baz]") == @[ss("__index", "foo", ss("bar", "baz"))]
      test("foo[]") == @[ss("__index", "foo", ss())]
      test("foo[1][2]") == @[ss("__index", ss("__index", "foo", "1"), "2")]
      test("foo[[x]]") == @[ss("__index", "foo", ss("__list", "x"))]
      test("(foo)[2]") == @[ss("__index", ss("foo"), "2")]
      test("\"foo\"[2]") == @[ss("__index", "\"foo\"", "2")]
      test("[foo][bar]") == @[ss("__index", ss("__list", "foo"), "bar")]
      test("foo [bar]") == @[ss("foo", ss("__list", "bar"))]
      test("[foo bar[baz]]") == @[ss("__list", "foo", ss("__index", "bar", "baz"))]

  test "Split atoms on symbols":
    check:
      test("foo + bar") == @[ss("foo", "+", "bar")]
      test("foo+bar") == @[ss("foo", "+", "bar")]
      test("foo +bar") == @[ss("foo", "+", "bar")]
      test("a.b") == @[ss("a", ".", "b")]

      # Multiple symbols are one atom
      test("foo->bar") == @[ss("foo", "->", "bar")]
      test("<=~~*") == @[ss("<=~~*")]

  test "Float literals":
    check:
      test("1.0") == @[ss("1.0")]
      test("0.0") == @[ss("0.0")]
      test("2.") == @[ss("2", ".")]
      test("12.3") == @[ss("12.3")]
      test("1.2.3") == @[ss("1.2", ".", "3")]
      test("1f2") == @[ss("1", "f2")]
      test("f123.45") == @[ss("f123", ".", "45")]
      test("f 0.2 0.4 10.2") == @[ss("f", "0.2", "0.4", "10.2")]
      test("[1.0 0.2 3 4 0.5]") == @[ss("__list", "1.0", "0.2", "3", "4", "0.5")]
      test("1.foo") == @[ss("1", ".", "foo")]
      test("1.2.foo") == @[ss("1.2", ".", "foo")]
      test("1.+.2") == @[ss("1", ".+.", "2")]

  test "Negative numeric literals":
    check:
      test("-1") == @[ss("-1")]
      test("1-") == @[ss("1", "-")]
      test("1 -") == @[ss("1", "-")]
      test("-10") == @[ss("-10")]
      test("-x") == @[ss("-", "x")]
      test("2-1") == @[ss("2", "-", "1")]
      test("a-10") == @[ss("a", "-", "10")]
      test("f -3") == @[ss("f", "-3")]
      test("(-1)") == @[ss("-1")]
      test("(1-)") == @[ss("1", "-")]
      test("-12.34") == @[ss("-12.34")]
      test("-1 - -2") == @[ss("-1", "-", "-2")]
      test("-1 --2") == @[ss("-1", "--", "2")]
      test("[1 2 -3 4 -5]") == @[ss("__list", "1", "2", "-3", "4", "-5")]
