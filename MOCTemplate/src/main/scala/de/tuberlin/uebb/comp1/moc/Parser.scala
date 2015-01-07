/*
 * Copyright (c) 2013, TU Berlin
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the TU Berlin nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS """AS IS""" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL TU Berlin BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * */

package de.tuberlin.uebb.comp1.moc


/** Parser for μ-Opal-Compiler*/

object Parser {
  import AbstractSyntax._
  import de.tuberlin.uebb.comp1.parsing.ParserCombinators
  import de.tuberlin.uebb.comp1.parsing.ParserCombinators._

  type P[A] = Parser[Token, A]

  /**
    * Parser for builtins
    */
  def parseBuiltins(inp: List[Token]): Either[Diag, List[Def]] = {
    run(inp, parseBuiltins) match {
      case Fail(ParseErrorMessage(diag)) => Left(diag)
      case Okay(e) => Right(e)
      case _ => throw new RuntimeException("compiler bug")
    }
  }

  private def parseBuiltins : P[List[Def]] =
    parseBuiltin ~ parseBuiltins1 ~* makeBuiltins

  private def parseBuiltins1 : P[List[Def]] =
    parseBuiltins |^
    followBuiltins1 ~> eps(List()) |^
    fail("DEF")

  // DEF add(x:nat, y:nat):nat
  private def parseBuiltin =
    skip(DefT()) ~> parseDecl

  private def followBuiltins1 = peek((t: Token) => t == EofT())

  private def makeBuiltins(a: (Decl, List[Def])) = a match {
    case (d, lst) => Def(Global, d, Builtin(d.id)) :: lst
  }

  /**
   * Starts the parser
   *
   * @param inp the token sequence (result of scanner)
   * @param opts [[Options]] given as arguments to comiler
   * @return either an error message [[Diag]] or a list of definitions [[Def]]
   */
  def parse(inp: List[Token], opts: Options): Either[Diag, Prog] = {
    run(inp, parseProg) match {
      case Fail(ParseErrorMessage(diag)) => Left(diag)
      case Okay(e) => Right(e)
      case _ => throw new RuntimeException("compiler bug")
    }
  }

  private def parseProg : P[Prog] =
    parseDefs ~< parseEOT ~* (Prog(_))

  private def parseDefs : P[List[Def]] =
    parseDef ~ parseDefs1 ~* makeDefs

  private def parseDefs1 : P[List[Def]] =
    parseDefs |^
    followDefs1 ~> eps(List()) |^
    fail("DEF")

  private def followDefs1 = peek((t: Token) => t == EofT())

  private def parseDef : P[Def] =
    getLoc ~< skip(DefT()) ~ parseLhs ~< skip(DefAsT()) ~ parseExpr ~* makeDef

  private def parseLhs : P[Decl] =
    parseMain |^
    parseDecl |^
    fail("identifier", "MAIN")

  private def parseMain : P[Decl] =
    shift(MainT()) ~ shift(ColonT()) ~> parseType ~* (Decl("MAIN", _))

  private def parseDecl : P[Decl] =
    parseId ~< shift(OpenT()) ~ parseParams ~< shift(CloseT()) ~<
      shift(ColonT()) ~ parseType ~* makeDecl

  private def parseParams : P[List[Param]] =
    parseParams1 |^
    followParams ~> eps(List()) |^
    fail("identifier", ")")

  private def followParams = peek((t: Token) => t == CloseT())

  private def parseParams1 : P[List[Param]] =
    parseParam ~& parseParams2 |^
    fail("identifier")

  private def parseParams2(p: Param) : P[List[Param]] =
    shift(CommaT()) ~> parseParams1 ~* (p :: _) |^
    followParams2 ~> eps(List(p)) |^
    fail(",", ")")

  private def followParams2 = followParams

  private def parseParam : P[Param] =
    parseId ~< shift(ColonT()) ~ parseType ~* makeParam |^
    fail("identifier")

  private def parseEOT = skip(EofT())

  private def parseId : P[String] = shift(_.isVar, "identifier") ~* makeId

  private def parseType : P[Type] =
    shift(NatT()) ~> eps(Natural:Type) |^
    shift(BoolT()) ~> eps(Bool) |^
    fail("nat", "bool")

  private def parseExpr : P[Expr] =
    shift(_.isNum, "number") ~* makeNumber |^
    shift(TrueT()) ~> eps(True) |^
    shift(FalseT()) ~> eps(False) |^
    getLoc ~ parseId ~& parseExpr2 |^
    getLoc ~< shift(IfT()) ~ parseExpr ~< shift(ThenT()) ~ parseExpr ~& parseExpr1 |^
    fail("number", "true", "false", "identifier", "IF")

  private def parseExpr1(a : ((Position, Expr), Expr)) : P[Expr] =
    shift(ElseT()) ~> parseExpr ~< shift(FiT()) ~* (Some(_)) ~* makeIf(a) |^
    shift(FiT()) ~> eps(None) ~* makeIf(a) |^
    fail("ELSE", "FI")

  private def parseExpr2(tup: (Position, String)) : P[Expr] = tup match {
    case (pos, id) =>
      shift(OpenT()) ~> parseArgs ~< shift(CloseT()) ~* (Call(pos, id, _):Expr) |^
      followExpr2 ~> eps(Id(pos, id)) |^
      fail("THEN", "FI", "ELSE", ",", "DEF", "(", ")", "EOF")
  }

  private def followExpr2 = peek((t: Token) =>
    t == ThenT() || t == FiT() || t == ElseT() || t == CommaT() ||
    t == DefT() || t == EofT() || t == CloseT()
  )

  private def parseArgs : P[List[Expr]] =
    parseArgs1 |^
    followArgs ~> eps(Nil) |^
    fail("number", "true", "false", "identifier", "IF", ")")

  private def parseArgs1 : P[List[Expr]] =
    parseExpr ~& parseArgs2
    fail("number", "true", "false", "identifier", "IF")

  private def parseArgs2(e: Expr) : P[List[Expr]] =
    shift(CommaT()) ~> parseArgs1 ~* (e :: _) |^
    followArgs2 ~> eps(List(e)) |^
    fail(",", ")")

  private def followArgs = peek((t: Token) => t == CloseT())

  private def followArgs2 = followArgs

  // helper parser combinators
  private def skip(t: Token): P[Unit] = ParserCombinators.skip(
    _ == t,
    t1 => ParseErrorMessage(
      Diag("Expecting '"+t.toString()+"', but found '"+t1.toString()+ "' instead.", t1.getPosition))
  )

  private def shift(t: Token): P[Token] = shift(_ == t, t.toString())

  private def shift(pred: (Token => Boolean), str: String): P[Token] = ParserCombinators.shift(
    pred,
    t1 => ParseErrorMessage(
      Diag("Expecting '"+str+"', but found "+t1.toString()+ " instead.", t1.getPosition))
  )

  private def getLoc: P[Position] = peek() ~* (_.getPosition)

  // semantic actions
  private def makeDefs(a: (Def, List[Def])) = a match {
    case (d, lst) => d :: lst
  }

  private def makeDef(a: ((Position, Decl), Expr)) = a match {
    case ((p, d), e) => Def(p, d, e)
  }

  private def makeId(id: Token) = id match {
    case VarT(id) => id
  }

  private def makeDecl(a: ((String, List[Param]), Type)) : Decl = a match {
    case ((id, params), ty) => Decl(id, ty, params)
  }

  private def makeParam(a: (String, Type)) = a match {
    case (id, ty) => Param(id, ty)
  }

  private def makeNumber(t : Token) : Expr = t match {
    case NumT(value) => Num(value)
  }

  private def makeTrue(t : Token) : Expr = t match {
    case TrueT() => True
  }

  private def makeFalse(t : Token) : Expr = t match {
    case FalseT() => False
  }

  private def makeIf(a: ((Position, Expr), Expr)) : Option[Expr] => Expr = a match {
    case ((loc, cond), e1) => e2 => If(loc, cond, e1, e2)
  }

  // error handling
  private def fail[A](exp: String*): P[A] =
    ParserCombinators.fail(t => ParseErrorMessage(Diag(
      exp.length match {
        case 1 => "Expecting '"+exp.apply(0).toString+"' but found '"+t.toString()+"' instead."
        case _ => "Expecting one of "+exp.map("'"+_.toString+"'").mkString(", ")+" but found '"+t.toString()+"' instead."
      } , t.getPosition)))

  private case class ParseErrorMessage(diag: Diag) extends Message
}
