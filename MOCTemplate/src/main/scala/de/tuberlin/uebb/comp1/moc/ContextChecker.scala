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

/** The μ-Opal context checker */

object ContextChecker {
  import AbstractSyntax._

  type SymTab = Map[String, Def]
  case class Ctx(tab: SymTab, errs: Array[Diag]) {
    def push(diag: Diag) = Ctx(tab, errs :+ diag)
    def push(sym: (String, Def)) = sym match {
      case (id, d) => Ctx(tab + (id -> d), errs)
    }
  }

  type Params = Map[String, Type]

  /**
   * Starts context check for μ-Opal
   * @param prog Complete program [[Prog]]
   * @param opts [[Options]] given as arguments to compiler
   * @return A list of error messages
    */
  def check(prog: Prog, opts: Options): Option[List[Diag]] = {
    val errs = runChecks(prog.defs, buildSymTab(prog.defs))
    if (errs.isEmpty) None else Some(errs.toList)
  }

  def runChecks(defs: List[Def], ctx: Ctx): Array[Diag] = {
    val checks = checkMain _ andThen (defs.foldLeft(_)(checkDef))
    checks(ctx).errs
  }

  def checkMain(c: Ctx): Ctx =
    if (c.tab contains "MAIN") c
    else c.push(Diag("DEF MAIN not found", Global))

  def checkDef(c: Ctx, d: Def): Ctx = {
    def duplicate(name: String) =
      Diag("Duplicate param '"+name+"' in DEF "+d.decl.id+".", d.loc);

    val (params, errs) = d.decl.params.
      foldLeft((Map[String, Type](), c.errs))((acc, p) => acc match {
        case (params, errs) =>
          if (params contains p.id) (params, errs :+ duplicate(p.id))
          else (params + (p.id -> p.ty), errs)
      })

    typeCheck(d, Ctx(c.tab, errs), params)
  }

  def typeCheck(d: Def, c: Ctx, params: Params) = {
    def mismatch(ty: Type) =
      Diag("The result type of DEF '"++d.decl.id+"' is "+ty+" but was declared as "+d.decl.ty+".", d.loc)

    val (ty, c2) = exprType(d.expr, c, params)
    ty match {
      case TypeError | d.decl.ty => c2
      case _ => c2.push(mismatch(ty))
    }
  }

  def exprType(e: Expr, c: Ctx, params: Params): (Type, Ctx) = {
    e match {
    case Num(v) => (Natural, c)
    case True | False => (Bool, c)
    case Id(loc, id) =>

        def undefined() =
          Diag("Undefined variable '"+id+"'.", loc)

        if (!(params contains id))
          (TypeError, c.push(undefined()))
        else
          (params(id), c)

    case Call(loc, id, args) =>
        def undefined() =
          Diag("Undefined DEF '"+id+"'.", loc)

        def argCountMismatch(nargs: Int, nparams: Int) =
          Diag("DEF '"+id+"' expects "+nparams+" arguments, but "
            +nargs+" were given.", loc)

        def argTypeMismatch(arg: Type, par: Param, pos: Int) =
          Diag("Can't pass a "+arg+" as "+ordinal(1+pos)+" argument to '"+
            par.id+":"+par.ty+"' of DEF '"+id+"'.", loc)

        if (!(c.tab contains id))
          (TypeError, c.push(undefined()))
        else {
          var (argTypes, c2) = args.foldLeft((Array[Type](), c))((acc, arg) => acc match {
            case (argTypes, c2) => exprType(arg, c2, params) match {
              case (ty, c3) => (argTypes :+ ty, c3)
            }
          })

          val d = c.tab(id)
          val nargs = argTypes.length
          val nparams = d.decl.params.length

          val c3 = if (nargs == nparams) c2
          else c2.push(argCountMismatch(nargs, nparams))

          val c4 = argTypes.zip(d.decl.params).
            zipWithIndex.foldLeft(c3)((c, elem) => elem match {
            case ((arg, par), pos) =>
              if (arg == par.ty) c
              else c.push(argTypeMismatch(arg, par, pos))
          })
          // return the function type regardless of errors
          // so to report most errors in a single pass
          (d.decl.ty, c4)
        }

    case If(loc, cond, e1, e2) =>
        def condMustBeBool(tc: Type) =
          Diag("IF condition must be a bool, not "+tc+".", loc)

        def typeMismatch(t1: Type, t2: Type) =
          Diag("Types for then and else branches differ, "+t1+" vs. "+t2+".", loc)

        // TODO: should really use a state monad here
        val (tc, c1) = exprType(cond, c, params)
        val (t1, c2) = exprType(e1, c1, params)
        val (t2, c3) = e2 match {
          case None => (t1, c2) // fake same type
          case Some(e) => exprType(e, c2, params)
        }

        val c4 = if (tc == Bool) c3
        else c3.push(condMustBeBool(tc))

        val c5 = if (t1 == t2) c4
        else c4.push(typeMismatch(t1, t2))

        // return the most reasonable type regardless of
        // errors so to report most errors in a single pass
        (t1, c5)

    case Builtin(id) => (c.tab(id).decl.ty, c)
    }
 }

  def buildSymTab(defs: List[Def]) = {
    def duplicate(d: Def, loc: Position) = {
      var id = d.decl.id
      Diag("DEF '"+id+"' already defined at '"+loc+"'.", d.loc)
    }

    def build(defs: List[Def], c: Ctx): Ctx = {
      defs match {
        case d :: ds =>
          var id = d.decl.id
          if (c.tab contains id) build(ds, c.push(duplicate(d, c.tab(id).loc)))
          else build(ds, c.push((id -> d)))
        case Nil => c
      }
    }
    build(defs, Ctx(builtins, Array()))
  }

  private def builtins : SymTab = {
    object DefaultOptions extends Options(false, false, false, "default")

    var str = """
    DEF add(X: nat, Y: nat): nat DEF sub(X: nat, Y: nat): nat
    DEF mul(X: nat, Y: nat): nat DEF div(X: nat, Y: nat): nat
    DEF eq(X: nat, Y: nat): bool DEF lt(X: nat, Y: nat): bool
    DEF and(X: bool, Y: bool): bool DEF or(X: bool, Y: bool): bool
    DEF not(X: bool): bool
    """

    // parse the builtin declarations, and return them as symbol table
    Scanner.scan(str, DefaultOptions).fold(Left(_), Parser.parseBuiltins(_)) match {
      case Left(diag) => throw new Exception("failed to parse builtins "+diag.msg)
      case Right(defs) => (defs map (d => (d.decl.id, d))).toMap
    }
  }

  private def ordinal(num: Int): String = num % 10 match {
    case 1 if num != 11 => num + "st"
    case 2 if num != 12 => num + "nd"
    case 3 if num != 13 => num + "rd"
    case _ => num + "th"
  }
}
