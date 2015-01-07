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
  case class Ctx(tab: SymTab, errs: Array[Diag])

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
    else Ctx(c.tab, c.errs :+ Diag("DEF MAIN not found", Global))

  def checkDef(c: Ctx, d: Def): Ctx = {
    def duplicate(name: String) =
      Diag("Duplicate param '"+name+"' in DEF "+d.decl.id+".", d.pos);

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
      Diag("The result type of DEF '"++d.decl.id+"' is "+ty+" but was declared as "+d.decl.ty+".", d.pos)

    val (ty, c2) = exprType(d.expr, c, params)
    ty match {
      case TypeError | d.decl.ty => c2
      case _ => Ctx(c2.tab, c2.errs :+ mismatch(ty))
    }
  }

  def exprType(e: Expr, c: Ctx, params: Params): (Type, Ctx) = {
    def typeError(c :Ctx, msg: String) =
      (TypeError, Ctx(c.tab, c.errs :+ Diag(msg, Global))) // TODO: e.pos

    e match {
    case Num(v) => (Natural, c)
    case True | False => (Bool, c)
    case Id(id) =>
        if (!(params contains id))
          typeError(c, "Undefined variable '"+id+"'.")
        else
          (params(id), c)

    case Call(id, args) =>
        def argCountMismatch(nargs: Int, nparams: Int) =
          Diag("DEF '"+id+"' expects "+nparams+" arguments, but "
            +nargs+" were given.", Global)

        def argTypeMismatch(arg: Type, par: Param, pos: Int) =
          Diag("Can't pass a "+arg+" as "+ordinal(1+pos)+" argument to '"+
            par.id+":"+par.ty+"' of DEF '"+id+"'.", Global)

        if (!(c.tab contains id))
          typeError(c, "Undefined DEF '"+id+"'.")
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
          else Ctx(c2.tab, c2.errs :+ argCountMismatch(nargs, nparams))

          val c4 = argTypes.zip(d.decl.params).
            zipWithIndex.foldLeft(c3)((c, elem) => elem match {
            case ((arg, par), pos) =>
              if (arg == par.ty) c
              else Ctx(c.tab, c.errs :+ argTypeMismatch(arg, par, pos))
          })
          // return the function type regardless of errors
          // so to report most errors in a single pass
          (d.decl.ty, c4)
        }

    case If(cond, e1, e2) =>
        def condMustBeBool(tc: Type) =
          Diag("IF condition must be a bool, not "+tc+".", Global)

        def typeMismatch(t1: Type, t2: Type) =
          Diag("Types for then and else branches differ, "+t1+" vs. "+t2+".", Global)

        // TODO: should really use a state monad here
        val (tc, c1) = exprType(cond, c, params)
        val (t1, c2) = exprType(e1, c1, params)
        val (t2, c3) = e2 match {
          case None => (t1, c2) // fake same type
          case Some(e) => exprType(e, c2, params)
        }

        val c4 = if (tc == Bool) c3
        else Ctx(c3.tab, c3.errs :+ condMustBeBool(tc))

        val c5 = if (t1 == t2) c4
        else Ctx(c4.tab, c4.errs :+ typeMismatch(t1, t2))

        // return the most reasonable type regardless of
        // errors so to report most errors in a single pass
        (t1, c5)

    case Builtin(id) => (c.tab(id).decl.ty, c)
    }
 }

  private def buildSymTab(defs: List[Def]) = {
    def duplicate(d: Def, tab: SymTab) = {
      var id = d.decl.id
      Diag("DEF '"+id+"' already defined at '"+tab(id).pos+"'.", d.pos)
    }

    def build(defs: List[Def], tab: SymTab, errs: Array[Diag]): Ctx = {
      defs match {
        case d :: ds =>
          var id = d.decl.id
          if (tab contains id) build(ds, tab, errs :+ duplicate(d, tab))
          else build(ds, tab + (id -> d), errs)
        case Nil => Ctx(tab, errs)
      }
    }
    build(defs, builtins, Array())
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
