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

  /**
   * Starts context check for μ-Opal
   * @param prog Complete program [[Prog]]
   * @param opts [[Options]] given as arguments to compiler
   * @return A list of error messages
    */
  def check(prog: Prog, opts: Options): Option[List[Diag]] = {
    buildSymTab(prog.defs) match {
      case (symTab, Some(errs)) => Some(errs.toList)
      case (symTab, None) => checkDefs(prog.defs, symTab).map(_.toList)
    }
  }

  def checkDefs(defs: List[Def], symTab: SymTab): Option[Array[Diag]] = {
    ???
  }

  private def buildSymTab(defs: List[Def]) = {
    def duplicate(d: Def, tab: SymTab) = {
      var id = d.decl.id
      Diag("'"+id+"' already defined at '"+tab(id).pos+"'.", d.pos)
    }

    def build(defs: List[Def], tab: SymTab, errs: Array[Diag]): (SymTab, Option[Array[Diag]]) = {
      defs match {
        case d :: ds =>
          var id = d.decl.id
          if (tab contains id) build(ds, tab, errs :+ duplicate(d, tab))
          else build(ds, tab + (id -> d), errs)
        case Nil => (tab, Some(errs))
      }
    }
    build(defs, builtins, Array())
  }

  object DefaultOptions extends Options(false, false, false, "default")

  private def builtins : SymTab = {
    var str = """
    DEF add(X: nat, Y: nat): nat DEF sub(X: nat, Y: nat): nat
    DEF mul(X: nat, Y: nat): nat DEF div(X: nat, Y: nat): nat
    DEF eq(X: nat, Y: nat): bool DEF lt(X: nat, Y: nat): bool
    DEF and(X: bool, Y: bool): bool DEF or(X: bool, Y: bool): bool
    DEF not(X: bool): bool
    """

    Scanner.scan(str, DefaultOptions).fold(Left(_), Parser.parseBuiltins(_)) match {
      case Left(diag) => throw new Exception("failed to parse builtins "+diag.msg)
      case Right(defs) => (defs map (d => (d.decl.id, d))).toMap
    }
  }
}
