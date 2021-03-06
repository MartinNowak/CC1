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

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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
 */

package de.tuberlin.uebb.comp1.moc

/** Abstract syntax of μ-Opal */

object AbstractSyntax {
  /** A complete program */
  case class Prog(defs: List[Def])

  case class Def(loc: Position, decl: Decl, expr: Expr)

  case class Decl(id: String, ty: Type, params: List[Param]=Nil)

  case class Param(id: String, ty: Type)

  sealed abstract class Expr
  case class Num(num: Int) extends Expr
  case object True extends Expr
  case object False extends Expr
  case class Id(loc: Position, id: String) extends Expr
  case class Call(loc: Position, id: String, args: List[Expr]) extends Expr
  case class If(loc: Position, cond: Expr, thenExpr: Expr, elseExpr: Option[Expr]=None) extends Expr
  case class Builtin(name: String) extends Expr

  sealed abstract class Type
  case object Natural extends Type {
    override def toString = "nat"
  }
  case object Bool extends Type {
    override def toString = "bool"
  }
  case object TypeError extends Type {
    override def toString = "error"
  }
}
