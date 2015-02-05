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

import de.tuberlin.uebb.comp1.covm.instructions.{Call => CallInst,_}

/** μ-Opal code generator */

object CodeGenerator {
  import AbstractSyntax._
  import scala.collection.mutable.ArrayBuffer
  import scala.collection.mutable.Map

  type Code = List[Instruction]
  /**
   * Generates [[Code]] for the given list of definitions
   * @param prog the context checked program [[Prog]]
   * @param opts [[Options]] given as arguments to compiler
   * @return Either an error message [[Diag]] or the code for the stack machine
   */
  def compile(prog: Prog, opts: Options): Either[Diag, Code] = {
    labelCount = 0
    var insts = new ArrayBuffer[Instruction];
    // entry point
    entryPoint(insts)
    // gen all DEFs
    prog.defs.foreach(gen(insts, _))
    // remove labels and compute jump targets
    insts = computeLabels(insts)
    Right(insts.toList)
  }

  def entryPoint(insts:ArrayBuffer[Instruction]) = {
    insts += PushAddr(LabelledAddress("MAIN"))
    insts += CallInst
    insts += Stop
  }

  def gen(insts:ArrayBuffer[Instruction], d:Def):Unit = {
    // label for function entry
    insts += Label(d.decl.id)
    sp2bp = d.decl.params.length
    // body
    gen(insts, d.expr, d.decl.params)
    // epilog
    insts += Ret
  }

  def gen(insts:ArrayBuffer[Instruction], e:Expr, params:List[Param]):Unit = e match {
    case True => insts += PushInt(1); sp2bp += 1
    case False => insts += PushInt(0); sp2bp += 1
    case Num(num) => insts += PushInt(num); sp2bp += 1

    // function call
    case Call(loc, id, args) => {
      // push arguments
      args.foreach(gen(insts, _, params))

      // handle builtins
      id match {
        // arithmethic
        // TODO: overflow checking
        case "add" => insts += Add; sp2bp -= 1
        case "sub" =>
          val underflow = mkLabel
          val passed = mkLabel
          insts += Sub
          insts += Push(0)
          insts += Jlt(LabelledAddress(underflow))
          insts += Jmp(LabelledAddress(passed))
          insts += Label(underflow)
          insts += Abort("Integer underflow")
          insts += Label(passed)
          sp2bp -= 1
        case "mul" => insts += Mul; sp2bp -= 1
        case "div" => insts += Div; sp2bp -= 1

        // comparison
        case "eq" =>
          // a - b == 0
          val eq_true = mkLabel
          val eq_end = mkLabel
          insts += Sub
          insts += Jz(LabelledAddress(eq_true))
          insts += PushInt(0)
          insts += Jmp(LabelledAddress(eq_end))
          insts += Label(eq_true)
          insts += PushInt(1)
          insts += Label(eq_end)
          sp2bp -= 1

        case "lt" =>
          // a - b < 0
          val lt_true = mkLabel
          val lt_end = mkLabel
          insts += Sub
          insts += Jlt(LabelledAddress(lt_true))
          insts += PushInt(0)
          insts += Jmp(LabelledAddress(lt_end))
          insts += Label(lt_true)
          insts += PushInt(1)
          insts += Label(lt_end)
          sp2bp -= 1

        // boolean
        case "and" =>
          // a + b - 2 == 0
          val and_true = mkLabel
          val and_end = mkLabel
          insts += Add
          insts += PushInt(2)
          insts += Sub
          insts += Jz(LabelledAddress(and_true))
          insts += PushInt(0)
          insts += Jmp(LabelledAddress(and_end))
          insts += Label(and_true)
          insts += PushInt(1)
          insts += Label(and_end)
          sp2bp -= 1

        case "or" =>
          // a + b != 0
          val or_false = mkLabel
          val or_end = mkLabel
          insts += Add
          insts += Jz(LabelledAddress(or_false))
          insts += PushInt(1)
          insts += Jmp(LabelledAddress(or_end))
          insts += Label(or_false)
          insts += PushInt(0)
          insts += Label(or_end)
          sp2bp -= 1

        case "not" =>
          // 1 - a
          insts += PushInt(1)
          insts += Swap
          insts += Sub

        // normal function call
        case id =>
          insts += PushAddr(LabelledAddress(id))
          insts += CallInst
          insts += Slide(args.length)
          sp2bp += 1
      }
    }

    case If(loc, cond, thenE, elseE) => {
      val if_false = mkLabel
      val if_end = mkLabel
      val off = sp2bp
      gen(insts, cond, params)
      insts += Jz(LabelledAddress(if_false))
      sp2bp = off
      gen(insts, thenE, params)
      sp2bp = off
      insts += Jmp(LabelledAddress(if_end))
      insts += Label(if_false)
      elseE match {
        case Some(e) => gen(insts, e, params)
        case None => insts += Abort("missing else branch")
      }
      insts += Label(if_end)
      sp2bp = off + 1
    }

    case Id(loc, id) => {
      insts += Push(sp2bp - params.map(_.id).indexOf(id))
      sp2bp += 1
    }

    case Builtin(_) => throw new RuntimeException("bug")
  }

  def computeLabels(insts:ArrayBuffer[Instruction]) = {
    var addrs = Map[String, Pointer]()
    // remove all labels and save the addresses
    var pos = 0
    for (i <- 0 until insts.length) {
      insts(i) match {
        case Label(id) => addrs += ((id, Pointer(pos)))
        case _ => insts(pos) = insts(i); pos += 1
      }
    }
    // shorten the array
    insts.slice(0, pos).map(_ match {
      case Jmp(LabelledAddress(id)) => Jmp(addrs(id))
      case Jz(LabelledAddress(id)) => Jz(addrs(id))
      case Jlt(LabelledAddress(id)) => Jlt(addrs(id))
      case Jgt(LabelledAddress(id)) => Jgt(addrs(id))
      case PushAddr(LabelledAddress(id)) => PushAddr(addrs(id))
      case i => i
    })
  }

  var sp2bp = 0
  var labelCount = 0

  def mkLabel() = {
    val label = "%"+labelCount.toString
    labelCount += 1
    label
  }
}
