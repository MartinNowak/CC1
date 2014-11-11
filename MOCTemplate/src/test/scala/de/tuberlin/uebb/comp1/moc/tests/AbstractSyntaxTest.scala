/*
 * Copyright (c) 2014, Martin Nowak
 * */

package de.tuberlin.uebb.comp1.moc.tests

import org.scalatest._
import de.tuberlin.uebb.comp1.moc._

class AbstractSyntaxTest extends FunSpec with Matchers {
  import AbstractSyntax._

  describe("Type") {
    it("should be matchable") {
      var t : Type = Bool
      (t match { case Bool => true case _ => false }) shouldBe true
      t = Natural
      (t match { case Bool => true case _ => false }) shouldBe false
    }
  }

  describe("Expr") {
    it("should be possible to build expression graphs") {
      var e : Expr = True
      e = False
      e = Num(10)
      e = Call("gt", Array(e, Num(20)))
      e = Id("foobar")
      e = Call("foobar", Array(e, Id("var1"), Num(10), False))
      e = If(e, Num(12), False)
    }
    it("should be possible to instantiate an If Expr without else body") {
      val e = If(True, False)
      e.cond shouldBe True
      e.thenExpr shouldBe False
      e.elseExpr shouldBe null
    }
  }

  describe("Decl") {
    it("should be possible to declare a function") {
      var d : Decl = Val("flag", Bool)
      d = Main(Natural)
      d = Func("cond", Natural, Array(Param("a", Bool), Param("b", Natural), Param("c", Natural)))
    }
  }

  describe("Def") {
    it("should be possible to define functions") {
      var d = Def(
        Func("cond", Natural, Array(Param("c", Bool), Param("a", Natural), Param("b", Natural))),
        If(Id("c"), Id("a"), Id("b"))
      )
      d = Def(
        Func("not", Bool, Array(Param("val", Bool))),
        If(Id("val"), False, True)
      )
    }
  }
}

