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
      e = Call("gt", List(e, Num(20)))
      e = Id("foobar")
      e = Call("foobar", List(e, Id("var1"), Num(10), False))
      e = If(e, Num(12), Some(False))
    }
    it("should be possible to instantiate an If Expr without else body") {
      val e = If(True, False)
      e.cond shouldBe True
      e.thenExpr shouldBe False
      e.elseExpr shouldBe None
    }
  }

  describe("Decl") {
    it("should be possible to declare a function") {
      var d : Decl = Func("flag", Bool)
      d = Func("MAIN", Natural)
      d = Func("cond", Natural, List(Param("a", Bool), Param("b", Natural), Param("c", Natural)))
    }
  }

  describe("Def") {
    it("should be possible to define functions") {
      var d = Def(
        Func("cond", Natural, List(Param("c", Bool), Param("a", Natural), Param("b", Natural))),
        If(Id("c"), Id("a"), Some(Id("b")))
      )
      d = Def(
        Func("not", Bool, List(Param("val", Bool))),
        If(Id("val"), False, Some(True))
      )
    }
  }
}
