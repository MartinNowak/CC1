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
      val loc = Global
      var e : Expr = True
      e = False
      e = Num(10)
      e = Call(loc, "gt", List(e, Num(20)))
      e = Id(loc, "foobar")
      e = Call(loc, "foobar", List(e, Id(loc, "var1"), Num(10), False))
      e = If(loc, e, Num(12), Some(False))
    }
    it("should be possible to instantiate an If Expr without else body") {
      val e = If(Global, True, False)
      e.cond shouldBe True
      e.thenExpr shouldBe False
      e.elseExpr shouldBe None
    }
  }

  describe("Decl") {
    it("should be possible to declare a function") {
      var d : Decl = Decl("flag", Bool)
      d = Decl("MAIN", Natural)
      d = Decl("cond", Natural, List(Param("a", Bool), Param("b", Natural), Param("c", Natural)))
    }
  }

  describe("Def") {
    it("should be possible to define functions") {
      val loc = Global
      var d = Def(
        loc,
        Decl("cond", Natural, List(Param("c", Bool), Param("a", Natural), Param("b", Natural))),
        If(loc, Id(loc, "c"), Id(loc, "a"), Some(Id(loc, "b")))
      )
      d = Def(
        loc,
        Decl("not", Bool, List(Param("val", Bool))),
        If(loc, Id(loc, "val"), False, Some(True))
      )
    }
  }
}
