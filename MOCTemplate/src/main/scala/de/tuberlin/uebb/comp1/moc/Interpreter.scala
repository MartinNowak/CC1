// Copyright (c) 2015 - , Martin Nowak
package de.tuberlin.uebb.comp1.moc

/** μ-Opal interpreter */

object Interpreter {
  import AbstractSyntax._
  import ContextChecker.{buildSymTab,SymTab}
  import scala.collection.mutable.ArrayBuffer

  /**
   * Interprets the given list of definitions [[Def]] starting with main function
   * @param prog the context checked program [[Prog]]
   * @param opts [[Options]] given as arguments to compiler
   * @return Either an error message [[Diag]] or a [[Value]]
   */
  def interpret(prog: Prog, opts: Options): Either[Diag, Value] = {
    stack.clear()
    val symtab = buildSymTab(prog.defs).tab
    try {
      eval(symtab("MAIN").expr, symtab) match {
        case Undef(diag) => Left(diag)
        case v => Right(v)
      }
    } catch {
      case e: InterpreterException => Left(Diag(e.getMessage(), e.loc))
      case e: StackOverflowError =>
        val func = stack._scopes.last
        Left(Diag("Stack overflow in "+func.decl.id+".", func.loc))
    }
  }

  def eval(e: Expr, tab: SymTab): Value = e match {
    case True => BoolV(true)
    case False => BoolV(false)
    case Num(num) => NumV(num)
    // arithmethic
    case Builtin("add") => add(numAt(0), numAt(1))
    case Builtin("sub") => sub(numAt(0), numAt(1))
    case Builtin("mul") => mul(numAt(0), numAt(1))
    case Builtin("div") => div(numAt(0), numAt(1))
    // comparison
    case Builtin("eq") => BoolV(numAt(0) == numAt(1))
    case Builtin("lt") => BoolV(numAt(0) < numAt(1))
    // boolean
    case Builtin("and") => BoolV(boolAt(0) && boolAt(1))
    case Builtin("or") => BoolV(boolAt(0) || boolAt(1))
    case Builtin("not") => BoolV(!boolAt(0))
    case Builtin(_) => throw new RuntimeException("bug")
    // variable reference
    case Id(loc, id) => stack(id)
    // function call
    case Call(loc, id, args) =>
      try {
        val func = tab(id)
        // eval arguments in old scope
        val vals = args.map(eval(_, tab))
        // push frame and scope
        stack.pushFunc(func)
        // push arguments
        vals.foreach(stack.push(_))
        // eval body in new scope
        val res = eval(func.expr, tab)
        // pop frame and scope
        stack.popFunc()
        // return result
        res
      } catch {
        // attach location of call
        case e: InterpreterException =>
          if (e.loc == Global)
            e.loc = loc;
          throw e
      }
    // control flow
    case If(loc, cond, thenE, elseE) =>
      eval(cond, tab) match {
        case BoolV(true) => eval(thenE, tab)
        case BoolV(false) => elseE match {
          case Some(e) => eval(e, tab)
          case None => Undef(Diag("Else branch not defined.", loc))
        }
        case _ => throw new RuntimeException("bug")
      }
  }

  private def numAt(idx: Int): Int = valAt(idx) match {
    case NumV(v) => v
    case _ => throw new RuntimeException("bug")
  }

  private def boolAt(idx: Int): Boolean = valAt(idx) match {
    case BoolV(v) => v
    case _ => throw new RuntimeException("bug")
  }

  private def valAt(idx: Int): Value = stack(idx)

  // arguments are in reverse order because of the stack
  def add(l: Long, r: Long): Value = checkOverflow(l + r, l, r, "+")
  def sub(l: Long, r: Long): Value = checkOverflow(l - r, l, r, "-")
  def mul(l: Long, r: Long): Value = checkOverflow(l * r, l, r, "*")
  def div(l: Int, r: Int): Value = {
    if (r == 0)
      throw new InterpreterException("Divide by zero error "+l+" / "+r+".")
    else
      NumV(l / r)
  }

  private def checkOverflow(res: Long, a: Long, b: Long, op: String): Value = {
    if (res < 0)
      throw new InterpreterException("Underflow while computing "+a+" "+op+" "+b+".")
    else if (res > Int.MaxValue)
      throw new InterpreterException("Overflow while computing "+a+" "+op+" "+b+".")
    else
      NumV(res.toInt)
  }

  private class Stack {
    var _values = new ArrayBuffer[Value]
    var _frames = new ArrayBuffer[Int]
    var _scopes = new ArrayBuffer[Def]

    def push(v: Value) = _values += v

    def pushFunc(func: Def) = {
      _frames += _values.length
      _scopes += func
    }

    def popFunc() = {
      _values = _values.take(_frames.last)
      _frames = _frames.dropRight(1)
      _scopes = _scopes.dropRight(1)
    }

    def clear() = {
      _values.clear()
      _frames.clear()
      _scopes.clear()
    }

    def apply(idx: Int): Value = _values(_frames.last + idx)
    def apply(id: String): Value = apply(_scopes.last.decl.params.map(_.id).indexOf(id))
  }

  private class InterpreterException(msg: String) extends Exception(msg) {
    var loc: Position = Global
  }

  private def interpException(msg: String) = new InterpreterException(msg)

  private var stack = new Stack
}
