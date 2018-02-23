package jsy.student

import jsy.lab3.{Lab3Like, ast}
import jsy.util.JsyApplication

object Lab3 extends JsyApplication with Lab3Like {
  import jsy.lab3.ast._
  
  /*
   * CSCI 3155: Lab 3 
   * <Your Name>
   * 
   * Partner: <Your Partner's Name>
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the '???' expression with your code in each function.
   *
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your lab will not be graded if it does not compile.
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert. Simply put in a
   * '???' as needed to get something  that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   */
  
  /*
   * The implementations of these helper functions for conversions can come
   * Lab 2. The definitions for the new value type for Function are given.
   */
  
  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case B(b)=> if (b) 1 else 0
      case S("") => 0
      case  S(s) =>
        try {
          s.toDouble
        }
        catch {
          case e:java.lang.NumberFormatException  => Double.NaN
        }

      case Undefined => Double.NaN
      case Function(_, _, _) => Double.NaN
    }
  }
  
  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case N(n)=> n match{
        case Double.NaN => false
        case _ => n!=0
      }
      case S(s)=> s!=""
      case Undefined => false
      case Function(_, _, _) => true
    }
  }
  
  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => s
      case Undefined => "undefined"
      case Function(_, _, _) => "function"
      case _ => pretty(v)
        // Here in toStr(Function(_, _, _)), we will deviate from Node.js that returns the concrete syntax
        // of the function (from the input program).
    }
  }

  /*
   * Helper function that implements the semantics of inequality
   * operators Lt, Le, Gt, and Ge on values.
   *
   * We suggest a refactoring of code from Lab 2 to be able to
   * use this helper function in eval and step.
   */
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
    require(isValue(v1))
    require(isValue(v2))
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    (bop: @unchecked) match {
      case Lt => (v1, v2) match
      {
        case (S(s1), S(s2)) => toStr(S(s1)) < toStr(S(s2))
        case _ =>
          val v1Num = toNumber(v1)
          val v2Num = toNumber(v2)
          v1Num < v2Num
        }


      case Le => (v1, v2) match
      {
        case (S(s1), S(s2)) => toStr(S(s1)) <= toStr(S(s2))
        case _ =>
          val v1Num = toNumber(v1)
          val v2Num = toNumber(v2)
          v1Num <= v2Num

      }

      case Gt => (v1, v2) match
      {
        case (S(s1), S(s2)) => toStr(S(s1)) > toStr(S(s2))
        case _ =>
          val v1Num = toNumber(v1)
          val v2Num = toNumber(v2)
          v1Num > v2Num

      }

      case Ge => (v1, v2) match
      {
        case (S(s1), S(s2)) => toStr(S(s1)) >= toStr(S(s2))
        case _ =>
          val v1Num = toNumber(v1)
          val v2Num = toNumber(v2)
          v1Num >= v2Num

      }

    }
  }


  /* Big-Step Interpreter with Dynamic Scoping */
  
  /*
   * Start by copying your code from Lab 2 here.
   */
  def eval(env: Env, e: Expr): Expr = {
    e match {
      /* Base Cases */
      case N(_) | B(_) | S(_) | Undefined | Function(_, _, _) => e
      case Var(x) => try{env(x)} catch{case e: java.util.NoSuchElementException => throw new NoSuchElementException("key not found: " + x)}

      /* Inductive Cases */
      case Print(e1) => e1 match {
        case _ => println(pretty(eval(env, e1))); Undefined
      }

      case ConstDecl(x, e1, e2) => eval(env + (x -> eval(env, e1)), e2)


      case Unary(uop, e1) => uop match {
            case Neg => N(-toNumber(eval(env, e1)))

            case Not => B(!toBoolean(eval(env, e1)))
          }

      case Binary(bop, e1, e2) => bop match {
              case And =>
                val v1 = eval(env, e1)
                if (toBoolean(v1)) eval(env, e2) else v1


              case Or =>
                val v1 = eval(env, e1)
                if (toBoolean(v1)) v1 else eval(env, e2)


              case _ =>
                val v1 = eval(env, e1)
                val v2 = eval(env, e2)
                (bop: @unchecked) match {

                  case Plus => (v1, v2) match {
                    case (S(s), _) => S(s + toStr(v2))
                    case (_, S(s)) => S(toStr(v1) + s)
                    case _ => N(toNumber(v1) + toNumber(v2))
                  }

                  case Minus => N(toNumber(v1) - toNumber(v2))

                  case Times => N(toNumber(v1) * toNumber(v2))

                  case Div => (toNumber(v1), toNumber(v2)) match {
                    case (Double.NaN, _) => N(Double.NaN)
                    case (_, Double.NaN) => N(Double.NaN)
                    case (0, 0) => N(Double.NaN)
                    case (n, 0) if n < 0 => N(Double.NegativeInfinity)
                    case (n, 0) => N(Double.PositiveInfinity)
                    case (n, v) => N(n / v)
                  }

                  case Ne =>(v1, v2) match {
                    case (Function(_, _, _), _) => throw DynamicTypeError(e)
                    case (_, Function(_, _, _)) => throw DynamicTypeError(e)
                    case (_, _) => B(v1 != v2)
                  }

                  case Eq => (v1, v2) match {
                    case (Function(_, _, _), _) => throw DynamicTypeError(e)
                    case (_, Function(_, _, _)) => throw DynamicTypeError(e)
                    case (_, _) => B(v1 == v2)
                  }

                  case Seq => v2


                  case _ => B(inequalityVal(bop, v1, v2))

                }
            }



      case If(e1, e2, e3) => if (toBoolean(eval(env, e1))) eval(env, e2) else eval(env, e3)

      case Call(e1, e2) =>

        val v1 = eval(env, e1)


        v1 match {
          case Function(None, x, eToDo) => eval(env+(x -> eval(env, e2)), eToDo)

          case Function(Some(name),x,eToDo) => eval((env + (x -> eval(env, e2))) + (name -> Function(Some(name), x, eToDo)), eToDo)

          case _ => throw new DynamicTypeError(e)
        }
    }
  }
    

  /* Small-Step Interpreter with Static Scoping */

  def iterate(e0: Expr)(next: (Expr, Int) => Option[Expr]): Expr = {
    def loop(e: Expr, n: Int): Expr = next(e,n) match {
      case None => e
      case Some(ep) => loop(ep, n+1)
    }
    loop(e0, 0)
  }

  def substitute(e: Expr, v: Expr, x: String): Expr = {
    require(isValue(v))
    e match {
      case N(_) | B(_) | Undefined | S(_) => e

      case Print(e1) => Print(substitute(e1, v, x))

      case Unary(uop, e1) => Unary(uop, substitute(e1, v, x))

      case Binary(bop, e1, e2) => Binary(bop, substitute(e1, v, x), substitute(e2, v, x))

      case If(e1, e2, e3) => If(substitute(e1, v, x), substitute(e2, v, x), substitute(e3, v, x))

      case Call(e1, e2) => Call(substitute(e1, v, x), substitute(e2, v,x))

      case Var(y) => if (y==x) v else Var(y)

      case Function(None, y, e1) =>  if (y==x) Function(None, y, e1) else Function(None, y, substitute(e1, v, x))

      case Function(Some(y1), y2, e1) => if (y1==x || y2==x) Function(Some(y1), y2, e1) else Function(Some(y1), y2, substitute(e1, v, x))

      case ConstDecl(y, e1, e2) => if (y == x) ConstDecl(y, substitute(e1, v, x), e2) else ConstDecl(y, substitute(e1, v, x), substitute(e2, v,x))
    }
  }
    
  def step(e: Expr): Expr = {
    e match {
      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => println(pretty(v1)); Undefined

      case Unary(uop, e1) if isValue(e1) => uop match {

        case Neg => N(-toNumber(e1))

        case Not => B(!toBoolean(e1))
      }

      case Binary(bop, e1, e2) if isValue(e1) && !isValue(e2) && (bop == And || bop == Or || bop == Seq) => bop match {

        case And => if (toBoolean(e1)) e2 else e1

        case Or => if (toBoolean(e1)) e1 else e2

        case Seq => e2
      }

      case Binary(bop, Function(_,_,_), _) if bop == Eq || bop == Ne => throw new DynamicTypeError(e)

      case Binary(bop, e1, e2) if isValue(e1) && isValue(e2) => bop match {

          case Plus => (e1, e2) match {
            case (S(s), _) => S(s + toStr(e2))
            case (_, S(s)) => S(toStr(e1) + s)
            case _ => N(toNumber(e1) + toNumber(e2))
          }

          case Minus => N(toNumber(e1) - toNumber(e2))

          case Times => N(toNumber(e1) * toNumber(e2))

          case Div => (toNumber(e1), toNumber(e2)) match {
                  case (Double.NaN, _) => N(Double.NaN)
                  case (_, Double.NaN) => N(Double.NaN)
                  case (0, 0) => N(Double.NaN)
                  case (n, 0) if n < 0 => N(Double.NegativeInfinity)
                  case (n, 0) => N(Double.PositiveInfinity)
                  case (n, v) => N(n / v)
                }

          case Eq => e2 match {
            case Function(_, _, _) => throw new DynamicTypeError(e)
            case _ => B(e1 == e2)
          }

          case Ne => e2 match{
            case Function(_,_,_) => throw new DynamicTypeError(e)
            case _ => B(e1 != e2)
          }

          case And => if (toBoolean(e1)) e2 else e1

          case Or => if (toBoolean(e1)) e1 else e2

          case Gt => (e1, e2) match {
              case (S(s1), S(s2)) => B(toStr(S(s1)) > toStr(S(s2)))
              case _ =>
                val v1Num = toNumber(e1)
                val v2Num = toNumber(e2)
                B(v1Num > v2Num)

            }

          case Lt => (e1, e2) match {
              case (S(s1), S(s2)) => B(toStr(S(s1)) < toStr(S(s2)))
              case _ =>
                val v1Num = toNumber(e1)
                val v2Num = toNumber(e2)
                B(v1Num < v2Num)

            }

          case Le => (e1, e2) match {
              case (S(s1), S(s2)) => B(toStr(S(s1)) <= toStr(S(s2)))
              case _ =>
                val v1Num = toNumber(e1)
                val v2Num = toNumber(e2)
                B(v1Num <= v2Num)

            }

          case Ge => (e1, e2) match {
              case (S(s1), S(s2)) => B(toStr(S(s1)) >= toStr(S(s2)))
              case _ =>
                val v1Num = toNumber(e1)
                val v2Num = toNumber(e2)
                B(v1Num >= v2Num)

            }

          case Seq => e2

        }
      case If(e1, e2, e3) if isValue(e1)=>
        if (toBoolean(e1)) e2 else e3

      case ConstDecl(x, e1, e2) if isValue(e1) => substitute(e2, e1, x)

      case Call(e1, e2) if isValue(e1) && isValue(e2) => e1 match {

        case Function(Some(funcName), x, eToDo) =>
          substitute(substitute(eToDo, Function(Some(funcName), x, eToDo), funcName), e2, x)

        case Function(None, x, eToDo) =>
          substitute(eToDo, e2, x)

        case _ => throw new DynamicTypeError(e)
      }


      /* Inductive Cases: Search Rules */
      case Print(e1) => Print(step(e1))

      // ****** Your cases here

      case Unary(uop, e1) => Unary(uop, step(e1))

      case Binary(bop, e1, e2) =>
        if (isValue(e1) && !isValue(e2)) Binary(bop, e1, step(e2))
        else Binary(bop, step(e1), e2)

      case If(e1, e2, e3) => If(step(e1), e2, e3)

      case ConstDecl(x, e1, e2) => ConstDecl(x, step(e1), e2)

      case Call(e1, e2) =>
        if (isValue(e1) && !isValue(e2)) {
          e1 match {
            case Function(_, _, _) => Call(e1, step(e2))
            case _ => throw new DynamicTypeError(e)
          }
        }

        else Call(step(e1), e2)

      /* Cases that should never match. Your cases above should ensure this. */
      case Var(_) => throw new AssertionError("Gremlins: internal error, not closed expression.")
      case N(_) | B(_) | Undefined | S(_) | Function(_, _, _) => throw new AssertionError("Gremlins: internal error, step should not be called on values.");
    }
  }


  /* External Interfaces */
  
  //this.debug = true // uncomment this if you want to print debugging information
  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file

}
