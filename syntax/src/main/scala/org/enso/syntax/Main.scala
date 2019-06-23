package org.enso.syntax

import java.io.Reader
import java.io.StringReader

import org.enso.syntax.text.lexer.Lexer
import org.enso.syntax.text.parser.Parser
import org.enso.syntax.text.parser.BParser

//import org.enso.syntax.text.parser.PP

import scala.language.implicitConversions

import org.enso.syntax.text.lexer.SParser

import scala.collection.mutable
import scala.collection.immutable

import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox
import org.feijoas.mango.common.collect.Bound._
import org.feijoas.mango.common.{collect => RRR}
import org.feijoas.mango.common.collect.mutable.RangeMap
//import math.Ordering.Int
import java.io.Reader
import java.io.StringReader
import sys.process._
import java.awt.Desktop
import java.net.URI
import java.net.URL
import java.net.URLEncoder

trait IsoComputeState
case object NotComputed extends IsoComputeState
case object InProgress  extends IsoComputeState
case object Computed    extends IsoComputeState

class Logger {
  var nesting = 0

  def log(msg: String): Unit =
    println("    " * nesting + msg)

  def group[T](msg: String)(f: () => T): T = {
    log(msg)
    nesting += 1
    val out = f()
    nesting -= 1
    out
  }
}

class State {
//  val links                         = mutable.SortedMap[Int, Int]()
  val isoLinks                      = new mutable.ArrayBuffer[Int]()
  var isos                          = Set[Int]()
  var isosId: Int                   = 0
  var isosComputed: IsoComputeState = NotComputed

  var start  = false
  var end    = false
  var code   = ""
  val links2 = RangeMap[Int, Int, Ordering.Int.type]()
}

////////////////
// Vocabulary //
////////////////

case class Range(start: Int, end: Int)

class Vocabulary {
  var divisions = immutable.SortedSet[Int](0, Int.MaxValue)

  def insert(range: Range): Unit = {
    divisions = divisions + range.start
    divisions = divisions + (range.end + 1)
  }

  def size(): Int = divisions.size - 1

  override def toString: String = {
    val elems   = mutable.ArrayBuffer[String]()
    val minCode = 32
    val maxCode = 126
    divisions.toList.foreach(i => {
      if (i >= minCode && i <= maxCode) {
        elems += s"'${i.toChar.toString}'"
      }
      else {
        elems += i.toString
      }
    })
    "Vocabulary(List(" + elems.mkString(",") + "))"
  }

//  def zipWithIndex(): mutable.Set[(Int, Int)] = divisions.zipWithIndex

  def forEach[U](f: Range => U): Unit = {
    var lastDiv = 0
    for (i <- divisions.drop(1)) {
      f(Range(lastDiv, i - 1))
      lastDiv = i
    }
  }

  def forEachIxed[U](f: ((Range, Int)) => U): Unit = {
    var lastDiv = 0
    for ((i, ix) <- divisions.drop(1).zipWithIndex) {
      f((Range(lastDiv, i - 1), ix))
      lastDiv = i
    }
  }

}

/////////
// NFA //
/////////

class NFA {
  val logger                             = new Logger()
  val states: mutable.ArrayBuffer[State] = new mutable.ArrayBuffer()
  val isoMap: mutable.Map[Set[Int], Int] = mutable.Map()

  val vocabulary = new Vocabulary()

  def addState(): Int = {
    val state = new State()
    states += state
    states.length - 1
  }

  def link(start: Int, end: Int, charStart: Char, charEnd: Char): Unit = {
    val icharStart = charStart.toInt
    val icharEnd   = charEnd.toInt

    vocabulary.insert(Range(icharStart, icharEnd))
    state(start).links2.put(RRR.Range.closed(icharStart, icharEnd), end)
  }

  def link(start: Int, end: Int, char: Char): Unit =
    link(start, end, char, char)

  def link(start: Int, end: Int): Unit =
    state(start).isoLinks += end

  def visualize(): String = {
    var lines = mutable.ArrayBuffer[String]()
    lines += "digraph G {"
    for ((state, source) <- states.zipWithIndex) {
      lines += s"${source}"
      for ((range, target) <- state.links2.asMapOfRanges()) {
        lines += s"""${source} -> ${target} [label="${range}"]"""
      }
      for (target <- state.isoLinks) {
        lines += s"""${source} -> ${target} [style="dashed"]"""
      }
    }
    lines += "}"
    val code    = lines.mkString("\n")
    var webCode = code
    webCode = URLEncoder.encode(webCode, "UTF-8")
    webCode = webCode.replaceAll("[+]", "%20")
    val address = "https://dreampuf.github.io/GraphvizOnline/#" + webCode
    Desktop.getDesktop().browse(new URI(address))
    code
  }

  def state(ix: Int): State =
    states(ix)

  def computeIsosFor(i: Int): Unit = {
    val s    = state(i)
    var isos = Set[Int](i)
    if (s.isosComputed == NotComputed) {
      var circular = false
      s.isosComputed = InProgress
      s.isoLinks.foreach((tgt) => {
        computeIsosFor(tgt)
        val s2 = state(tgt)
        isos = isos + tgt
        isos = isos ++ s2.isos
        if (s2.isosComputed == InProgress) {
          circular = true
        }
      })
      s.isos = isos
      if (!circular) {
        isoMap.get(isos) match {
          case Some(id) => s.isosId = id
          case None => {
            val id = isoMap.size
            s.isosId = id
            isoMap += (isos -> id)
          }
        }
        s.isosComputed = Computed
      }
    }
  }

  def computeIsos(): Unit =
    for (i <- states.indices) {
      computeIsosFor(i)
    }

  def computeNFAMatrix2(): Array[Array[Int]] = {
    logger.group("Computing NFA Matrix")(() => {
      val matrix = Array.ofDim[Int](states.length, vocabulary.size)
      for (stateIx <- states.indices) {
        val s = state(stateIx)
        vocabulary.forEachIxed({
          case (range, vocIx) => {
            s.links2.get(range.start) match {
              case Some(tgt) => matrix(stateIx)(vocIx) = tgt
              case None      => matrix(stateIx)(vocIx) = -1
            }

          }
        })
      }
      matrix
    })
  }

  def computeDFA(): DFA = {
    logger.group("Computing DFA Matrix")(() => {
      val nfaMatrix  = computeNFAMatrix2()
      var dfaRows    = 0
      var dfaMatrix  = Array[Array[Int]]()
      val dfaIsoMap  = mutable.Map[Set[Int], Int]()
      val dfaIsoKeys = mutable.ArrayBuffer[Set[Int]]()

      def addDFAKey(key: Set[Int]): Int = {
        val id = dfaIsoMap.size
        dfaIsoMap += (key -> id)
        dfaIsoKeys += key
        dfaRows += 1
        dfaMatrix :+= Array.fill(vocabulary.size) { -1 }
        logger.log(s"DFA[${id}] = ${key}")
        id
      }

      logger.group(s"Preparing start points")(() => {
        val startIsos = state(0).isos
        addDFAKey(startIsos)
      })

      var i = 0
      while (i < dfaRows) {
        val isos = dfaIsoKeys(i)
        logger.group(s"Computing DFA[${i}]")(() => {

          vocabulary.forEachIxed({
            case (voc, vocIx) => {
              logger.group(s"Vocabulary '${voc}'")(() => {
                var tt = Set[Int]()
                isos.foreach(iso => {
                  val tgt = nfaMatrix(iso)(vocIx)
                  if (tgt != -1) {
                    tt = tt ++ state(tgt).isos
                  }
                })
                if (!tt.isEmpty) {
                  dfaMatrix(i)(vocIx) = dfaIsoMap.get(tt) match {
                    case None => addDFAKey(tt)
                    case Some(id) => {
                      logger.log(s"Existing DFA ID ${id}")
                      id
                    }
                  }
                }
              })
            }
          })
        })
        i += 1
      }

      val nfaEndStatePriorityMap = mutable.Map[Int, Int]()
      for (i <- nfaMatrix.indices) {
        if (state(i).end) {
          nfaEndStatePriorityMap += (i -> (nfaMatrix.length - i))
        }
      }

      val dfaEndStatePriorityMap = mutable.Map[Int, (Int, String)]()
      for ((isos, dfaIx) <- dfaIsoKeys.zipWithIndex) {
        var priority = -1
        var code     = ""
        isos.foreach(iso => {
          nfaEndStatePriorityMap.get(iso) match {
            case None => {}
            case Some(p) => {
              if (p > priority) {
                priority = p
                code     = state(iso).code
              }
            }
          }
        })
        if (priority >= 0) {
          dfaEndStatePriorityMap += dfaIx -> (priority, code)
        }
      }

      println(">>>", nfaEndStatePriorityMap)
      println(">>>", dfaEndStatePriorityMap)

      DFA(vocabulary, dfaMatrix, dfaEndStatePriorityMap)
    })
  }
}

case class DFA(
  vocabulary: Vocabulary,
  links: Array[Array[Int]],
  endStatePriorityMap: mutable.Map[Int, (Int, String)]) {}

case class CodeGen(dfa: DFA) {
  var code = new CodeBuilder()

  def generateStateMatch(): Unit = {
    code._match("state")(() => {
      for (state <- dfa.links.indices) {
        code._case(state)(() => {
          dfa.vocabulary.forEachIxed({
            case (range, vocIx) => {
              val targetState = dfa.links(state)(vocIx)
              val p1          = dfa.endStatePriorityMap.get(state)
              val p2          = dfa.endStatePriorityMap.get(targetState)
              var blocked     = false
              println(s"** (${state} -> ${targetState})", p1, p2)
              (p1, p2) match {
                case (Some((l, _)), Some((r, _))) =>
                  if (l > r) {
                    blocked = true
                  }
                case _ => {}
              }
              if (vocIx != 0) {
                code.add("else ")
              }
              code._ifLTE("codePoint", range.end)(() => {
                if (blocked) {
                  code.comment("blocked")
                  code.assign("state", -1)
                }
                else if (state != targetState) {
                  var targetStatex = targetState
                  if (targetState == -1) {
                    p1 match {
                      case None => { targetStatex = -2 }
                      case Some((_, c)) => {
                        code.addLine(c)
                      }
                    }
                  }
                  code.assign("state", targetStatex)
                }
              })
            }
          })
        })
      }
    })
  }

  def generate(): String = {
    code
      .add("while(state >= 0)")
      .block(() => {
        code.addLine("codePoint = buffer(offset).toInt")
        generateStateMatch()
        code.addLine("if(state >= 0) {offset += 1}")
      })
    code.build()
  }

}

class CodeBuilder {
  var lines       = mutable.ArrayBuffer[String]()
  var currentLine = ""
  var indentation = 0

  def submitLine(): CodeBuilder = {
    if (!currentLine.isEmpty) {
      lines += "  " * indentation + currentLine
      currentLine = ""
    }
    this
  }

  def add(s: String): CodeBuilder = {
    currentLine += s
    this
  }

  def addLine(s: String): CodeBuilder = {
    add(s)
    submitLine()
  }

  def comment(s: String): CodeBuilder = {
    add(s"// ${s}")
    submitLine()
  }

  def _match[T](s: Any)(f: () => T): CodeBuilder =
    add(s"${s} match").block(f)

  def _case[T](s: Any)(f: () => T): CodeBuilder =
    add(s"case ${s} =>").block(f)

  def _if[T](s: String)(f: () => T): CodeBuilder =
    add(s"if (${s})").block(f)

  def _else_if[T](s: String)(f: () => T): CodeBuilder = {
    add("else ")
    _if(s)(f)
  }

  def _ifLTE[T](left: Any, right: Any)(f: () => T): CodeBuilder =
    _if(s"${left} <= ${right}")(f)

  def _else_ifLTE[T](left: Any, right: Any)(f: () => T): CodeBuilder = {
    add("else ")
    _ifLTE(left, right)(f)
  }

  def assign(left: Any, right: Any): Unit = {
    add(s"${left} = ${right}")
    submitLine()
  }

  def block[T](f: () => T): CodeBuilder = {
    add(" {")
    submitLine()
    indentation += 1
    f()
    submitLine()
    indentation -= 1
    add("}")
    submitLine()
    this
  }

  def build(): String = {
    submitLine()
    lines.mkString("\n")
  }

}

trait Expr {
  def |(that: Expr)  = Or(this, that)
  def >>(that: Expr) = Seq(this, that)

  def many(): Expr = Many(this)
}
case class Ran(start: Char, end: Char)    extends Expr
case class Or(left: Expr, right: Expr)    extends Expr
case class Seq(first: Expr, second: Expr) extends Expr
case class Many(body: Expr)               extends Expr

case class Rule(expr: Expr, code: String)
case class Recipe(rules: List[Rule]) {

  def buildAutomata(): NFA = {
    val nfa       = new NFA
    val start     = nfa.addState()
    val endpoints = rules.map(rule => buildRuleAutomata(nfa, start, rule))
    val end       = nfa.addState()
    nfa.state(end).end = true
    for (endpoint <- endpoints) {
      nfa.link(endpoint, end)
    }
    nfa
  }

  def buildRuleAutomata(nfa: NFA, previous: Int, rule: Rule): Int = {
    val end = buildExprAutomata(nfa, previous, rule.expr)
    nfa.state(end).end  = true
    nfa.state(end).code = rule.code
    end
  }

  def buildExprAutomata(nfa: NFA, previous: Int, expr: Expr): Int = {
    val current = nfa.addState()
    nfa.link(previous, current)
    expr match {
      case Ran(start, end) => {
        val state = nfa.addState()
        nfa.link(current, state, start, end)
        state
      }
      case Seq(first, second) => {
        val s1 = buildExprAutomata(nfa, current, first)
        buildExprAutomata(nfa, s1, second)
      }
      case Many(body) => {
        val s1 = nfa.addState()
        val s2 = buildExprAutomata(nfa, s1, body)
        val s3 = nfa.addState()
        nfa.link(current, s1)
        nfa.link(current, s3)
        nfa.link(s2, s3)
        nfa.link(s3, s1)
        s3
      }
      case Or(first, second) => {
        val s1 = buildExprAutomata(nfa, current, first)
        val s2 = buildExprAutomata(nfa, current, second)
        val s3 = nfa.addState()
        nfa.link(s1, s3)
        nfa.link(s2, s3)
        s3
      }
    }
  }
}

object Main extends App {

  implicit def charToExpr(char: Char): Expr = Ran(char, char)

  class ExtendedChar(_this: Char) {
    def ||(that: Char): Expr = Or(char(_this), char(that))
  }

  implicit def extendChar(i: Char) = new ExtendedChar(i)

  def char(c: Char): Expr = Ran(c, c)

  var indent = 0

  def showMatrix[T](matrix: Array[Array[T]]): String = {
    var repr = ""
    for (i <- matrix.indices) {
      val row = matrix(i)
      for (j <- row.indices) {
        val cell = row(j).toString()
        repr += cell + " " * (3 - cell.length)
      }
      repr += "\n"
    }
    repr
  }

  def pprint(s: String) {
    print("  " * indent)
    val (l, r2) = s.span(x => x != '(' && x != ')')
    print(l)
    if (r2 == "") {
      println
      return
    }

    val (m, r) = r2.splitAt(1)

    if (m == "(") {
      indent += 1
      println(m)
      pprint(r)
    }
    else if (m == ")") {
      indent -= 1
      println(m)
      pprint(r)
    }

  }

//  val str = "a (b"
  val str = "a\n b\n a" // .stripMargin
  println(str)
  val reader = new StringReader(str)
  val ss     = new Lexer(reader)
  val toks   = ss.lexAll()
  var ttt    = 10
  pprint(toks.toString)

  val sparser = new SParser(new StringReader(str))

  val bparser = new BParser(new StringReader(str))
  val parser  = new Parser(new StringReader(str))

  pprint(bparser.parse.toString())
  pprint(parser.parse.toString())
  pprint("!")
  println(sparser.strInput)
  pprint(sparser.parse.toString)

//  val p_ab: Expr = ('a' || 'b').many
//  val p_ac: Expr = ('a' || 'c').many
//
//  val recipe = Recipe(Rule(p_ab, "") :: Rule(p_ac, "") :: Nil)
//
//  val nfa = recipe.buildAutomata()

  val p_test1: Expr = char('x') >> ('a'.many)
  val p_test2: Expr = char('x') >> ('b'.many)

  val p_ab: Expr = ('a' || 'b').many
  val p_ac: Expr = ('a' || 'c').many

  val p_x_ab: Expr = char('x') >> p_ab >> char('y')
  val p_x_ac: Expr = char('x') >> p_ac >> char('y')

  val recipe = Recipe(
    Rule(p_x_ab, "println(\"rule1!\")") :: Rule(p_x_ac, "println(\"rule2!\")") :: Nil
  )

  val nfa = recipe.buildAutomata()

//  val nfa = new NFA
//  val s1  = nfa.addState()
//  val s2  = nfa.addState()
//  val s3  = nfa.addState()
//  val s4  = nfa.addState()
//  val s5  = nfa.addState()
//  val s6  = nfa.addState()
//  val s7  = nfa.addState()
//  val s8  = nfa.addState()
//  val s9  = nfa.addState()
//  val s10 = nfa.addState()
//  nfa.state(s5).end  = true
//  nfa.state(s9).end  = true
//  nfa.state(s10).end = true
//
//  nfa.link(s1, s2)
//  nfa.link(s2, s3)
//  nfa.link(s2, s5)
//  nfa.link(s3, s4, 'A')
//  nfa.link(s3, s4, 'B')
//  nfa.link(s4, s5)
//  nfa.link(s5, s3)
//  nfa.link(s5, s10)
//
//  nfa.link(s1, s6)
//  nfa.link(s6, s7)
//  nfa.link(s6, s9)
//  nfa.link(s7, s8, 'A')
//  nfa.link(s7, s8, 'C')
//  nfa.link(s8, s9)
//  nfa.link(s9, s7)
//  nfa.link(s9, s10)

  println("--------------------")
  println(nfa.vocabulary)

//  println(nfa.visualize())

  println("--------------------")

  nfa.computeIsos()

//  println(nfa.state(s1).isos)
  val m2 = nfa.computeNFAMatrix2()
  println(showMatrix(m2))

  val dfa = nfa.computeDFA()

  println(CodeGen(dfa).generate())

  val BUFFERSIZE = 16384
  val sreader    = new StringReader("xayp")
  val buffer     = new Array[Char](BUFFERSIZE)
  val numRead    = sreader.read(buffer, 0, buffer.length)

  var offset: Int    = 0
  var codePoint: Int = 0
  var state: Int     = 0

  while (state >= 0) {
    codePoint = buffer(offset).toInt
    state match {
      case 0 => {
        if (codePoint <= 96) {
          state = -2
        }
        else if (codePoint <= 97) {
          state = -2
        }
        else if (codePoint <= 98) {
          state = -2
        }
        else if (codePoint <= 99) {
          state = -2
        }
        else if (codePoint <= 119) {
          state = -2
        }
        else if (codePoint <= 120) {
          state = 1
        }
        else if (codePoint <= 121) {
          state = -2
        }
        else if (codePoint <= 2147483646) {
          state = -2
        }
      }
      case 1 => {
        if (codePoint <= 96) {
          state = -2
        }
        else if (codePoint <= 97) {
          state = 2
        }
        else if (codePoint <= 98) {
          state = 3
        }
        else if (codePoint <= 99) {
          state = 4
        }
        else if (codePoint <= 119) {
          state = -2
        }
        else if (codePoint <= 120) {
          state = -2
        }
        else if (codePoint <= 121) {
          state = 5
        }
        else if (codePoint <= 2147483646) {
          state = -2
        }
      }
      case 2 => {
        if (codePoint <= 96) {
          state = -2
        }
        else if (codePoint <= 97) {}
        else if (codePoint <= 98) {
          state = 3
        }
        else if (codePoint <= 99) {
          state = 4
        }
        else if (codePoint <= 119) {
          state = -2
        }
        else if (codePoint <= 120) {
          state = -2
        }
        else if (codePoint <= 121) {
          state = 5
        }
        else if (codePoint <= 2147483646) {
          state = -2
        }
      }
      case 3 => {
        if (codePoint <= 96) {
          state = -2
        }
        else if (codePoint <= 97) {
          state = 6
        }
        else if (codePoint <= 98) {}
        else if (codePoint <= 99) {
          state = -2
        }
        else if (codePoint <= 119) {
          state = -2
        }
        else if (codePoint <= 120) {
          state = -2
        }
        else if (codePoint <= 121) {
          state = 7
        }
        else if (codePoint <= 2147483646) {
          state = -2
        }
      }
      case 4 => {
        if (codePoint <= 96) {
          state = -2
        }
        else if (codePoint <= 97) {
          state = 8
        }
        else if (codePoint <= 98) {
          state = -2
        }
        else if (codePoint <= 99) {}
        else if (codePoint <= 119) {
          state = -2
        }
        else if (codePoint <= 120) {
          state = -2
        }
        else if (codePoint <= 121) {
          state = 9
        }
        else if (codePoint <= 2147483646) {
          state = -2
        }
      }
      case 5 => {
        if (codePoint <= 96) {
          println("rule1!")
          state = -1
        }
        else if (codePoint <= 97) {
          println("rule1!")
          state = -1
        }
        else if (codePoint <= 98) {
          println("rule1!")
          state = -1
        }
        else if (codePoint <= 99) {
          println("rule1!")
          state = -1
        }
        else if (codePoint <= 119) {
          println("rule1!")
          state = -1
        }
        else if (codePoint <= 120) {
          println("rule1!")
          state = -1
        }
        else if (codePoint <= 121) {
          println("rule1!")
          state = -1
        }
        else if (codePoint <= 2147483646) {
          println("rule1!")
          state = -1
        }
      }
      case 6 => {
        if (codePoint <= 96) {
          state = -2
        }
        else if (codePoint <= 97) {}
        else if (codePoint <= 98) {
          state = 3
        }
        else if (codePoint <= 99) {
          state = -2
        }
        else if (codePoint <= 119) {
          state = -2
        }
        else if (codePoint <= 120) {
          state = -2
        }
        else if (codePoint <= 121) {
          state = 7
        }
        else if (codePoint <= 2147483646) {
          state = -2
        }
      }
      case 7 => {
        if (codePoint <= 96) {
          println("rule1!")
          state = -1
        }
        else if (codePoint <= 97) {
          println("rule1!")
          state = -1
        }
        else if (codePoint <= 98) {
          println("rule1!")
          state = -1
        }
        else if (codePoint <= 99) {
          println("rule1!")
          state = -1
        }
        else if (codePoint <= 119) {
          println("rule1!")
          state = -1
        }
        else if (codePoint <= 120) {
          println("rule1!")
          state = -1
        }
        else if (codePoint <= 121) {
          println("rule1!")
          state = -1
        }
        else if (codePoint <= 2147483646) {
          println("rule1!")
          state = -1
        }
      }
      case 8 => {
        if (codePoint <= 96) {
          state = -2
        }
        else if (codePoint <= 97) {}
        else if (codePoint <= 98) {
          state = -2
        }
        else if (codePoint <= 99) {
          state = 4
        }
        else if (codePoint <= 119) {
          state = -2
        }
        else if (codePoint <= 120) {
          state = -2
        }
        else if (codePoint <= 121) {
          state = 9
        }
        else if (codePoint <= 2147483646) {
          state = -2
        }
      }
      case 9 => {
        if (codePoint <= 96) {
          println("rule2!")
          state = -1
        }
        else if (codePoint <= 97) {
          println("rule2!")
          state = -1
        }
        else if (codePoint <= 98) {
          println("rule2!")
          state = -1
        }
        else if (codePoint <= 99) {
          println("rule2!")
          state = -1
        }
        else if (codePoint <= 119) {
          println("rule2!")
          state = -1
        }
        else if (codePoint <= 120) {
          println("rule2!")
          state = -1
        }
        else if (codePoint <= 121) {
          println("rule2!")
          state = -1
        }
        else if (codePoint <= 2147483646) {
          println("rule2!")
          state = -1
        }
      }
    }
    if (state >= 0) {
      offset += 1
    }
  }

  println(offset)
  println(state)

//  def refill() = {}
//  println(rangeMap.get(7))
//  rangeMap += Range.open(9, 10) -> "2" // Map((3..7) -> 1, [9..10] -> 2)
//  rangeMap += Range.closed(12, 16) -> "3" // Map((3..7) -> 1, [9..10] -> 2, [12..16] -> 3)
  //  val toolbox = currentMirror.mkToolBox()
//
//  val as    = "2*(2+3)"
//  val compe = toolbox.eval(toolbox.parse(as))
//
//  println(compe.getClass) // prints class java.lang.Integer
//  println(compe) // prints 10

}
