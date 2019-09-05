//package org.enso.syntax.text.ast.text
//
//import cats.Functor
//import cats.derived.semi
//import org.enso.data.ADT
//import org.enso.syntax.text.AST
//import org.enso.syntax.text.AST.Text.Segment
//import org.enso.syntax.text.AST.{ASTOf, OffsetZip}
//import org.enso.syntax.text.ast.Repr
//import org.enso.syntax.text.ast.Repr.R
//
//object Escape {
//
//  type Escape[T] = Segment.Escape[T]
//
//  type Number  = ASTOf[NumberOf]
//  type Invalid = ASTOf[InvalidOf]
//
//  case class NumberOf[T](int: Int) extends Escape[T]
//  object Number {
//    def apply[T](int: Int):    NumberOf[T]            = NumberOf(int)
//    implicit def functor:      Functor[NumberOf]      = semi.functor
//    implicit def offsetZip[T]: OffsetZip[NumberOf, T] = t => t.coerce
//    implicit def repr[T]:      Repr[NumberOf[T]]      = R + '\\' + _.int.toString
//  }
//  case class InvalidOf[T](str: String) extends Escape[T] //with AST.InvalidOf[T] FIXME if we unseal InvalidOf, we won't be able to derive functor for it
//  object Invalid {
//    def apply[T](str: String): InvalidOf[T]       = InvalidOf(str)
//    implicit def repr[T]:      Repr[InvalidOf[T]] = R + '\\' + _.str
//  }
//
//  // Reference: https://en.wikipedia.org/wiki/String_literal
//  sealed trait Unicode[T] extends Escape[T]
//  object Unicode {
//
//    type U          = ASTOf[UFO]
//    type U16        = ASTOf[UFO16]
//    type U32        = ASTOf[UFO32]
//    type U21        = ASTOf[UFO21]
//    type InvalidU16 = ASTOf[InvalidUFO16]
//    type InvalidU32 = ASTOf[InvalidUFO32]
//    type InvalidU21 = ASTOf[InvalidUFO21]
//
//    abstract class UFO[T](val pfx: String, val sfx: String) extends Unicode[T] {
//      val digits: String
//    }
//    object UFO {
//      implicit def repr[T]: Repr[UFO[T]] =
//        t => R + "\\" + t.pfx + t.digits + t.sfx
//    }
//
//    final case class UFO16[T] private (digits: String) extends UFO[T]("u", "")
//    final case class UFO32[T] private (digits: String) extends UFO[T]("U", "")
//    final case class UFO21[T] private (digits: String) extends UFO[T]("u{", "}")
//
//    final case class InvalidUFO16[T] private (digits: String)
//        extends UFO[T]("u", "")
////        with AST.InvalidOf[T]
//
//    final case class InvalidUFO32[T] private (digits: String)
//        extends UFO[T]("U", "")
////        with AST.InvalidOf[T]
//
//    final case class InvalidUFO21[T] private (digits: String)
//        extends UFO[T]("u{", "}")
////        with AST.InvalidOf[T]
//
//    object Validator {
//      val hexChars = (('a' to 'f') ++ ('A' to 'F') ++ ('0' to '9')).toSet
//      def isHexChar(char: Char) =
//        hexChars.contains(char)
//    }
//
//    object U16 {
//      def apply[T](digits: String): Unicode[T] =
//        if (validate(digits)) UFO16(digits) else InvalidUFO16(digits)
//      def validate(digits: String) = {
//        import Validator._
//        val validLength = digits.length == 4
//        val validChars  = digits.map(isHexChar).forall(identity)
//        validLength && validChars
//      }
//    }
//    object U32 {
//      def apply[T](digits: String): Unicode[T] =
//        if (validate(digits)) UFO32(digits) else InvalidUFO32(digits)
//      def validate(digits: String) = {
//        import Validator._
//        val validLength = digits.length == 8
//        val validPrefix = digits.startsWith("00")
//        val validChars  = digits.map(isHexChar).forall(identity)
//        validLength && validPrefix && validChars
//      }
//    }
//    object U21 {
//      def apply[T](digits: String): Unicode[T] =
//        if (validate(digits)) UFO21(digits) else InvalidUFO21(digits)
//      def validate(digits: String) = {
//        import Validator._
//        val validLength = digits.length >= 1 && digits.length <= 6
//        val validChars  = digits.map(isHexChar).forall(identity)
//        validLength && validChars
//      }
//    }
//  }
//
//  type Simple = ASTOf[SimpleOf]
//
//  case class SimpleOf[T](char: CharCode) extends Escape[T]
//  object SimpleOf {
//    implicit def repr[T]: Repr[SimpleOf[T]] = R + _.char.repr
//  }
//
//  abstract class CharCode(val code: Int) {
//    def name = toString
//    def repr = R + '\\' + name
//  }
//
//  case object Slash    extends CharCode('\\') { override def repr = "\\\\" }
//  case object Quote    extends CharCode('\'') { override def repr = "\\'"  }
//  case object RawQuote extends CharCode('"')  { override def repr = "\\\"" }
//
//  // Reference: https://en.wikipedia.org/wiki/String_literal
//  sealed trait Character extends CharCode
//  object Character {
//    case object a extends CharCode('\u0007') with Character
//    case object b extends CharCode('\u0008') with Character
//    case object f extends CharCode('\u000C') with Character
//    case object n extends CharCode('\n') with Character
//    case object r extends CharCode('\r') with Character
//    case object t extends CharCode('\u0009') with Character
//    case object v extends CharCode('\u000B') with Character
//    case object e extends CharCode('\u001B') with Character
//    val codes = ADT.constructors[Character]
//  }
//
//  // Reference: https://en.wikipedia.org/wiki/Control_character
//  sealed trait Control extends CharCode
//  object Control {
//    case object NUL extends CharCode(0x00) with Control
//    case object SOH extends CharCode(0x01) with Control
//    case object STX extends CharCode(0x02) with Control
//    case object ETX extends CharCode(0x03) with Control
//    case object EOT extends CharCode(0x04) with Control
//    case object ENQ extends CharCode(0x05) with Control
//    case object ACK extends CharCode(0x06) with Control
//    case object BEL extends CharCode(0x07) with Control
//    case object BS  extends CharCode(0x08) with Control
//    case object TAB extends CharCode(0x09) with Control
//    case object LF  extends CharCode(0x0A) with Control
//    case object VT  extends CharCode(0x0B) with Control
//    case object FF  extends CharCode(0x0C) with Control
//    case object CR  extends CharCode(0x0D) with Control
//    case object SO  extends CharCode(0x0E) with Control
//    case object SI  extends CharCode(0x0F) with Control
//    case object DLE extends CharCode(0x10) with Control
//    case object DC1 extends CharCode(0x11) with Control
//    case object DC2 extends CharCode(0x12) with Control
//    case object DC3 extends CharCode(0x13) with Control
//    case object DC4 extends CharCode(0x14) with Control
//    case object NAK extends CharCode(0x15) with Control
//    case object SYN extends CharCode(0x16) with Control
//    case object ETB extends CharCode(0x17) with Control
//    case object CAN extends CharCode(0x18) with Control
//    case object EM  extends CharCode(0x19) with Control
//    case object SUB extends CharCode(0x1A) with Control
//    case object ESC extends CharCode(0x1B) with Control
//    case object FS  extends CharCode(0x1C) with Control
//    case object GS  extends CharCode(0x1D) with Control
//    case object RS  extends CharCode(0x1E) with Control
//    case object US  extends CharCode(0x1F) with Control
//    case object DEL extends CharCode(0x7F) with Control
//    val codes = ADT.constructors[Control]
//  }
//}
