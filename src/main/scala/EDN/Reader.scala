package EDN

import clojure.lang.Keyword

import scala.util.parsing.combinator._
import java.util.UUID

/**
 * Namespace containing a EDN parser
 * https://github.com/martintrojer/edn-scala
 *
 * The reader can be extended to handle more "tagged literals" in the pattern match in tagElem below
 */
object Reader extends JavaTokenParsers {
  val set: Parser[Set[Any]] = "#{" ~> rep(elem) <~ "}" ^^ (Set() ++ _)
  val map: Parser[Map[Any, Any]] = "{" ~> rep(pair) <~ "}" ^^ (Map() ++ _)
  val vector: Parser[Vector[Any]] = "[" ~> rep(elem) <~ "]" ^^ (Vector() ++ _)
  val list: Parser[List[Any]] = "(" ~> rep(elem) <~ ")"
  val keyword: Parser[Keyword] = ":" ~> """[^,#\"\{\}\[\]\(\)\s]+""".r ^^ (Keyword.intern(_))
  val symbol: Parser[clojure.lang.Symbol] = """[a-zA-Z][^,#\"\{\}\[\]\(\)\s]*""".r ^^ (clojure.lang.Symbol.create(_))
  lazy val pair: Parser[(Any, Any)] = elem ~ elem ^^ {
    case key ~ value => (key, value)
  }
  lazy val tagElem: Parser[Any] = """#[^,#\"\{\}\[\]\(\)\s]+""".r ~ elem ^^ {
    case "#uuid" ~ (value: String) => UUID.fromString(value)
    case "#inst" ~ (value: String) => Instant.read(value)
    case name ~ value => (name, value)
  }
  val ratio: Parser[Double] = floatingPointNumber ~ "/" ~ floatingPointNumber ^^ {
    case num ~ _ ~ denom => num.toDouble / denom.toDouble
  }

  val ednElem: Parser[Any] =  set | map | vector | list | keyword | tagElem | ratio |
                              wholeNumber <~ not('.') ^^ (_.toInt) |
                              floatingPointNumber     ^^ (_.toDouble) |
                              "nil"                   ^^ (_ => null)  |
                              "true"                  ^^ (_ => true)  |
                              "false"                 ^^ (_ => false) |
                              symbol                                  |
                              stringLiteral           ^^ { case "" => ""; case s => s.tail.init }

  val elem: Parser[Any] = ednElem | "," ~> elem | "N" ~> elem

  /**
   * Read all EDN form in the supplied java.io.Reader and return nested List, Set, Vector and Maps representing the data
   * @param reader java.io.Reader with source data
   * @return Any nested List, Set, Vector and Maps with Dobules, Strings, UUID or java.util.Date
   */
  def readAll(reader: java.io.Reader) = parseAll(elem, reader).get
  /** See above */
  def readAll(str: String) = parseAll(elem, str).get
}
