package EDN

import java.util.UUID
import java.text.DateFormat

/**
 * Namespace containing and EDN compliant writer
 * https://github.com/martintrojer/edn-scala
 */
object Writer {

  /**
   * Convert nested List, Set, Vector and Maps to and EDN string
   * @param data Any nested List, Set, Vector and Maps with Doubles, Strings, UUID or java.util.Date
   * @return EDN String
   */
  def writeAll(data: Any): String =  data match {
    case d: Double => d.toString
    case i: Int => i.toString
    case l: List[_] =>
      if (l == Nil) "()"
      else "(" + (l map writeAll).mkString(" ") + ")"
    case s: Set[_] =>
      if (s == Set.empty) "#{}"
      else "#{" + (s map writeAll).mkString(" ") + "}"
    case v: Vector[_] =>
      if (v == Vector.empty) "[]"
      else "[" + (v map writeAll).mkString(" ") + "]"
    case m: Map[_,_] =>
      if (m == Map.empty) "{}"
      else "{" + (m map { case (k, v) => writeAll(k) + " " + writeAll(v)})
        .mkString(" ") + "}"
    case true => "true"
    case false => "false"
    case null => "nil"
    case u: UUID => "#uuid \"" + u.toString + "\""
    case d: java.util.Date => Instant.write(d)
    case s: String =>
      if (s.startsWith(":")) s else "\"" + s + "\""
    case _ => ""
  }
}
