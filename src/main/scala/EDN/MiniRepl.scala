package EDN

//import scala.util.parsing.combinator._

import Reader._

/**
 * Created by mathieu on 12/02/2015.
 */
object MiniRepl  {

  def stdin = Console.readLine()

  def miniReplLoop() : Unit = {
    print("Edn> ")
    Console.flush()
    val result = Reader.parse(Reader.elem,stdin)
    result match {
      case Success(matched,_) => println(s"Parsed $matched " + matched.getClass())
      case Failure(msg,_) =>     println("FAILURE: " + msg)
      case Error(msg,_) =>       println("ERROR: " + msg)

    }
    miniReplLoop()
  }
  def main(args : Array[String]) = miniReplLoop()

}
