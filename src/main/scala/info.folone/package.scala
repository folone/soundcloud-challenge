package info.folone

import scalaz._
import effect._
import IO._ 
import std.option._
import syntax.show._
import syntax.std.boolean._

import scala.collection.SortedMap

// Let's bake a little cake
package object soundcloud extends GraphModule with IOModule with TypeAliases

// Types and their typeclass instances
trait TypeAliases { self: GraphModule ⇒
  // Result datatype
  type Result = SortedMap[Node, List[Node]]

  // Show typeclass instance for the result
  implicit val resShow = new Show[Result] {
    override def shows(res: Result) = res.map { case(name, friends) ⇒
      name + "\t" + friends.mkString("\t")
    }.mkString("\n")
  }
}

// Poor man's graph library
trait GraphModule { self: TypeAliases ⇒

  type Node = String

  case class Graph(adjacencyMatrix: Map[Node, Set[Node]]) {
    lazy val nodes = adjacencyMatrix.keys

    val nodesWithin = Memo.immutableHashMapMemo[(Int, Node), Set[Node]](nodesWithinUnderlying)
    private def nodesWithinUnderlying(tuple: (Int, Node)): Set[Node] = {
      val (n, node) = tuple
      val adjacent = adjacencyMatrix(node)
      adjacent ++ adjacent.flatMap { nd ⇒
        if(n > 1) this.nodesWithin(n - 1, nd)
        else Nil
      }
    }

  }
}

// Poor man's IO library
trait IOModule { self: TypeAliases ⇒

  import scala.io.Source
  import java.io.FileWriter

  // write to file
  def toFile(path: String, data: String) =
    IO(new FileWriter(path))
      .bracket(close) { fw ⇒
      IO(fw.write(data))
    }

  // read lines from file
  def lines(path: String): IO[List[String]] = IO(Source.fromFile(path))
    .bracket(close) { source ⇒
    IO(source.getLines.toList)
  }

  // Using structural types, which are implemented using reflection on JVM
  import scala.language.reflectiveCalls

  private def close(r: {def close(): Unit}) = IO { r.close() }

  // Write results to screen (for debugging)
  def dumpToScreen(res: Result): IO[Unit] = putStrLn(res.shows)

  // Write results to file
  def dumpToFile(res: Result, path: String): IO[Unit] =
    toFile(path, res.shows)
}
