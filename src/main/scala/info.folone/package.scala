package info.folone

import scalaz._
import effect._
import IO._ 
import std.option._
import std.list._
import syntax.show._
import syntax.traverse._
import syntax.std.boolean._
import Free.{suspend ⇒ _, _}
import Trampoline._
import Isomorphism._

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

  // This instance is not lawfull, but it is pretty convenient here
  implicit val isoTraverse: IsomorphismTraverse[Set, List] =
    new IsomorphismTraverse[Set, List] {
      def G = Traverse[List]
      def iso = new IsoFunctorTemplate[Set, List] {
        def to[A](sa: Set[A]): List[A]   = sa.toList
        def from[A](la: List[A]): Set[A] = la.toSet
      }
    }
}

// Poor man's graph library
trait GraphModule { self: TypeAliases ⇒

  type Node = String

  case class Graph(adjacencyList: Map[Node, Set[Node]]) {
    lazy val nodes = adjacencyList.keys

    def nodesWithin(n: Int, node: Node) = nodesWithinUnderlying(n, node).run.toSet
    private def nodesWithinUnderlying(n: Int, node: Node): Trampoline[Set[Node]] = {
      val adjacent  = adjacencyList(node)
      adjacent.map { nd ⇒
        for {
          res ← {
            if(n > 1) suspend(this.nodesWithinUnderlying(n - 1, nd))
            else done(Set())
          }
        } yield adjacent ++ res
      }.sequenceU.map(_.flatten)
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
