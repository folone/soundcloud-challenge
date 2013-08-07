package info.folone.soundcloud

import scalaz._
import effect._
import std.list._
import syntax.traverse._
import syntax.show._
import syntax.id._


object Algorithmic {

  // generate graph from lines
  def generateGraphFrom(lines: List[String]): Graph = {
    val res = for {
      line ← lines
      pair = line.split("\\t")
      res ← List((pair(0), pair(1)), (pair(1), pair(0)))
    } yield res
    Graph(res.groupBy(_._1).mapValues(_.map(_._2).toSet))
  }

  // Generate map of n-level friends for each user
  def processGraph(n: Int)(graph: Graph): Result = {
    val res = graph.nodes.par.map { node ⇒
      node → graph.nodesWithin(n, node).toList.sorted.filterNot(_ == node)
    }.seq.toSeq
    scala.collection.SortedMap(res:_*)
  }

  def mainIO(path: String, n: Int, toFile: String): IO[Unit] = for {
    lines ← lines(path)
    res   ← generateGraphFrom(lines) |> processGraph(n) |> dumpToFile(toFile)
  } yield res

  def main(args: Array[String]) = {
    val path   = args(0)
    val n      = args(1).toInt
    val toFile = args(2)

    // Yuck!
    mainIO(path, n, toFile).unsafePerformIO()
  }
}
