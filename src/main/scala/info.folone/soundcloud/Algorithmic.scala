package info.folone.soundcloud

import scala.collection.SortedMap

import scalaz._
import effect._
import std.list._
import syntax.traverse._
import syntax.show._


object Algorithmic {

  // generate graph from lines
  def generateGraphFrom(lines: List[String]): Graph = {
    val res = for {
      line ← lines
      pair = line.split("\\t")
      res ← List((pair(0), pair(1)), (pair(1), pair(0)))
      // For this problem having an adjacency matrix is more performant
      // than adjacdency list, that we have.
    } yield res
    Graph(res.groupBy(_._1).mapValues(_.map(_._2).toSet))
  }

  // Generate map of n-level friends for each user
  def processGraph(graph: Graph, n: Int): Result = {
    val res = graph.nodes.par.map { node ⇒
      node → graph.nodesWithin(n, node).toList.sorted.filterNot(_ == node)
    }.seq
    SortedMap(res.toSeq:_*)
  }

  def mainIO(path: String, n: Int, toFile: String): IO[Unit] = {
    val result = for {
      lines ← lines(path)
      graph = generateGraphFrom(lines)
    } yield processGraph(graph, n)

    result.flatMap(res ⇒ dumpToFile(res, toFile))
  }

  def main(args: Array[String]) = {
    val path   = args(0)
    val n      = args(1).toInt
    val toFile = args(2)

    // Yuck!
    mainIO(path, n, toFile).unsafePerformIO()
  }
}
