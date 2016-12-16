/**
  * Created by jianyuanlee on 17/12/2016.
  */
object Day3 extends App {

  val input = io.Source.fromInputStream(io.Source.getClass.getResourceAsStream("/day3.txt")).getLines().toList

  val triangles = input.map(_.trim.split("""\s+""").map(_.toInt).toList)

  require(triangles.forall(_.size == 3))

  def countValidTriangles(triangles: List[List[Int]]): Int = {
    triangles
      .map(_.sorted)
      .count(sizes => sizes.init.sum > sizes.last)
  }

  val answer1 = countValidTriangles(triangles)
  println(s"Part 1: How many of the listed triangles are possible? $answer1")

  val answer2 = countValidTriangles(triangles.grouped(3).flatMap(_.transpose).toList)
  println(s"Part 2: How many of the listed triangles are possible? $answer2")
}
