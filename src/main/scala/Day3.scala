/**
  * Created by jianyuanlee on 17/12/2016.
  */
object Day3 extends App {

  val input = io.Source.fromInputStream(io.Source.getClass.getResourceAsStream("/day3.txt")).getLines().toList

  val triangles = input.map(_.trim.split("""\s+""").map(_.toInt).toList.sorted)

  require(triangles.forall(_.size == 3))

  val answer1 = triangles.count(sizes => sizes.init.sum > sizes.last)
  println(s"How many of the listed triangles are possible? $answer1")

}
