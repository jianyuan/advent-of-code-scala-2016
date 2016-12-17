/**
  * Created by jianyuanlee on 17/12/2016.
  */
object Day6 extends App {

  val input = io.Source.fromInputStream(io.Source.getClass.getResourceAsStream("/day6.txt")).getLines().toList

  val answer1 = input.transpose.map(_.groupBy(identity).maxBy(_._2.size)._1).mkString
  println(s"Part 1: $answer1")

  val answer2 = input.transpose.map(_.groupBy(identity).minBy(_._2.size)._1).mkString
  println(s"Part 2: $answer2")
}
