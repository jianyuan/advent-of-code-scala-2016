/**
  * Created by jianyuanlee on 17/12/2016.
  */
object Day6 extends App {

  val input = io.Source.fromInputStream(io.Source.getClass.getResourceAsStream("/day6.txt")).getLines().toList

  val answer1 = input.transpose.map(_.groupBy(identity).maxBy(_._2.size)._1).mkString
  println(s"What is the error-corrected version of the message being sent? $answer1")
}
