import java.security.MessageDigest

import scala.annotation.tailrec

/**
  * Created by jianyuanlee on 17/12/2016.
  */
object Day5 extends App {

  case class DoorDigest(doorId: String) {
    private val initialDoorIdDigest = MessageDigest.getInstance("MD5")
    initialDoorIdDigest.update(doorId.getBytes)

    def doorIdDigest: MessageDigest = initialDoorIdDigest.clone.asInstanceOf[MessageDigest]

    def hashForIndex(i: Int): String = doorIdDigest.digest(i.toString.getBytes).map("%02x".format(_)).mkString
  }

  val input = "ffykfhsq"
  val doorDigest = DoorDigest(input)

  val combinations: Stream[String] = Stream
    .from(0)
    .map(doorDigest.hashForIndex)
    .filter(_.substring(0, 5) == "00000")

  val answer1 = combinations
    .take(8)
    .map(_ (5))
    .mkString
  println(s"Given the actual Door ID, what is the password? $answer1")

  @tailrec
  def solve(combinations: Stream[String], password: List[Option[Char]]): String =
    if (password.forall(_.isDefined)) password.map(_.get).mkString
    else combinations match {
      case combination #:: others if combination(5) <= '7' && password(combination(5) - '0').isEmpty =>
        solve(others, password.updated(combination(5) - '0', Some(combination(6))))
      case _ #:: others => solve(others, password)
    }

  val answer2 = solve(combinations, List.fill(8)(Option.empty[Char]))
  println(s"Given the actual Door ID and this new method, what is the password? $answer2")
}
