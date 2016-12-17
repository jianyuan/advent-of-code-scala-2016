import java.security.MessageDigest

/**
  * Created by jianyuanlee on 17/12/2016.
  */
object Day5 extends App {

  case class DoorDigest(doorId: String) {
    private val initialDoorIdDigest = MessageDigest.getInstance("MD5")
    initialDoorIdDigest.update(doorId.getBytes)

    def doorIdDigest: MessageDigest = initialDoorIdDigest.clone.asInstanceOf[MessageDigest]

    def hashIndex(i: Int): String = doorIdDigest.digest(i.toString.getBytes).map("%02x".format(_)).mkString
  }

  val input = "ffykfhsq"
  val doorDigest = DoorDigest(input)

  val answer1 = Stream
    .from(0)
    .map(doorDigest.hashIndex)
    .filter(_.substring(0, 5) == "00000")
    .take(8)
    .map(_ (5))
    .mkString
  println(s"Given the actual Door ID, what is the password? $answer1")
}
