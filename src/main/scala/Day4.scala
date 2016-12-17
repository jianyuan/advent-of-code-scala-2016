/**
  * Created by jianyuanlee on 17/12/2016.
  */
object Day4 extends App {

  val input = io.Source.fromInputStream(io.Source.getClass.getResourceAsStream("/day4.txt")).getLines().toList

  val roomPattern = """([a-z\-]+)-(\d+)\[([a-z]+)\]""".r

  case class Room(name: String, sectorId: Int, checksum: String) {
    def isValid: Boolean = {
      val expectedChecksum = name
        .filterNot(_ == '-')
        .groupBy(x => x)
        .mapValues(_.length)
        .groupBy(_._2)
        .mapValues(_.keys.toList.sorted)
        .toList
        .sortBy(_._1)
        .reverse
        .flatMap(_._2)
        .take(5)
        .mkString
      checksum == expectedChecksum
    }
  }

  val rooms = input.map {
    case roomPattern(name, sectorId, checksum) => Room(name, sectorId.toInt, checksum)
  }

  val answer1 = rooms.filter(_.isValid).map(_.sectorId).sum
  println(s"What is the sum of the sector IDs of the real rooms? $answer1")
}
