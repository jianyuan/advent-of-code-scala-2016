/**
  * Created by jianyuanlee on 16/12/2016.
  */
object Day2 extends App {

  sealed trait Direction

  case object Up extends Direction

  case object Down extends Direction

  case object Left extends Direction

  case object Right extends Direction

  type KeyPad = List[List[Char]]

  val keyPad1: KeyPad = List("123", "456", "789").map(_.toList)
  val keyPad2: KeyPad = List("  1  ", " 234 ", "56789", " ABC ", "  D  ").map(_.toList)

  val input = io.Source.fromInputStream(io.Source.getClass.getResourceAsStream("/day2.txt")).getLines().toList

  case class State(x: Int, y: Int, keyPad: KeyPad) {
    require(keyPad.nonEmpty)
    require(keyPad.forall(row => row.nonEmpty && row.size == keyPad.head.size))
    require(y >= 0 && keyPad.size > y)
    require(x >= 0 && keyPad.head.size > x)

    lazy val key: Char = keyPad(y)(x)

    def move(direction: Direction): State = {
      val nextState = direction match {
        case Up if y > 0 => copy(y = y - 1)
        case Down if y < keyPad.size - 1 => copy(y = y + 1)
        case Left if x > 0 => copy(x = x - 1)
        case Right if x < keyPad.head.size - 1 => copy(x = x + 1)
        case _ => this
      }
      if (nextState.key == ' ') this else nextState
    }
  }

  val instructions = input.map(_.map {
    case 'U' => Up
    case 'D' => Down
    case 'L' => Left
    case 'R' => Right
  })

  val answer1 = instructions.scanLeft(State(1, 1, keyPad1)) { (state, instructions) =>
    instructions.foldLeft(state) { (state, direction) => state.move(direction) }
  }.tail.map(_.key).mkString
  println(s"Part 1: What is the bathroom code? $answer1")

  val answer2 = instructions.scanLeft(State(0, 2, keyPad2)) { (state, instructions) =>
    instructions.foldLeft(state) { (state, direction) => state.move(direction) }
  }.tail.map(_.key).mkString
  println(s"Part 2: What is the bathroom code? $answer2")
}
