/**
  * Created by jianyuanlee on 15/12/2016.
  */
object Day1 extends App {

  sealed trait Turn

  case object Left extends Turn

  case object Right extends Turn

  sealed trait Direction {
    def turn(turn: Turn): Direction = (this, turn) match {
      case (North, Left) => West
      case (North, Right) => East
      case (South, Left) => East
      case (South, Right) => West
      case (East, Left) => North
      case (East, Right) => South
      case (West, Left) => South
      case (West, Right) => North
    }
  }

  case object North extends Direction

  case object South extends Direction

  case object East extends Direction

  case object West extends Direction

  case class Coordinate(x: Int, y: Int) {
    lazy val distance: Int = x.abs + y.abs

    def move(direction: Direction, steps: Int): Coordinate = direction match {
      case North => Coordinate(x, y + steps)
      case South => Coordinate(x, y - steps)
      case East => Coordinate(x + steps, y)
      case West => Coordinate(x - steps, y)
    }
  }

  case class Instruction(turn: Turn, steps: Int)

  case class State(coordinate: Coordinate, direction: Direction) {
    def apply(instruction: Instruction): State = {
      val newDirection = direction.turn(instruction.turn)
      val newCoordinate = coordinate.move(newDirection, instruction.steps)
      State(newCoordinate, newDirection)
    }
  }

  val instructionPattern = """(L|R)(\d+)""".r

  def parseInstructions(rawInstructions: String): Seq[Instruction] = rawInstructions.split("""\s*,\s*""").map { rawInstruction =>
    val instructionPattern(rawTurn, rawSteps) = rawInstruction
    val turn = rawTurn match {
      case "L" => Left
      case "R" => Right
    }
    Instruction(turn, rawSteps.toInt)
  }

  val input = io.Source.fromInputStream(io.Source.getClass.getResourceAsStream("/day1.txt")).mkString
  val instructions = parseInstructions(input)

  val initialState = State(Coordinate(0, 0), North)

  val steps = instructions.scanLeft(initialState) { (state, instruction) => state.apply(instruction) }

  println(s"How many blocks away is Easter Bunny HQ? ${steps.last.coordinate.distance}")
}
