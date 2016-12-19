/**
  * Created by jianyuanlee on 19/12/2016.
  */
object Day8 extends App {

  sealed trait Instruction {
    def nextState(state: State): State
  }

  case class Rect(a: Int, b: Int) extends Instruction {
    override def nextState(state: State): State = {
      val newScreen: List[List[Boolean]] = state.screen.zipWithIndex.map {
        case (row, y) => row.zipWithIndex.map {
          case (pixelState, x) => if (x < a && y < b) true else pixelState
        }
      }
      state.copy(screen = newScreen)
    }
  }

  case class RotateRow(y: Int, by: Int) extends Instruction {
    override def nextState(state: State): State = {
      val newScreen: List[List[Boolean]] = state.screen.zipWithIndex.map {
        case (row, yy) if yy == y => row.zipWithIndex.map {
          case (pixelState, xx) => state.screen(yy)((xx + state.screenA - (by % state.screenA)) % state.screenA)
        }
        case (row, _) => row
      }
      state.copy(screen = newScreen)
    }
  }

  case class RotateColumn(x: Int, by: Int) extends Instruction {
    override def nextState(state: State): State = {
      val newScreen: List[List[Boolean]] = state.screen.zipWithIndex.map {
        case (row, yy) => row.zipWithIndex.map {
          case (pixelState, xx) if xx == x => state.screen((yy + state.screenB - (by % state.screenB)) % state.screenB)(xx)
          case (pixelState, _) => pixelState
        }
      }
      state.copy(screen = newScreen)
    }
  }

  case class State(screen: List[List[Boolean]]) {
    val screenA: Int = screen.head.length
    val screenB: Int = screen.length

    def renderScreen: String = screen.map {
      _.map {
        case false => ' '
        case true => '#'
      }.mkString
    }.mkString("\n")

    def apply(instruction: Instruction): State = instruction.nextState(this)

    lazy val numLit: Int = screen.flatten.count(identity)
  }

  val input = io.Source.fromInputStream(io.Source.getClass.getResourceAsStream("/day8.txt")).getLines().toList

  val rectPattern = """rect (\d+)x(\d+)""".r
  val rotateRowPattern = """rotate row y=(\d+) by (\d+)""".r
  val rotateColumnPattern = """rotate column x=(\d+) by (\d+)""".r
  val instructions = input.map {
    case rectPattern(a, b) => Rect(a.toInt, b.toInt)
    case rotateRowPattern(y, by) => RotateRow(y.toInt, by.toInt)
    case rotateColumnPattern(x, by) => RotateColumn(x.toInt, by.toInt)
  }

  val screen: List[List[Boolean]] = List.fill(6, 50)(false)

  val finalState = instructions.foldLeft(State(screen))((state, instruction) => state(instruction))

  val answer1 = finalState.numLit
  println(s"How many pixels should be lit? $answer1")

  println(finalState.renderScreen)
}
