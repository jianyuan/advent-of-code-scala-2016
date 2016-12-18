/**
  * Created by jianyuanlee on 18/12/2016.
  */
object Day7 extends App {

  val hypernetPattern = """\[([a-z]+)\]""".r

  def isABBA(input: String): Boolean = input.sliding(4).exists(x => x(0) == x(3) && x(1) == x(2) && x(0) != x(1))

  case class IpAddress(ip: String) {
    lazy val supernets: List[String] = hypernetPattern.split(ip).toList
    lazy val hypernets: List[String] = hypernetPattern.findAllIn(ip).toList
    lazy val supportsTLS: Boolean = supernets.exists(isABBA) && !hypernets.exists(isABBA)
  }

  val input = io.Source.fromInputStream(io.Source.getClass.getResourceAsStream("/day7.txt")).getLines().toList
  val ipAddresses = input.map(IpAddress.apply)

  val answer1 = ipAddresses.count(_.supportsTLS)
  println(s"How many IPs in your puzzle input support TLS? $answer1")
}
