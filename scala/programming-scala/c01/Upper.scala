//package c01

class Upper {
  def upper(strings: String*): Seq[String] = {
    strings.map(s => s.toUpperCase())
  }
}

val up = new Upper
println(up.upper("hello", "world"))

object UpperObj {
  def upper(strings: String*) = strings.map(_.toUpperCase)
}

println(UpperObj.upper("hello", "world"))

object UpperMain {
  def main(args: Array[String]) = {
    println(args.map(_.toUpperCase).mkString(" "))
  }
}
