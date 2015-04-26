abstract class Shape {
  def draw(f: String => Unit) = f(s"draw: $this")
}

case class Point(x: Double = 0.0, y: Double = 0.0)
case class Circle(center: Point, radius: Double) extends Shape
case class Rectangle(lowerLeft: Point, upperRight: Point) extends Shape
case class Triangle(p1: Point, p2: Point, p3: Point) extends Shape

val p00 = Point
val p20 = Point(2)
val p02 = Point(y = 2)

object Point {
  def apply(x: Double = 0, y: Double = 0) = new Point(x, y)
}
