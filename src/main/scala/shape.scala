package sandbox

trait Shape extends Comparable[Shape] {

  def perimeter: Double

  def area: Double

  def compareTo(o: Shape) = this.area.compareTo(o.area)
}

trait Radial {

  def radius: Double

  def angle: Double

  final def diameter = 2 * radius

  final def arc = radius * angle

}

final class Circle(val radius: Double)
    extends Shape
    with Radial {

  def angle = 2 * math.Pi

  def perimeter = arc

  def area = math.Pi * radius * radius

}

final class Sector(val radius: Double,
                   val angle: Double)
    extends Shape
    with Radial {

  def perimeter = diameter + arc

  def area = arc * radius / 2

}


class Triangle(val ab: Double,
               val ac: Double,
               val a: Double)
    extends Shape {

  def bc = math.sqrt(
    (ab * ab) + (ac * ac) - 2 * ab * ac * math.cos(a))

  def b = math.atan(ac / ab)

  def c = math.atan(ab / ac)

  def perimeter = ab + ac + bc

  def area = ab * ac * math.sin(a) / 2
}

class Rectangle(val ab: Double,
                val ac: Double)
    extends Shape {

  final def cd = ab

  final def bd = ac

  def bc = math.sqrt(ab * ab + ac * ac)

  final def ad = bc

  def perimeter = 2 * (ab + ac)

  def area = ab * ac

}

final class Square(val side: Double)
    extends Rectangle(side, side) {

  override def perimeter = 4 * side

  override def bc = math.sqrt(2) * side

}

