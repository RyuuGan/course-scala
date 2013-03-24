package sandbox

class Fraction(n: Int, d: Int) {
  require(d != 0)

  // Signs are corrected

  val numerator = if (d < 0) -n else n
  val denominator = math.abs(d)

  // Decimal form

  val decimal: Double = numerator.toDouble / denominator

  // Greatest common divisor (Euclidean algorithm)

  def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  def gcd: Int = gcd(numerator, denominator)

  // Reduction

  def isReduced = gcd == 1

  def reduced: Fraction =
    if (isReduced) new Fraction(numerator, denominator)
    else new Fraction(numerator / gcd, denominator / gcd)

  // Improper fractions

  def integer: Int = numerator / denominator
  def remainder: Fraction = new Fraction(numerator % denominator, denominator)

  // Inversed

  def inversed = new Fraction(denominator, numerator)

  // Arithmetic operations

  def abs: Fraction =
    if (numerator >= 0) new Fraction(numerator, denominator)
    else new Fraction(-numerator, denominator)

  def +(that: Fraction) =
    new Fraction(
      this.numerator * that.denominator +
          this.denominator * that.numerator,
      this.denominator * that.denominator)
        .reduced

  def -(that: Fraction) = this + (-that)

  def *(that: Fraction) =
    new Fraction(
      this.numerator * that.numerator,
      this.denominator * that.denominator)
        .reduced

  def /(that: Fraction) = this * that.inversed

  // Unary operations

  def unary_+ = new Fraction(numerator, denominator)
  def unary_- = new Fraction(-numerator, denominator)

  // Comparison

  override def equals(obj: Any) = obj match {
    case that: Fraction =>
      this.decimal == that.decimal
    case that: Double =>
      this.decimal == that
    case _ => false
  }

  override def hashCode = this.decimal.hashCode

  def <(that: Fraction) = this.decimal < that.decimal
  def <=(that: Fraction) = (this < that) || (this == that)
  def >(that: Fraction) = !(this <= that)
  def >=(that: Fraction) = !(this < that)

  // Printing

  override def toString = {
    var result = numerator.toString
    if (denominator != 1)
      result += "/" + denominator
    result
  }

  def toImproperValue = {
    var result = ""
    val i = integer
    val r = remainder
    if (i != 0)
      result += i
    if (r.numerator != 0) {
      if (r.numerator > 0)
        result += " + "
      else result += " - "
      result += r.abs.toString
    }
    result
  }

}