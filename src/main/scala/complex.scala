package sandbox

class Complex(val re: Double,
              val im: Double) {

  // Polar

  val modulus = math.sqrt(re * re + im * im)
  val argument = math.atan2(im, re)

  // Arithmetic operations

  def conjugate = new Complex(re, -im)

  def +(that: Complex): Complex =
    new Complex(this.re + that.re, this.im + that.im)

  def -(that: Complex): Complex =
    new Complex(this.re - that.re, this.im - that.im)

  def *(that: Complex): Complex =
    new Complex(
      this.re * that.re + this.im * that.im,
      this.re * that.im - this.im * that.re)

  def /(that: Complex): Complex = {
    require(that.re != 0)
    val n = this * that.conjugate
    val d = that.re * that.re + that.im * that.im
    new Complex(n.re / d, n.im / d)
  }

  // Unary operations

  def unary_- = new Complex(-re, -im)

  def unary_+ = new Complex(re, im)

  // Comparison

  override def equals(obj: Any) = obj match {
    case that: Complex =>
      this.re == that.re && this.im == that.im
    case number: Numeric =>
      this.re == number && this.im == 0
    case _ =>
  }

  def <(that: Complex) = this.modulus < that.modulus

  def <=(that: Complex) = this < that || this == that

  def >=(that: Complex) = !(this < that)

  def >(that: Complex) = this >= that && this != that

  // Printing

  def realPart: String =
    if (re == 0) ""
    else re.toString

  def imaginaryPart: String = {
    var result = ""
    if (im > 0 && re != 0) result += " + "
    if (im < 0) result += " - "
    if (im != 0) {
      result += math.abs(im)
      result += "i"
    }
    result
  }

  override def toString = realPart + imaginaryPart


}