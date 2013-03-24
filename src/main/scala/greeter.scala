package sandbox

class Greeter(val name: String) {

  var prefix = "Hello, "
  var suffix = "!"

  def greeting = prefix + name + suffix

  def greet() {
    println(greeting)
  }

  override def toString = greeting

}

class ParamGreeter(val name: String,
                   var prefix: String = "Hello, ",
                   var suffix: String = "!") {

  def greeting = prefix + name + suffix

  def greet() {
    println(greeting)
  }

  override def toString = greeting

}

class MutableGreeter {

  var name: String = "World"
  var prefix: String = "Hello, "
  var suffix: String = "!"

  def greeting = prefix + name + suffix

  def greet() {
    println(greeting)
  }

  override def toString = greeting

}

class ImmutableGreeter(val name: String = "World",
                       val prefix: String = "Hello, ",
                       val suffix: String = "!") {

  def withName(newName: String) =
    new ImmutableGreeter(newName, prefix, suffix)

  def withPrefix(newPrefix: String) =
    new ImmutableGreeter(name, newPrefix, suffix)

  def withSuffix(newSuffix: String) =
    new ImmutableGreeter(name, prefix, newSuffix)

  def greeting = prefix + name + suffix

  def greet() {
    println(greeting)
  }

  override def toString = greeting

}
