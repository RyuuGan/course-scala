package sandbox.greeter

class Greeter(val name: String) {

  var prefix = "Hello, "
  var suffix = "!"

  def greeting = prefix + name + suffix

  def greet() {
    println(greeting)
  }

}

class ParameterizedGreeter(val name: String,
                           var prefix: String,
                           var suffix: String) {

  def greeting = prefix + name + suffix

  def greet() {
    println(greeting)
  }

}

class MutableGreeter {

  var name: String = "World"
  var prefix: String = "Hello, "
  var suffix: String = "!"

  def greeting = prefix + name + suffix

  def greet() {
    println(greeting)
  }

}
