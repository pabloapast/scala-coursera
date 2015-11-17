import math.abs

def sum(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (b < a) acc
    else loop(a + 1, acc + f(a))
  }
  loop(a, 0)
}

def id(x: Int): Int = x
sum(id)(3, 5)
sum(id)(3, 5)
sum(id)(5, 5)
sum(id)(6, 5)


def product(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (b < a) acc
    else loop(a + 1, acc * f(a))
  }
  loop(a, 1)
}

product((x: Int) => x)(1, 2)
product((x: Int) => x)(2, 2)
product((x: Int) => x)(1, 5)


def factorial(n: Int) = product(x => x)(1, n)

factorial(5)

// ----------------------------------------------------

val tolerance = 0.0001

def isCloseEnough(x: Double, y: Double) =
  abs((x- y) / x) / x < tolerance

def fixedPoint(f: Double => Double)(firsGuess: Double) = {
  def iterate(guess: Double): Double = {
    val next = f(guess)
    if(isCloseEnough(guess, next)) next
    else iterate(next)
  }
  iterate(firsGuess)
}

fixedPoint(x => 1 + x/2)(1)

def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

def sqrt(x: Double) = {
  fixedPoint(averageDamp(y => x / y))(1)
}

sqrt(2)


// ----------------------------------------------------

type Set = Int => Boolean


def contains(s: Set, elem: Int): Boolean = s(elem)

def singletonSet(elem: Int): Set = (x: Int) => x == elem

def s = singletonSet(3)
contains(s, 3)
contains(s, 5)
contains(s, -3)



