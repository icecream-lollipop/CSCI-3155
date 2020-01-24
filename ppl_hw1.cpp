def isPalindrome(s: String): Boolean = {
    for(x <- 0 until s.size){
      if(s(x) != s(s.size-x-1)){
        return false
      }
    }
    return true
 }

def isPalindromeRec(s: String): Boolean = {
  if(s.size <= 1 || (s.head == s.last && isPalindromeRec(s.substring(1, s.size - 1)))){
    return true
  }
  return false
}





def isOddFunction(f: Int => Int, n: Int): Boolean = {
    for(x <- 1 until (n + 1)){
      if(f(-x) != -f(x)){
        return false
      }
    }
    return true
}








class Complex(val real: Double, val im: Double){
  override def toString(): String = {
    s"$real + i $im"
  }

  def modulus(): Double = {
    math.sqrt(real*real + im*im)
  }

  def + (r: Complex) : Complex = {
    return new Complex(r.real + real, r.im + im)
  }

  def - (r: Complex): Complex = {
     return new Complex(real - r.real, im - r.im)
  }

  def * (r: Complex): Complex = {
      return new Complex(real * r.real - im * r.im, real * r.im + im * r.real)
  }

  def inverse() : Complex = {
      return new Complex(real / (math.pow(real, 2) + math.pow(im, 2)), - im / (math.pow(real, 2) + math.pow(im, 2)))
  }
}











def calculateRoot(x0: Double): Double = {
  def f(x: Double) = {math.pow(x,2) - 3*x + 2.0}
  def df(x: Double) = {2*x - 3}
  var xi = x0
  while(math.abs(f(xi)) > math.pow(10,-8)){
    xi = xi - f(xi) / df(xi)
  }
  return xi
}


def calculateRootRec(x: Double): Double = {
  def f(x: Double) = {math.pow(x,2) - 3*x + 2.0}
  def df(x: Double) = {2*x - 3}
  if(math.abs(f(x)) > math.pow(10,-8)){
    return calculateRootRec(x - f(x) / df(x))
  }
  return x
}

def solveEquationNewtonRaphson(x: Double, f: Double => Double, df: Double => Double): Double = {
  if(math.abs(f(x)) > math.pow(10,-8)){
    return solveEquationNewtonRaphson(x - f(x) / df(x), f, df)
  }
  return x
}
