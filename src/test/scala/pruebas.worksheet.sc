import scala.util.Random
import common._
import Benchmark._
import math._
type Matriz = Vector[Vector[Int]]

val random = new Random()

def matrizAlAzar(long: Int, vals: Int): Matriz = {
  val v = Vector.fill(long, long) { random.nextInt(vals) }
  v
}

def vectorAlAzar(long: Int, vals: Int): Vector[Int] = {
  val v = Vector.fill(long) { random.nextInt(vals) }
  v
}

def productoPunto(v1: Vector[Int], v2: Vector[Int]): Int = {
  (v1 zip v2).map({ case (i, j) => (i * j) }).sum
}

def transpuesta(m: Matriz): Matriz = {
  val l = m.length
  Vector.tabulate(l, l)((i, j) => m(j)(i))
}

def multMatriz(m1: Matriz, m2: Matriz): Matriz = {
  val l = m1.length
  val T = transpuesta(m2)
  Vector.tabulate(l, l)((i, j) => productoPunto(m1(i), T(j)))
}

val m1 = Vector(
  Vector(1, 1, 0, 1),
  Vector(0, 1, 1, 0),
  Vector(0, 0, 0, 1),
  Vector(1, 0, 0, 1)
)

val m2 = Vector(
  Vector(1, 0, 0, 1),
  Vector(1, 1, 0, 1),
  Vector(0, 1, 0, 1),
  Vector(1, 1, 0, 1)
)
//val l = m1.length

val m4 = matrizAlAzar(8, 2)

/*m1 map(vector=>transpuesta(m2) map(vector2=> productoPunto(vector,vector2)))
Vector.tabulate(l,l)((i,j)=>productoPunto(m1(i),transpuesta(m2)(j)))*/

def multMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
  val l = m1.length
  val T = transpuesta(m2)
  Vector.tabulate(l, l)((i, j) => task(productoPunto(m1(i), T(j)))) map (v =>
    v map (_.join)
  )
}

def subMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz =
  Vector.tabulate(l, l)((z, r) => m(z + i)(r + j))

def sumMatriz(m1: Matriz, m2: Matriz): Matriz =
  Vector.tabulate(m1.length, m1.length)((i, j) => m1(i)(j) + m2(i)(j))

def multMatrizRec(m1: Matriz, m2: Matriz): Matriz = {

  if (m1.length == 1) {
    Vector(Vector(m1(0)(0) * m2(0)(0))): Matriz
  } else {

    val l = m1.length
    val mitad = l / 2
    val A11 = subMatriz(m1, 0, 0, mitad)
    val A12 = subMatriz(m1, 0, mitad, mitad)
    val A21 = subMatriz(m1, mitad, 0, mitad)
    val A22 = subMatriz(m1, mitad, mitad, mitad)
    val B11 = subMatriz(m2, 0, 0, mitad)
    val B12 = subMatriz(m2, 0, mitad, mitad)
    val B21 = subMatriz(m2, mitad, 0, mitad)
    val B22 = subMatriz(m2, mitad, mitad, mitad)
    val C11 = sumMatriz(multMatrizRec(A11, B11), multMatrizRec(A12, B21))
    val C12 = sumMatriz(multMatrizRec(A11, B12), multMatrizRec(A12, B22))
    val C21 = sumMatriz(multMatrizRec(A21, B11), multMatrizRec(A22, B21))
    val C22 = sumMatriz(multMatrizRec(A21, B12), multMatrizRec(A22, B22))

    Vector.tabulate(l, l)((i, j) =>
      if (i < mitad && j < mitad) C11(i)(j)
      else if (i < mitad) C12(i)(j - mitad)
      else if (j < mitad) C21(i - mitad)(j)
      else C22(i - mitad)(j - mitad)
    )
  }
}

/*def multMatrizRecPar(m1: Matriz, m2: Matriz): Matriz = {
  val l = m1.length
  if (l == 1) {
    Vector(Vector(m1(0)(0) * m2(0)(0))): Matriz
  } else {
    val mitad = l / 2
    val A11 = subMatriz(m1, 0, 0, mitad)
    val A12 = subMatriz(m1, 0, mitad, mitad)
    val A21 = subMatriz(m1, mitad, 0, mitad)
    val A22 = subMatriz(m1, mitad, mitad, mitad)
    val B11 = subMatriz(m2, 0, 0, mitad)
    val B12 = subMatriz(m2, 0, mitad, mitad)
    val B21 = subMatriz(m2, mitad, 0, mitad)
    val B22 = subMatriz(m2, mitad, mitad, mitad)
    val (c11, c12, c21, c22) = parallel(
      sumMatriz(multMatrizRec(A11, B11), multMatrizRec(A12, B21)),
      sumMatriz(multMatrizRec(A11, B12), multMatrizRec(A12, B22)),
      sumMatriz(multMatrizRec(A21, B11), multMatrizRec(A22, B21)),
      sumMatriz(multMatrizRec(A21, B12), multMatrizRec(A22, B22))
    )

    Vector.tabulate(l, l)((i, j) =>
      if (i < mitad && j < mitad) c11(i)(j)
      else if (i < mitad) c12(i)(j - mitad)
      else if (j < mitad) c21(i - mitad)(j)
      else c22(i - mitad)(j - mitad)
    )
  }
}*/

def crearMatrizC(
    c11: Matriz,
    c12: Matriz,
    c21: Matriz,
    c22: Matriz,
    mitad:Int
): Matriz = {

  Vector.tabulate(4, 4)((i, j) =>
    if (i < mitad && j < mitad) c11(i)(j)
    else if (i < mitad) c12(i)(j - mitad)
    else if (j < mitad) c21(i - mitad)(j)
    else c22(i - mitad)(j - mitad)
  )
}

def multMatrizRecPar(m1: Matriz, m2: Matriz): Matriz = {
  val umbral = pow(2, 3)
  val l = m1.length
  if (l == 1) {
    Vector(Vector(m1(0)(0) * m2(0)(0))): Matriz
  } else {
    val mitad = l / 2
    val A11 = subMatriz(m1, 0, 0, mitad)
    val A12 = subMatriz(m1, 0, mitad, mitad)
    val A21 = subMatriz(m1, mitad, 0, mitad)
    val A22 = subMatriz(m1, mitad, mitad, mitad)
    val B11 = subMatriz(m2, 0, 0, mitad)
    val B12 = subMatriz(m2, 0, mitad, mitad)
    val B21 = subMatriz(m2, mitad, 0, mitad)
    val B22 = subMatriz(m2, mitad, mitad, mitad)
    if (l <= umbral) {
      val c11 = sumMatriz(multMatrizRec(A11, B11), multMatrizRec(A12, B21))
      val c12 = sumMatriz(multMatrizRec(A11, B12), multMatrizRec(A12, B22))
      val c21 = sumMatriz(multMatrizRec(A21, B11), multMatrizRec(A22, B21))
      val c22 = sumMatriz(multMatrizRec(A21, B12), multMatrizRec(A22, B22))
      crearMatrizC(c11, c12, c21, c22, mitad)
    } else {
      val (c11, c12, c21, c22) = parallel(
        sumMatriz(multMatrizRec(A11, B11), multMatrizRec(A12, B21)),
        sumMatriz(multMatrizRec(A11, B12), multMatrizRec(A12, B22)),
        sumMatriz(multMatrizRec(A21, B11), multMatrizRec(A22, B21)),
        sumMatriz(multMatrizRec(A21, B12), multMatrizRec(A22, B22))
      )
      crearMatrizC(c11, c12, c21, c22, mitad)
    }
  }
}

multMatrizRec(m1, m2)
multMatriz(m1, m2)
multMatrizRecPar(m1, m2)

val mp1 = matrizAlAzar(pow(2, 3).toInt, 2)
val mp2 = matrizAlAzar(pow(2, 3).toInt, 2)

compararAlgoritmos(multMatrizRec, multMatrizRecPar)(mp1, mp2)

