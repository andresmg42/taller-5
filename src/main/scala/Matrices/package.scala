import scala.util.Random
import common._
import math._
import scala.collection.parallel.immutable._

package object Matrices {
type Matriz=Vector[Vector [Int]]

 val random = new Random()

def matrizAlAzar(long:Int,vals:Int):Matriz={
    val v=Vector.fill(long,long){random.nextInt(vals)}
    v
}

def vectorAlAzar(long:Int,vals:Int):Vector[Int]={
    val v=Vector.fill(long){random.nextInt(vals)}
    v
}

def prodPunto(v1:Vector[Int],v2:Vector[Int]):Int={
    (v1 zip v2).map({case(i,j)=>(i*j)}).sum
}

def transpuesta(m:Matriz):Matriz={
    val l=m.length
    Vector.tabulate(l,l)((i,j)=>m(j)(i))
}


def multMatriz(m1:Matriz,m2:Matriz):Matriz={
val l=m1.length
val T=transpuesta(m2)
Vector.tabulate(l,l)((i,j)=>prodPunto(m1(i),T(j)))
}

def multMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
val l=m1.length
val T=transpuesta(m2)
 Vector.tabulate(l,l)((i,j)=>task(prodPunto(m1(i),T(j)))) map (v=>v map(_.join))
}

def subMatriz(m:Matriz,i:Int,j:Int,l:Int):Matriz= Vector.tabulate(l,l)((z,r)=>m(z+i)(r+j))

def sumMatriz(m1:Matriz,m2:Matriz):Matriz=Vector.tabulate(m1.length,m1.length)((i,j)=>m1(i)(j)+m2(i)(j))

def crearMatrizC(
    c11: Matriz,
    c12: Matriz,
    c21: Matriz,
    c22: Matriz,
    mitad:Int
): Matriz = {
  Vector.tabulate(4,4)((i, j) =>
    if (i < mitad && j < mitad) c11(i)(j)
    else if (i < mitad) c12(i)(j - mitad)
    else if (j < mitad) c21(i - mitad)(j)
    else c22(i - mitad)(j - mitad)
  )
}

def multMatrizRec(m1:Matriz,m2:Matriz):Matriz={
val l=m1.length 
if(l==1){
  Vector(Vector(m1(0)(0) * m2(0)(0))):Matriz
}else{


val mitad=l/2
val A11=subMatriz(m1,0,0,mitad)
val A12=subMatriz(m1,0,mitad,mitad)
val A21=subMatriz(m1,mitad,0,mitad)
val A22=subMatriz(m1,mitad,mitad,mitad)
val B11=subMatriz(m2,0,0,mitad)
val B12=subMatriz(m2,0,mitad,mitad)
val B21=subMatriz(m2,mitad,0,mitad)
val B22=subMatriz(m2,mitad,mitad,mitad)
val C11=sumMatriz(multMatrizRec(A11,B11),multMatrizRec(A12,B21))
val C12=sumMatriz(multMatrizRec(A11,B12),multMatrizRec(A12,B22))
val C21=sumMatriz(multMatrizRec(A21,B11),multMatrizRec(A22,B21))
val C22=sumMatriz(multMatrizRec(A21,B12),multMatrizRec(A22,B22))

crearMatrizC(C11, C12, C21, C22, mitad)

}
}

def multMatrizRecPar(m1: Matriz, m2: Matriz): Matriz = {
  val umbral = pow(2, 3)
  val l = m1.length

  if (l <= umbral) {

    multMatrizRec(m1, m2)

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
      sumMatriz(multMatrizRecPar(A11, B11), multMatrizRecPar(A12, B21)),
      sumMatriz(multMatrizRecPar(A11, B12), multMatrizRecPar(A12, B22)),
      sumMatriz(multMatrizRecPar(A21, B11), multMatrizRecPar(A22, B21)),
      sumMatriz(multMatrizRecPar(A21, B12), multMatrizRecPar(A22, B22))
    )
    crearMatrizC(c11, c12, c21, c22, l)
  }

}

/*def multMatrizRecPar(m1: Matriz, m2: Matriz): Matriz = {
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
*/
def restaMatriz(m1: Matriz, m2: Matriz): Matriz = {
  Vector.tabulate(m1.length, m1.length)((i, j) => m1(i)(j) - m2(i)(j))
}

def multStrassen(m1: Matriz, m2: Matriz): Matriz = {
  val l = m1.length
  if (l == 1) {
    Vector(Vector(m1(0)(0) * m2(0)(0))): Matriz
  } else {
    val mitad = l / 2
    val (a11, a12, a21, a22) = (
      subMatriz(m1, 0, 0, mitad),
      subMatriz(m1, 0, mitad, mitad),
      subMatriz(m1, mitad, 0, mitad),
      subMatriz(m1, mitad, mitad, mitad)
    )

    val (b11, b12, b21, b22) = (
      subMatriz(m2, 0, 0, mitad),
      subMatriz(m2, 0, mitad, mitad),
      subMatriz(m2, mitad, 0, mitad),
      subMatriz(m2, mitad, mitad, mitad)
    )

    val P1 = multStrassen(a11, restaMatriz(b12, b22))
    val P2 = multStrassen(sumMatriz(a11, a12), b22)
    val P3 = multStrassen(sumMatriz(a21, a22), b11)
    val P4 = multStrassen(a22, restaMatriz(b21, b11))
    val P5 = multStrassen(sumMatriz(a11, a22), sumMatriz(b11, b22))
    val P6 = multStrassen(restaMatriz(a12, a22), sumMatriz(b21, b22))
    val P7 = multStrassen(restaMatriz(a11, a21), sumMatriz(b11, b12))

    val (c11, c12, c21, c22) = (
      sumMatriz(sumMatriz(P5, P6), restaMatriz(P4, P2)),
      sumMatriz(P1, P2),
      sumMatriz(P3, P4),
      sumMatriz(P5, restaMatriz(restaMatriz(P1, P3), P7))
    )

    crearMatrizC(c11, c12, c21, c22, l)

  }
}

def multStrassenPar(m1: Matriz, m2: Matriz): Matriz = {
  val umbral = pow(2, 3)
  val l = m1.length

  if (l <= umbral) {

    multStrassen(m1, m2)

  } else {
    val mitad = l / 2
    val (a11, a12, a21, a22) = (
      subMatriz(m1, 0, 0, mitad),
      subMatriz(m1, 0, mitad, mitad),
      subMatriz(m1, mitad, 0, mitad),
      subMatriz(m1, mitad, mitad, mitad)
    )

    val (b11, b12, b21, b22) = (
      subMatriz(m2, 0, 0, mitad),
      subMatriz(m2, 0, mitad, mitad),
      subMatriz(m2, mitad, 0, mitad),
      subMatriz(m2, mitad, mitad, mitad)
    )
    val P1 = task(multStrassenPar(a11, restaMatriz(b12, b22)))
    val P2 = task(multStrassenPar(sumMatriz(a11, a12), b22))
    val P3 = task(multStrassenPar(sumMatriz(a21, a22), b11))
    val P4 = task(multStrassenPar(a22, restaMatriz(b21, b11)))
    val P5 = task(multStrassenPar(sumMatriz(a11, a22), sumMatriz(b11, b22)))
    val P6 = task(multStrassenPar(restaMatriz(a12, a22), sumMatriz(b21, b22)))
    val P7 = task(multStrassenPar(restaMatriz(a11, a21), sumMatriz(b11, b12)))

    val (c11, c12, c21, c22) = (
      sumMatriz(sumMatriz(P5.join, P6.join), restaMatriz(P4.join, P2.join)),
      sumMatriz(P1.join, P2.join),
      sumMatriz(P3.join, P4.join),
      sumMatriz(P5.join, restaMatriz(restaMatriz(P1.join, P3.join), P7.join))
    )

    crearMatrizC(c11, c12, c21, c22, l)

  }

}

/*
def multStrassenPar(m1: Matriz, m2: Matriz): Matriz = {
  val l = m1.length
  if (l == 1) {
    Vector(Vector(m1(0)(0) * m2(0)(0))): Matriz
  } else {
    val mitad = l / 2
    val (a11, a12, a21, a22) = (
      subMatriz(m1, 0, 0, mitad),
      subMatriz(m1, 0, mitad, mitad),
      subMatriz(m1, mitad, 0, mitad),
      subMatriz(m1, mitad, mitad, mitad)
    )

    val (b11, b12, b21, b22) = (
      subMatriz(m2, 0, 0, mitad),
      subMatriz(m2, 0, mitad, mitad),
      subMatriz(m2, mitad, 0, mitad),
      subMatriz(m2, mitad, mitad, mitad)
    )

    val P1 = task(multStrassen(a11, restaMatriz(b12, b22)))
    val P2 = task(multStrassen(sumMatriz(a11, a12), b22))
    val P3 = task(multStrassen(sumMatriz(a21, a22), b11))
    val P4 = task(multStrassen(a22, restaMatriz(b21, b11)))
    val P5 = task(multStrassen(sumMatriz(a11, a22), sumMatriz(b11, b22)))
    val P6 = task(multStrassen(restaMatriz(a12, a22), sumMatriz(b21, b22)))
    val P7 = task(multStrassen(restaMatriz(a11, a21), sumMatriz(b11, b12)))

    val (c11, c12, c21, c22) = (
      sumMatriz(sumMatriz(P5.join, P6.join), restaMatriz(P4.join, P2.join)),
      sumMatriz(P1.join, P2.join),
      sumMatriz(P3.join, P4.join),
      sumMatriz(P5.join, restaMatriz(restaMatriz(P1.join, P3.join), P7.join))
    )

    crearMatrizC(c11, c12, c21, c22, l)

  }
}
*/
def prodPuntoParD(v1:ParVector[Int],v2:ParVector[Int]):Int={
  (v1 zip v2).map({case (i,j)=>(i*j)}).sum
}


}

