package discsort
import scalaz._, Scalaz._
import shapeless._, poly._

// Bungled attempt at implementing generic discrimiated sort,
// based on the following paper;
// www.diku.dk/hjemmesider/ansatte/henglein/papers/henglein2011a.pdf

abstract class SOrder[A]
case class Nat0(i: Int) extends SOrder[Int]
case class Triv0[A]() extends SOrder[A]
case class SumL[A,B](t1: SOrder[A], t2: SOrder[B]) extends SOrder[A\/B]
case class ProdL[A,B](t1: SOrder[A], t2: SOrder[B]) extends SOrder[(A,B)]
case class Map0[A,B](f: A => B, t2: SOrder[B]) extends SOrder[A]
case class ListL[A](t: SOrder[A]) extends SOrder[List[A]]

object SOrder {
   type Disc[A,B] = List[(A,B)] => List[B]
   val ordUnit = Triv0[Unit]()
   val ordNat8 = Nat0(255)
   val ordNat16 = Nat0(65535)
   val ordInt32 = Map0(splitW compose {x:Int => x+(-2147483648)},
                       ProdL(ordNat16, ordNat16))
   def splitW: Int => (Int,Int) = x => ((x>>16) & 65535,x & 65535)
   val ordChar8 = Map0({x: Char => x.toInt},ordNat8)

   def sdisc[A,B](ord: SOrder[A], xs: List[(A,B)]): List[B] = ord match {
      case n: Nat0 => sdiscNat(n,xs)
      case SumL(orda,ordb) => sdiscSum(orda,ordb,xs)
      case ProdL(orda,ordb) => sdiscProd(orda,ordb,xs)
      case Map0(f,ordb) => sdiscMap(f,ordb,xs)
   }

   def sdiscMap[A,B,C](f: A => B, ordb: SOrder[B], xs: List[(A,C)]): List[C] = {
      val xs_reassoc = xs map { case (a,c) => (f(a),c) }
      sdisc(ordb,xs_reassoc)
   }

   def sdiscProd[A,B,C](orda: SOrder[A], ordb: SOrder[B], xs: List[((A,B),C)]): List[C] = {
      val xs_reassoc = xs map { case ((a,b),c) => (a,(b,c)) }
      sdisc(orda,xs_reassoc) |> { sdisc(ordb,_) }
   }
   def sdiscSum[A,B,C](orda: SOrder[A], ordb: SOrder[B], xs: List[(A\/B,C)]): List[C] = {
      val (lefts,rights) =
         xs.foldLeft((List[(A,C)](),List[(B,C)]())) { (acc,x) =>
            if(x._1.isLeft) ((((~x._1).toOption.get,x._2)) +: acc._1, acc._2)
            else (acc._1, ((x._1.toOption.get,x._2)) +: acc._2)
         }
      sdisc(orda,lefts.reverse) ++ sdisc(ordb,rights.reverse)
   }

   val v1 = List[(Int\/Int,Int)]((4.left,4),(3.left,3),(5.right,5),(6.right,6),(1.right,1))
   val res1: List[Int] = sdisc(SumL(Nat0(255),Nat0(255)),v1)

   val v2 = List[((Int,Int),Int)](((4,5),4),((3,6),3),((3,2),5),((1,10),6),((9,3),1))
   val res2: List[Int] = sdisc(ProdL(Nat0(255),Nat0(255)),v2)

   val v3 = List[(Char,Int)](('y',5),('h',1),('c',3),('l',1),('t',9))
   val res3: List[Int] = sdisc(ordChar8,v3)

   def sdiscNat[A](z: Nat0, xs: List[(Int,A)]): List[A] =
      accumArray[A]( { (xs: List[A], x: A) => x :: xs }, List(), (0,z.i-1),  xs)

   // Dirty bucket sort
   def accumArray[B](f: (List[B],B) => List[B],
                       init: List[B],
                       bounds: (Int,Int),
                       xs: List[(Int,B)]): List[B] = {
      val arr = new Array[List[B]](bounds._2+1)
      xs.foreach( { arg: (Int,B) =>
         val idx = arg._1
         val l: List[B] = arr(idx)
         val nl = if(l == null) f(init,arg._2) else f(l,arg._2)
         arr(idx) = nl
      } )
      arr.toList.filter(_!=null).flatten
   }
}
