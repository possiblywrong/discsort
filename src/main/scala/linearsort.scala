package discsort
import scalaz._, Scalaz._
import shapeless._, poly._
import util.Random._

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
   val ordUnit = Triv0[Unit]()
   val ordNat8 = Nat0(255)
   val ordNat16 = Nat0(65535)
   val ordInt32 = Map0(splitW compose {x:Int => x+(-2147483648)},
                       ProdL(ordNat16, ordNat16))
   def splitW: Int => (Int,Int) = x => ((x>>16) & 65535,x & 65535)
   val ordChar8 = Map0({x: Char => x.toInt},ordNat8)
   val ordString8 = ListL(ordChar8)

   def sdisc[A,B](ord: SOrder[A], xs: List[(A,B)]): List[List[B]] =
      xs match {
         case Nil => Nil
         case List((_,v)) => List(List(v))
         case _ => ord match {
            case Triv0() => List(xs map { _._2 })
            case n: Nat0 => sdiscNat(n,xs)
            case SumL(orda,ordb) => sdiscSum(orda,ordb,xs)
            case ProdL(orda,ordb) => sdiscProd(orda,ordb,xs)
            case Map0(f,ordb) => sdiscMap(f,ordb,xs)
            case ListL(orda) => sdiscList(orda,xs)
         }
      }

   def sdiscList[A,B](orda: SOrder[A], xs: List[(List[A],B)]): List[List[B]] = {
      def stripPartition: List[A] => (Unit\/(A,List[A]))  =
         { l => l.headOption.cata( { x: A => (x,l.tail).right[Unit] },
                                   { ().left[(A,List[A])] } ) }
      sdisc(Map0(stripPartition, SumL(ordUnit,ProdL(orda,ListL(orda)))), xs)
   }

   def sdiscMap[A,B,C](f: A => B, ordb: SOrder[B], xs: List[(A,C)]): List[List[C]] = {
      val xs_reassoc = xs map { case (a,c) => (f(a),c) }
//      println(xs_reassoc)
      sdisc(ordb,xs_reassoc)
   }

   def sdiscProd[A,B,C](orda: SOrder[A], ordb: SOrder[B], xs: List[((A,B),C)]): List[List[C]] = {
      val xs_reassoc = xs map { case ((a,b),c) => (a,(b,c)) }
      for {
         ys <- sdisc(orda,xs_reassoc)
//         j = println("ys: "+ys)
         vs <- sdisc(ordb,ys)
//         k = println("vs: "+vs)
      } yield vs
   }
   def sdiscSum[A,B,C](orda: SOrder[A], ordb: SOrder[B], xs: List[(A\/B,C)]): List[List[C]] = {
      val (lefts,rights) =
         xs.foldLeft((List[(A,C)](),List[(B,C)]())) { (acc,x) =>
            if(x._1.isLeft) ((((~x._1).toOption.get,x._2)) +: acc._1, acc._2)
            else (acc._1, ((x._1.toOption.get,x._2)) +: acc._2)
         }
      sdisc(orda,lefts.reverse) ++ sdisc(ordb,rights.reverse)
   }

   val v1 = List[(Int\/Int,Int)]((4.left,4),(3.left,3),(5.right,5),(6.right,6),(1.right,1))
   lazy val resA: List[Int] = sdisc(SumL(Nat0(255),Nat0(255)),v1).flatten

   val v2 = List[((Int,Int),Int)](((4,5),4),((3,6),3),((3,2),5),((1,10),6),((9,3),1))
   lazy val resB: List[Int] = sdisc(ProdL(Nat0(255),Nat0(255)),v2).flatten

   val v3 = List[(Char,Int)](('y',5),('h',1),('c',3),('l',1),('t',9))
   lazy val resC: List[Int] = sdisc(ordChar8,v3).flatten

   val v4 = List[(String,String)](("bb","bb"),("ab","ab"),("aa","aa"),("ba","ba"))
   lazy val resD: List[String] = sdisc(ordString8,v4.map { x => (x._1.toList,x._2) }).flatten

   val data10 = List.fill(10)(nextInt)
   val data10t = data10.zip(data10)
   val data1000 = List.fill(1000)(nextInt)
   val data1000t = data1000.zip(data1000)
   val data10000 = List.fill(10000)(nextInt)
   val data10000t = data10000.zip(data10000)
   val data1000000 = List.fill(1000000)(nextInt)
   val data1000000t = data1000000.zip(data1000000)

   def sdiscNat[A](z: Nat0, xs: List[(Int,A)]): List[List[A]] =
      accumArray[A]( { (xs: List[A], x: A) => x :: xs }, List(), (0,z.i),  xs)

   // Dirty bucket sort
   def accumArray[B](f: (List[B],B) => List[B],
                       init: List[B],
                       bounds: (Int,Int),
                       xs: List[(Int,B)]): List[List[B]] = {
      val arr = new Array[List[B]](bounds._2+1)
      xs.foreach( { arg: (Int,B) =>
         val idx = arg._1
         val l: List[B] = arr(idx)
         val nl = if(l == null) f(init,arg._2) else f(l,arg._2)
         arr(idx) = nl
      } )
      arr.toList.filter(_!=null)
   }

   def time[A](f: => A) = {
      val s = System.nanoTime
      val ret = f
      println("time: "+(System.nanoTime-s)/1e6+"ms")
      ret
   }
}
