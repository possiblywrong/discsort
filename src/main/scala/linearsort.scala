package discsort
import scala.collection.mutable.MutableList
import scalaz._, Scalaz._
import util.Random._
import kadai.cmdopts._, CmdOpts._

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
            case n: Nat0 => sdiscNat(n,xs) |> { arr =>
                               val mlist = MutableList[List[B]]()
                               arr.foreach { a: MutableList[B] => if(a != null) mlist += a.toList }
                               mlist.toList
                            }
            case SumL(orda,ordb) => sdiscSum(orda,ordb,xs)
            case ProdL(orda,ordb) => sdiscProd(orda,ordb,xs)
            case Map0(f,ordb) => sdiscMap(f,ordb,xs)
            case ListL(orda) => sdiscList(orda,xs)
         }
      }

   def stripPartition[A](l: List[A]): (Unit\/(A,List[A]))  =
      l.headOption.cata( { x: A => (x,l.tail).right[Unit] },
                         { ().left[(A,List[A])] } )

   def sdiscList[A,B](orda: SOrder[A], xs: List[(List[A],B)]): List[List[B]] = {
      sdisc(Map0(stripPartition[A], SumL(ordUnit,ProdL(orda,ListL(orda)))), xs)
   }

   def sdiscMap[A,B,C](f: A => B, ordb: SOrder[B], xs: List[(A,C)]): List[List[C]] = {
      val xs_reassoc = xs map { case (a,c) => (f(a),c) }
      sdisc(ordb,xs_reassoc)
   }

   def flipTuple[A,B,C](t: ((A,B),C)): (A,(B,C))  = t match { case ((a,b),c) => (a,(b,c)) }

   def sdiscProd[A,B,C](orda: SOrder[A], ordb: SOrder[B], xs: List[((A,B),C)]): List[List[C]] = {
      val xs_reassoc = xs map flipTuple
      for { ys <- sdisc(orda,xs_reassoc); vs <- sdisc(ordb,ys) } yield vs
   }
   def sdiscSum[A,B,C](orda: SOrder[A], ordb: SOrder[B], xs: List[(A\/B,C)]): List[List[C]] = {
      val lefts  = MutableList[(A,C)]()
      val rights = MutableList[(B,C)]()
      xs.foreach { x =>
         x._1.bimap( { l => lefts  += ((l,x._2)) }
                    ,{ r => rights += ((r,x._2)) } )
      }
      sdisc(orda,lefts.toList) ++ sdisc(ordb,rights.toList)
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

   def sdiscNat[A](z: Nat0, xs: List[(Int,A)]): Array[MutableList[A]] = {
      val arr = new Array[MutableList[A]](z.i+1)
      xs.foreach { arg: (Int,A) =>
         val idx = arg._1
         if(arr(idx) == null) arr(idx) = MutableList[A](arg._2)
         else arr(idx) += arg._2
      }
      arr
   }

   def time[A](f: => A) = {
      val s = System.nanoTime
      val ret = f
      println("time: "+(System.nanoTime-s)/1e6+"ms")
      ret
   }
}

object Main extends App {
   import SOrder._

   object CArgs extends CmdOpts(args) {
      lazy val cmpsort = opt("--cmp",TRUE) | false
      lazy val strings = opt("--string",TRUE) | false
      lazy val file = opt("--file",((x: String) => x)) | "words.txt"
      lazy val size = opt("--size",((x: String) => x.toInt)) | 1000
      override def version = opt("--version", () => "0.1")
   }

   if(CArgs.strings){
      val words: List[String] = scala.io.Source.fromFile(CArgs.file)
                    .getLines
                    .flatMap(_.split("\\W+")).toList
      val wordst: List[(List[Char],String)] = words.zip(words).map { x => (x._1.toList,x._2) }
      if(CArgs.cmpsort) while(true) { time{ words.sorted } }
      else              while(true) { time{ sdisc(ordString8,wordst) } }
   } else {
      val data = List.fill(CArgs.size)(nextInt)
      val datat = data.zip(data)
      if(CArgs.cmpsort) while(true) { time{ data.sorted } }
      else              while(true) { time{ sdisc(ordInt32,datat) } }
   }
}
