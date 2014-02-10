package discsort
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scalaz._, Scalaz._
import util.Random._
import kadai.cmdopts._, CmdOpts._

// Bungled attempt at implementing generic discrimiated sort,
// based on the following paper;
// www.diku.dk/hjemmesider/ansatte/henglein/papers/henglein2011a.pdf

abstract class DiscOrder[A]
case class NatOrd(i: Int) extends DiscOrder[Int]
case class TrivOrd[A]() extends DiscOrder[A]
case class SumOrd[A,B](t1: DiscOrder[A], t2: DiscOrder[B]) extends DiscOrder[Either[A,B]]
case class ProdOrd[A,B](t1: DiscOrder[A], t2: DiscOrder[B]) extends DiscOrder[(A,B)]
case class MapOrd[A,B](f: A => B, t2: DiscOrder[B]) extends DiscOrder[A]
case class ListOrd[A](t: DiscOrder[A]) extends DiscOrder[Array[A]]

object DiscOrder {
   val ordUnit = TrivOrd[Unit]()
   val ordNat8 = NatOrd(255)
   val ordNat16 = NatOrd(65535)
   val ordInt32 = MapOrd(splitW compose {x:Int => x+(-2147483648)},
                       ProdOrd(ordNat16, ordNat16))
   def splitW: Int => (Int,Int) = x => ((x>>16) & 65535,x & 65535)
   val ordChar8 = MapOrd({x: Char => x.toInt},ordNat8)
   val ordString8 = ListOrd(ordChar8)

   def dosort[A,B:ClassTag](ord: DiscOrder[A], xs: Vector[(A,B)]): Array[Array[B]] =
      dsort(ord,xs.toArray)

   def dsort[A,B:ClassTag](ord: DiscOrder[A], xs: Array[(A,B)]): Array[Array[B]] =
      xs match {
         case Array() => Array.empty[Array[B]]
         case Array((_,v)) => val inner = new Array[B](1); inner(0) = v
                              val outer = new Array[Array[B]](1); outer(0) = inner
                              outer
         case _ => ord match {
            case TrivOrd() => val ret = new Array[Array[B]](1); ret(0) = (xs map { _._2 }); ret
            case n: NatOrd => dsortNat(n,xs)
            case SumOrd(orda,ordb) => dsortSum(orda,ordb,xs)
            case ProdOrd(orda,ordb) => dsortProd(orda,ordb,xs)
            case MapOrd(f,ordb) => dsortMap(f,ordb,xs)
            case ListOrd(orda) => dsortList(orda,xs)
         }
      }

   def stripPartition[A](l: Array[A]): (Either[Unit,(A,Array[A])])  =
      l.headOption.cata( { x: A => Right((x,l.tail)) }, { Left(()) } )

   def dsortList[A,B:ClassTag](orda: DiscOrder[A], xs: Array[(Array[A],B)]): Array[Array[B]] =
      dsort(MapOrd(stripPartition[A], SumOrd(ordUnit,ProdOrd(orda,ListOrd(orda)))), xs)

   def dsortMap[A,B,C:ClassTag](f: A => B, ordb: DiscOrder[B], xs: Array[(A,C)]): Array[Array[C]] = {
      var i = 0; val sz = xs.size
      val arr = new Array[(B,C)](sz)
      while ( i < sz ) {
         val (a,v) = xs(i)
         arr(i) = ((f(a),v))
         i += 1
      }
      dsort(ordb,arr)
   }

   def dsortProd[A,B,C:ClassTag](orda: DiscOrder[A], ordb: DiscOrder[B], xs: Array[((A,B),C)]): Array[Array[C]] = {
      val arr = ArrayBuffer[(A,(B,C))]()
      var i = 0; val sz = xs.size
      while ( i < sz ) {
         arr += ( xs(i) match { case ((a,b),c) => (a,(b,c)) } )
         i += 1
      }
      val ds1 = dsort(orda,arr.toArray)
      val ret = ArrayBuffer[Array[C]]()
      i = 0; val sz2 = ds1.size
      while ( i < sz2 ) {
         val ls: Array[Array[C]] = dsort(ordb,ds1(i))
         var j = 0; val sz3 = ls.size
         while( j < sz3 ) {
            ret += ls(j)
            j += 1
         }
         i += 1
      }
      ret.toArray
   }

   def dsortSum[A,B,C:ClassTag](orda: DiscOrder[A], ordb: DiscOrder[B], xs: Array[(Either[A,B],C)]): Array[Array[C]] = {
      val lefts = ArrayBuffer[(A,C)]()
      val rights = ArrayBuffer[(B,C)]()
      var i = 0; val sz = xs.size
      while ( i < sz ) {
         xs(i) match {
            case (Left(x),v) => lefts += ((x,v))
            case (Right(x),v) => rights += ((x,v))
         }
         i += 1
      }
      dsort(orda,lefts.toArray) ++ dsort(ordb,rights.toArray)
   }

   val v1 = Vector[(Either[Int,Int],Int)]((Left(4),4),(Left(3),3),(Right(5),5),(Right(6),6),(Right(1),1))
   lazy val resA = dosort(SumOrd(NatOrd(255),NatOrd(255)),v1).flatten

   val v2 = Vector[((Int,Int),Int)](((4,5),4),((3,6),3),((3,2),5),((1,10),6),((9,3),1))
   lazy val resB = dosort(ProdOrd(NatOrd(255),NatOrd(255)),v2).flatten

   val v3 = Vector[(Char,Int)](('y',5),('h',1),('c',3),('l',1),('t',9))
   lazy val resC = dosort(ordChar8,v3).flatten

   val v4 = Vector[(String,String)](("bb","bb"),("ab","ab"),("aa","aa"),("ba","ba"))
   lazy val resD = dosort(ordString8,v4.map { x => (x._1.toArray,x._2) }).flatten

   val data10 = List.fill(10)(nextInt)
   val data10t = data10.zip(data10)
   val data1000 = List.fill(1000)(nextInt)
   val data1000t = data1000.zip(data1000)
   val data10000 = List.fill(10000)(nextInt)
   val data10000t = data10000.zip(data10000)
   val data1000000 = List.fill(1000000)(nextInt)
   val data1000000t = data1000000.zip(data1000000)

   def dsortNat[A:ClassTag](z: NatOrd, xs: Array[(Int,A)]): Array[Array[A]] = {
      val asize = z.i+1; val input_size = xs.size
      val arr = new Array[ArrayBuffer[A]](asize)
      var i = 0
      while( i < input_size ) {
         xs(i) match { case (idx,v) =>
                           if(arr(idx) == null) arr(idx) = ArrayBuffer[A](v)
                           else arr(idx) += v }
         i += 1
      }
      var ret = new ArrayBuffer[Array[A]]()
      i = 0
      while( i < asize ){ if(arr(i) != null) ret += arr(i).toArray; i += 1 }
      ret.toArray
   }

   def time[A](f: => A) = {
      val s = System.nanoTime
      val ret = f
      println("time: "+(System.nanoTime-s)/1e6+"ms")
      ret
   }
}

object Main extends App {
   import DiscOrder._

   object CArgs extends CmdOpts(args) {
      lazy val cmpsort = opt("--cmp",TRUE) | false
      lazy val strings = opt("--string",TRUE) | false
      lazy val file = opt("--file",((x: String) => x)) | "words.txt"
      lazy val size = opt("--size",((x: String) => x.toInt)) | 1000
      override def version = opt("--version", () => "0.1")
   }

   if(CArgs.strings){
      val words: Vector[String] = scala.io.Source.fromFile(CArgs.file)
                    .getLines
                    .flatMap(_.split("\\W+")).toVector
      val wordst: Vector[(Array[Char],String)] = words.zip(words).map { x => (x._1.toArray,x._2) }
      if(CArgs.cmpsort) while(true) { time{ words.sorted } }
      else              while(true) { time{ dosort(ordString8,wordst) } }
   } else {
      val data = Vector.fill(CArgs.size)(nextInt)
      val datat = data.zip(data)
      if(CArgs.cmpsort) while(true) { time{ data.sorted } }
      else              while(true) { time{ dosort(ordInt32,datat) } }
   }
}
