package discsort
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.MutableList
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
case class ListOrd[A](t: DiscOrder[A]) extends DiscOrder[Stream[A]]

object DiscOrder {
   val ordUnit = TrivOrd[Unit]()
   val ordNat8 = NatOrd(255)
   val ordNat16 = NatOrd(65535)
   val ordInt32 = MapOrd(splitW compose {x:Int => x+(-2147483648)},
                       ProdOrd(ordNat16, ordNat16))
   def splitW: Int => (Int,Int) = x => ((x>>16) & 65535,x & 65535)
   val ordChar8 = MapOrd({x: Char => x.toInt},ordNat8)
   val ordString8 = ListOrd(ordChar8)

   def dsort[A,B](ord: DiscOrder[A], xs: Stream[(A,B)]): Stream[Stream[B]] =
      xs match {
         case Stream() => Stream()
         case Stream((_,v)) => Stream(Stream(v))
         case _ => ord match {
            case TrivOrd() => Stream(xs map { _._2 })
            case n: NatOrd => dsortNat(n,xs)
            case SumOrd(orda,ordb) => dsortSum(orda,ordb,xs)
            case ProdOrd(orda,ordb) => dsortProd(orda,ordb,xs)
            case MapOrd(f,ordb) => dsortMap(f,ordb,xs)
            case ListOrd(orda) => dsortList(orda,xs)
         }
      }

   def stripPartition[A](l: Stream[A]): (Either[Unit,(A,Stream[A])])  =
      l.headOption.cata( { x: A => Right((x,l.tail)) }, { Left(()) } )

   def dsortList[A,B](orda: DiscOrder[A], xs: Stream[(Stream[A],B)]): Stream[Stream[B]] =
      dsort(MapOrd(stripPartition[A], SumOrd(ordUnit,ProdOrd(orda,ListOrd(orda)))), xs)

   def dsortMap[A,B,C](f: A => B, ordb: DiscOrder[B], xs: Stream[(A,C)]): Stream[Stream[C]] =
      dsort(ordb,xs map { case (a,c) => (f(a),c) })

   def dsortProd[A,B,C](orda: DiscOrder[A], ordb: DiscOrder[B], xs: Stream[((A,B),C)]): Stream[Stream[C]] =
      dsort(orda,xs map { case ((a,b),c) => (a,(b,c)) }) flatMap { dsort(ordb,_) }

   def dsortSum[A,B,C](orda: DiscOrder[A], ordb: DiscOrder[B], xs: Stream[(Either[A,B],C)]): Stream[Stream[C]] = {
      val (lefts,rights) = xs.foldLeft((Stream[(A,C)](),Stream[(B,C)]())) {
         case ((ls,rs),(Left(x),v)) => (ls :+ (x,v),rs)
         case ((ls,rs),(Right(x),v)) => (ls,rs :+ (x,v))
      }
      dsort(orda,lefts) ++ dsort(ordb,rights)
   }

   val v1 = Stream[(Either[Int,Int],Int)]((Left(4),4),(Left(3),3),(Right(5),5),(Right(6),6),(Right(1),1))
   lazy val resA: Stream[Int] = dsort(SumOrd(NatOrd(255),NatOrd(255)),v1).flatten

   val v2 = Stream[((Int,Int),Int)](((4,5),4),((3,6),3),((3,2),5),((1,10),6),((9,3),1))
   lazy val resB: Stream[Int] = dsort(ProdOrd(NatOrd(255),NatOrd(255)),v2).flatten

   val v3 = Stream[(Char,Int)](('y',5),('h',1),('c',3),('l',1),('t',9))
   lazy val resC: Stream[Int] = dsort(ordChar8,v3).flatten

   val v4 = Stream[(String,String)](("bb","bb"),("ab","ab"),("aa","aa"),("ba","ba"))
   lazy val resD: Stream[String] = dsort(ordString8,v4.map { x => (x._1.toStream,x._2) }).flatten

   val data10 = List.fill(10)(nextInt)
   val data10t = data10.zip(data10)
   val data1000 = List.fill(1000)(nextInt)
   val data1000t = data1000.zip(data1000)
   val data10000 = List.fill(10000)(nextInt)
   val data10000t = data10000.zip(data10000)
   val data1000000 = List.fill(1000000)(nextInt)
   val data1000000t = data1000000.zip(data1000000)

   def dsortNat[A](z: NatOrd, xs: Stream[(Int,A)]): Stream[Stream[A]] = {
     val arr = new Array[MutableList[A]](z.i+1)
     xs.foreach { arg: (Int,A) =>
        val idx = arg._1
        if(arr(idx) == null) arr(idx) = MutableList[A](arg._2)
        else arr(idx) += arg._2
     }
     arr.toStream.filter(_ != null).map(_.toStream)
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
      val src: Stream[String] = scala.io.Source.fromFile(CArgs.file)
                    .getLines
                    .flatMap(_.split("\\W+")).toStream
      val words = src.zip(src)
      val wordst: Stream[(Stream[Char],String)] = words.map { x => (x._1.toStream,x._2) }
      if(CArgs.cmpsort) while(true) { time{ words.sorted } }
      else              while(true) { time{ dsort(ordString8,wordst) } }
   } else {
      val data = Stream.fill(CArgs.size)(nextInt)
      val datat = data.zip(data)
      if(CArgs.cmpsort) while(true) { time{ datat.sorted } }
      else              while(true) { time{ dsort(ordInt32,datat) } }
   }
}
