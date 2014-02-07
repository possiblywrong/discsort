package discsort
import scalaz._, Scalaz._
import shapeless._

// Bungled attempt at implementing generic discrimiated sort,
// based on the following paper;
// www.diku.dk/hjemmesider/ansatte/henglein/papers/henglein2011a.pdf

trait SOrder[A]
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

//   object sdisc extends Poly1 {
//
//      implicit def caseNat0 = at[Nat0]( { case Nat0(i) => sdiscNat(i) } )
//      implicit def caseSumL[A,B,C] =
//         at[SumL[A,B]]( { case SumL(t1,t2) => { xs: List[(A\/B,C)] =>
//            sdisc(t1)(lefts) ++ sdisc(t2)(rights)
//         } } )
//
//   }

   def sdiscNat[B](i: Int): Disc[Int,B] = { xs: List[(Int,B)] =>
      accumArray[B]( { (xs: List[B], x: B) => x :: xs }, List(), (0,i-1),  xs)
   }

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
         println(s"Setting idx[$idx] to $nl")
         arr(idx) = nl
      } )
      arr.toList.filter(_!=null).flatten
   }
}

//object TestCase {
//   // Given input of type X return a function of
//   // type List[(A,B)] => List[B] forall B
//   // Types X & A are related
//   object filt extends Poly1 {
//      implicit def caseOne = at[Int]( i => {
//         object partialbind extends somefn(i);
//         partialbind
//      } )
//      // ...may have other cases
//   }
//
//   class somefn(j: Int) extends Poly1 {
//      implicit def caseOne[A] = at[List[(Int,A)]]( xs => xs.filter(_._1 == j).map(_._2) )
//   }
//
//   val t1 = filt(4)
////   val t2 = filt(5)(List((1,1),(5,5)))
//}
//
//object TestCase2 {
//   // Given input of type X return a function of
//   // type List[(A,B)] => List[B] forall B
//   // Types X & A are related
//   object filt extends Poly1 {
//      implicit def caseOne[A] = at[Int]( i => {
//         xs: List[(Int,A)] => xs.filter(_._1 == i).map(_._2)
//      } )
//      // ...may have other cases
//   }
//
////   val t1 = filt[String](4)
////   val t2 = filt[Int](5)(List((1,1),(5,5)))
//}
//
//object TestCase3 {
//   // Given input of type X return a function of
//   // type List[(A,B)] => List[B] forall B
//   // Types X & A are related
//   object filt extends Poly1 {
//      implicit def caseOne = at[Int]( i => {
//         object somefn { def apply[A]: List[(Int,A)] => List[A] = ( xs => xs.filter(_._1 == i).map(_._2) ) }
//      } )
//      // ...may have other cases
//   }
//
//   val t1 = filt(4)
////   val t2 = filt(5)(List((1,1),(5,5)))
//}

object TestCase {
   trait MYtype[A]
   case class Bob(i: Int) extends MYType[Int]
   case class Wibble[A,B](t1: MYType[A], t2: MYType[B]) extends MYType[A\/B]

   // MYType => ( List[(A,B)] => List[B] )
   // So we define myfun for each member of the MYType ADT
   object myfun extends Poly1 {
      implicit def caseOne = at[Bob]( i => { ... somefn } )
      implicit def caseTwo[A,B] = at[Wibble[A,B]]( i => { { xs => myfun(as) ++ myfun(bs) } } )
   }

   object somefn extends Poly1 {
      implicit def caseOne[A] = at[List[(Int,A)]]( ... )
   }

   val t1 = myfun(Bob(42))
   val t2 = myfun(Wibble(Bob(5),Bob(101)))
}
