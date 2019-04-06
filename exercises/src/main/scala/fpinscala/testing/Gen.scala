package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.datastructures.{List => MList, _}
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Nonblocking.Par
import fpinscala.errorhandling.{Option => MOption, Some => MSome}
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

// import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter
/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

// trait Prop {self =>
//   def check: Either[(FailedCase, SuccessCount), SuccessCount]
//   // def &&(p: Prop): Prop = new Prop {
//   //   def check:Boolean = self.check.right && p.check
//   // }
// }

case class Gen[+A](sample: State[RNG,A]) {
  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)
  def listOfN(size: Gen[Int]): Gen[List[A]] = size flatMap (n => this.listOfN(n))
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))
  def map[B](f: A => B): Gen[B] = Gen(sample.map(a => f(a)))
  def map2[B, C](b: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(b.sample)(f))
  def unsized: SGen[A] = SGen(_ => this)
  def **[B](g: Gen[B]): Gen[(A, B)] = 
    (this map2 g)((_, _))
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
  type MaxSize = Int

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, testCases, rng) => randomStream(gen)(rng).zip(Stream.from(0)).take(testCases).map{
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMessage(a, e), i) }
    }.find(_.isFalsified) getOrElse Passed
  }

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, testCases, rng) => 
      val casesPerSize = (testCases + (max + 1)) / max
      val props: Stream[Prop] = Stream.from(0).take((testCases min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop = props.map(p => Prop {(max, _, rng) =>
                          p.run(max, casesPerSize, rng)
                       }).toList.reduce(_ && _)
     prop.run(max, testCases, rng)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(s => Some(g.sample.run(rng)))
  def buildMessage[A](s: A, e: Exception): String = 
    s"test case $s \n" +
    s"generated an excepetion ${e.getMessage}\n" +
    s"stack trace: \n ${e.getStackTrace.mkString("\n")}"

  def check(p: => Boolean): Prop = Prop((_, _, _) => 
    if (p) Proved else Falsified ("()", 0)
  )

  val S = weighted(choose(1,4).map(Executors.newFixedThreadPool) -> 0.75,
                  unit(Executors.newCachedThreadPool) -> 0.25)

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    // forAll(S.map2(g)((_, _))){case (s, a) => f(a)(s).get}
    forAll( S ** g ){case s ** a => Par.run(s)(f(a))}// second ** by unapply object

  def run(p: Prop, maxSize: MaxSize = 100, testCases: TestCases = 100, rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Falsified(msg, i) =>
        println(s"! Falsified after $i passed tests:\n $msg")
      case Proved =>
        println(s"+ OK, Proved property")
    }

  }
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Passed|Proved => p.run(max, n, rng)
      case x => x 
    }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Falsified(failMessage, successes) => p.tag(failMessage).run(max, n, rng)
      case x => x
    }
  }

  def tag(msg: String): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Falsified(failMessage, successes) => Falsified(msg + "\n" + failMessage, successes)
      case x => x
    }
  }
}

trait Cogen[-A] {
  def sample(a: A, rng: RNG): RNG
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a)) 
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))
  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))
  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean flatMap (b => if (b) g1 else g2)
  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(State(RNG.double).flatMap{d => 
      if (d < threshold) g1._1.sample
      else g2._1.sample
    })
  }

  object ** {
    def unapply[A, B](a: (A, B)) = Some(a)
  }


  def listOf[A](g: Gen[A]): SGen[List[A]] = {
    // val g2: Int => Gen[List[A]] = n => Gen.listOfN(n, g)
    // SGen(g2)
    SGen(n => g.listOfN(n))
  }

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n max 1))

  def genStringFn[A](g: Gen[A]): Gen[String => A] = Gen {
    State { (rng: RNG) =>
      val (seed, rng2) = rng.nextInt
      val f = (s: String) => g.sample.run(RNG.Simple(s.length * seed))._1 // run :: s -> (a, s)
      (f, rng2)
    }
  }

  def fn[A,B](in: Cogen[A])(out: Gen[B]): Gen[A => B] = Gen {
    State { (rng: RNG) =>
      val (seed, rng2) = rng.nextInt
      val f = (a:A) => out.sample.run(in.sample(a, rng2))._1
      (f, rng2)
    }
  }
}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}

case object Proved extends Result {
  def isFalsified = false
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)
  def map[B](f: A => B): SGen[B] = SGen{ forSize(_) map (a => f(a)) }
  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val g2: Int => Gen[B] = n => {forSize(n) flatMap (a => f(a).forSize(n))}
    SGen(g2)
  }
  def **[B](s2: SGen[B]): SGen[(A,B)] =
    SGen(n => apply(n) ** s2(n))
}


object Test {
  val smallInt: Gen[Int] = Gen.choose(-10,10)
  val complexInt: Gen[List[Int]] = Gen.choose(0, 10).listOfN(Gen.choose(0, 10))
  def treeInt: Gen[(List[Int], Tree[Int])] = Gen.choose(-100, 100).listOfN(Gen.choose(0, 20)) map {l => 
    (l, l.foldLeft(Tree.unit(0))((acc, a) => Branch(acc, Leaf(a))))
  }
  def optionGenList: Gen[(List[Int], List[MOption[Int]])] = Gen.choose(-100, 100).listOfN(Gen.choose(0, 20)) map {l =>
    (l, l.foldLeft(List.empty[MOption[Int]])((acc, a) => MSome(a) :: acc ))
  }

  def optionSequenceProp = forAll(optionGenList){ case (a, b) =>
    val t1 = MOption.sequence2(b)
    println("sequence2 " + t1)
    println("oring lis " + a.reverse)
    (t1 getOrElse None) == a.reverse
  }

  def testOptionSequence = Prop.run(optionSequenceProp)
  val maxProp = forAll(listOf1(smallInt)){ ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  def a = Prop.run(maxProp)


  val sortedListProp = forAll(listOf(smallInt)){ ns =>
    val nss = ns.sorted
    nss.isEmpty || {!nss.zip(nss.tail).exists {
      case (a, b) => a > b
    }}
  }

  def b = Prop.run(sortedListProp)

  def parInt = Gen.choose(0, 10) map (Par.unit(_))
  def parProp = forAllPar(parInt){ n => 
    Par.equal(Par.map(n)(y => y), n)
  }
  def c = Prop.run(parProp)

  def listProp1 = forAll(listOf1(smallInt)){ns =>
    val initL = ns.length
    ns.drop(2).length - initL == 2
  }

  def l = Prop.run(listProp1)

  val treeProp = forAll((treeInt)){case(a, b) =>
    val treeMax = Tree.fold(b)(identity)(_ max _)
    treeMax == (a.max)
  }

  def testTreeFold = Prop.run(treeProp)

}
