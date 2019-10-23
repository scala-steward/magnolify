package magnolia.scalacheck.semiauto

import magnolia._
import magnolia.shims.Monadic
import org.scalacheck.{Arbitrary, Gen}

import scala.language.experimental.macros

object ArbitraryDerivation {
  type Typeclass[T] = Arbitrary[T]

  sealed trait Recursive[T] {
    def fallback: Gen[T]
  }

  object Recursive {
    def apply[T](t: T): Recursive[T] = Recursive(Gen.const(t))
    def apply[T](g: Gen[T]): Recursive[T] = new Recursive[T] {
      override def fallback: Gen[T] = g
    }
    implicit def defaultFallback[T]: Recursive[T] = Recursive[T](Gen.fail)
  }

  def combine[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] = Arbitrary {
    Gen.lzy(Gen.sized { size =>
      if (size >= 0) {
        caseClass.constructMonadic(_.typeclass.arbitrary)(monadicGen)
      } else {
        implicitly[Recursive[T]].fallback
      }
    })
//    Gen.lzy(Gen.sized { size =>
//      if (size >= 0) {
//        val g = caseClass.constructMonadic(_.typeclass.arbitrary)(monadicGen)
//        Gen.resize(size - 1, g)
//      } else {
//        implicitly[Recursive[T]].fallback
//      }
//    })
  }

  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] = Arbitrary {
    Gen.sized { size =>
      if (size >= 0) {
        val g: Gen[T] = Gen.oneOf(sealedTrait.subtypes.map(_.typeclass.arbitrary)).flatMap(identity)
        Gen.resize(size - 1, g)
      } else {
        implicitly[Recursive[T]].fallback
      }
    }
  }

  private val monadicGen: Monadic[Gen] = new Monadic[Gen] {
    override def point[A](value: A): Gen[A] = Gen.const(value)
    override def flatMapS[A, B](from: Gen[A])(fn: A => Gen[B]): Gen[B] = from.flatMap(fn)
    override def mapS[A, B](from: Gen[A])(fn: A => B): Gen[B] = from.map(fn)
  }

  implicit def apply[T]: Typeclass[T] = macro Magnolia.gen[T]
}
