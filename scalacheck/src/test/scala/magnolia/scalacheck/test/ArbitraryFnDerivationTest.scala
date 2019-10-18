package magnolia.scalacheck.test

import magnolia.test.Simple._
import magnolia.test.ADT._
import magnolia.scalacheck._
import org.scalacheck._
import org.scalatest._

class ArbitraryFnDerivationTest extends FlatSpec with Matchers {
//  val arbI = implicitly[Arbitrary[Integers => Integers]]
//  println(arbI.arbitrary.sample.get.apply(Integers(1, 2L)))

  var arbN = implicitly[Arbitrary[Node => Node]]
  arbN.arbitrary
//  println(arbN.arbitrary.sample.get.apply(Branch(Leaf(1), Leaf(2))))

//  implicitly[Arbitrary[A => A]]
}