import magnolia.scalacheck._
import magnolia.test.ADT.Node
import org.scalacheck._

object Debug {
  def main(args: Array[String]): Unit = {
    val seed = rng.Seed.fromBase64("aOEGx2W0xmJUeu5gDOM01EwECnDRkWUPIb0lub28MgO=").get
    val gen = implicitly[Arbitrary[Node]].arbitrary
    Gen.listOfN(70, gen).apply(Gen.Parameters.default, seed)
  }
}
