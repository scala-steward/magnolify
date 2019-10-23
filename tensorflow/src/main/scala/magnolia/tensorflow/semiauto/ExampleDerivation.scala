package magnolia.tensorflow.semiauto

import magnolia._
import org.tensorflow.example._

import scala.collection.JavaConverters._
import scala.language.experimental.macros

trait ExampleMappable[V] {
  type M = FeaturesOrBuilder

  def from(v: Example): V = ???
  def to(v: V): Example = ???

  def fromF(v: Feature): V
  def toF(v: V): Feature

  def empty: M = Features.newBuilder()
  def get(m: M, k: String): V = fromF(m.getFeatureOrThrow(k))
  def put(m: M, k: String, v: V): Unit =
    m.asInstanceOf[Features.Builder].putFeature(k, toF(v))
}

object ExampleMappable {
  implicit val longM = new ExampleMappable[Long] {
    override def fromF(v: Feature): Long = v.getInt64List.getValue(0)
    override def toF(v: Long): Feature = Feature.newBuilder()
      .setInt64List(Int64List.newBuilder().addValue(v))
      .build()
  }

  implicit val longM2 = new ExampleMappable[List[Long]] {
    override def fromF(v: Feature): List[Long] =
      v.getInt64List.getValueList.asScala.toList.asInstanceOf[List[Long]]
    override def toF(v: List[Long]): Feature = Feature.newBuilder()
      .setInt64List(Int64List.newBuilder()
        .addAllValue(v.asInstanceOf[List[java.lang.Long]].asJava))
      .build()
  }
}

object ExampleDerivation {
  type Typeclass[T] = ExampleMappable[T]

  def combine[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] = new Typeclass[T] {
    override def from(v: Example): T = {
      val b = v.getFeatures.toBuilder
      v.getFeaturesOrBuilder
      caseClass.construct { p =>
        p.typeclass.get(b, p.label)
      }
    }

    override def to(v: T): Example =
      Example.newBuilder().setFeatures(
        caseClass.parameters.foldLeft(this.empty) { (m, p) =>
          p.typeclass.put(m, p.label, p.dereference(v))
          m
        }.asInstanceOf[Features.Builder]
      ).build()

    override def fromF(v: Feature): T = ???
    override def toF(v: T): Feature = ???
  }

  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] = ???

  implicit def apply[T]: Typeclass[T] = macro Magnolia.gen[T]
}

object Test {
  case class A(i: Long)
  case class B(i: List[Long])
  def main(args: Array[String]): Unit = {
    val m = ExampleDerivation[A]
    val a = m.to(A(1))
    println(a)
    val b = m.from(a)
    println(b)

    val m1 = ExampleDerivation[B]
    println(m1.to(B(List(1, 2, 3))))
  }
}
