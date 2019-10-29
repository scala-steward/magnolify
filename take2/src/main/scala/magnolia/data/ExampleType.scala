package magnolia.data

import java.util.{List => JList}

import com.google.protobuf.ByteString
import magnolia._
import magnolia.shims.FactoryCompat
import org.tensorflow.example._

import scala.collection.JavaConverters._
import scala.language.experimental.macros

trait ExampleType[T] extends ConverterType[T, ExampleOrBuilder, Example.Builder]

object ExampleType {
  def apply[T](implicit rt: ExampleRecord[T]): ExampleType[T] = new ExampleType[T] {
    override val recordType: RecordType[T, ExampleOrBuilder, Example.Builder] = rt
  }
}

trait ExampleRecord[V] extends RecordType[V, ExampleOrBuilder, Example.Builder] {
  override def newWriter: Example.Builder = Example.newBuilder()
}

object ExampleRecord extends LowPriorityExampleImplicits {
  type Typeclass[V] = ExampleRecord[V]

  def combine[V](caseClass: CaseClass[Typeclass, V]): Typeclass[V] = new Typeclass[V] {
    override def get(r: ExampleOrBuilder, k: String): V = caseClass.construct { p =>
      // FIXME: handle nested types
      p.typeclass.get(r, p.label)
    }

    override def put(w: Example.Builder, k: String, v: V): Example.Builder =
      caseClass.parameters.foldLeft(w) { (w, p) =>
        // FIXME: handle nested types
        p.typeclass.put(w, p.label, p.dereference(v))
      }
  }

  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] = ???

  implicit def apply[T]: Typeclass[T] = macro Magnolia.gen[T]
}

trait LowPriorityExampleImplicits {
  implicit def singletonExampleRecord[V](implicit f: ExampleField[V]): ExampleRecord[V] =
    new ExampleRecord[V] {
      override def get(r: ExampleOrBuilder, k: String): V =
        f.read(r.getFeatures.getFeatureMap.get(k))

      override def put(w: Example.Builder, k: String, v: V): Example.Builder =
        w.mergeFeatures(Features.newBuilder().putFeature(k, f.write(v).build()).build())
    }
}

trait ExampleField[V] extends FieldType[V, FeatureOrBuilder, Feature.Builder]

abstract class BaseExampleField[V] extends ExampleField[V] {
  def readList(v: FeatureOrBuilder): JList[V]
  def writeList(v: JList[V]): Feature.Builder
}

trait BaseExampleFieldImplicits {
  implicit val longExampleField = new BaseExampleField[Long] {
    override def readList(v: FeatureOrBuilder): JList[Long] =
      v.getInt64List.getValueList.asInstanceOf[JList[Long]]
    override def writeList(v: JList[Long]): Feature.Builder =
      Feature.newBuilder().setInt64List(
        Int64List.newBuilder().addAllValue(v.asInstanceOf[JList[java.lang.Long]]))

    override def read(v: FeatureOrBuilder): Long = readList(v).get(0)
    override def write(v: Long): Feature.Builder = writeList(java.util.Collections.singletonList(v))
  }

  implicit val bytesExampleField = new BaseExampleField[ByteString] {
    override def readList(v: FeatureOrBuilder): JList[ByteString] = v.getBytesList.getValueList
    override def writeList(v: JList[ByteString]): Feature.Builder =
      Feature.newBuilder().setBytesList(BytesList.newBuilder().addAllValue(v))

    override def read(v: FeatureOrBuilder): ByteString = readList(v).get(0)
    override def write(v: ByteString): Feature.Builder =
      writeList(java.util.Collections.singletonList(v))
  }
}

object ExampleField extends BaseExampleFieldImplicits {
  def atLong[V](f: Long => V)(g: V => Long): ExampleFieldType[Long, V] =
    new ExampleFieldType[Long, V](f, g)

  def atBytes[V](f: ByteString => V)(g: V => ByteString): ExampleFieldType[ByteString, V] =
    new ExampleFieldType[ByteString, V](f, g)

  implicit def singletonExampleField[B, V](implicit base: BaseExampleField[B],
                                           t: ExampleFieldType[B, V])
  : ExampleField[V] = new ExampleField[V] {
    override def read(v: FeatureOrBuilder): V = t.from(base.read(v))
    override def write(v: V): Feature.Builder = base.write(t.to(v))
  }

  implicit def optionExampleField[B, V](implicit base: BaseExampleField[B],
                                        t: ExampleFieldType[B, V])
  : ExampleField[Option[V]] = new ExampleField[Option[V]] {
    override def read(v: FeatureOrBuilder): Option[V] =
      base.readList(v).asScala.headOption.map(t.from)
    override def write(v: Option[V]): Feature.Builder =
      base.writeList(v.toList.map(t.to).asJava)
  }

//  def atLong[V](f: Long => V)(g: V => Long): ExampleField[JList[V]] =
//    new ExampleField[JList[V]] {
//      override def read(v: FeatureOrBuilder): JList[V] =
//        v.getInt64List.getValueList.asScala.foldLeft(new java.util.ArrayList[V]) { (xs, x) =>
//          xs.add(f(x))
//          xs
//        }
//      override def write(v: JList[V]): Feature.Builder =
//        Feature.newBuilder().setInt64List(v.asScala.foldLeft(Int64List.newBuilder()) { (b, x) =>
//          b.addValue(g(x))
//        })
//    }
//
//  def atBytes[V](f: ByteString => V)(g: V => ByteString): ExampleField[Iterable[V]] =
//    new ExampleField[Iterable[V]] {
//      override def read(v: FeatureOrBuilder): Iterable[V] =
//        v.getBytesList.getValueList.asScala.map(f)
//      override def write(v: Iterable[V]): Feature.Builder =
//        Feature.newBuilder().setBytesList(BytesList.newBuilder().addAllValue(v.map(g).asJava))
//    }
//
//  implicit val atLongExampleField = atLong[Long](identity)(identity)
//  implicit val atStringExampleField = atBytes[String](_.toStringUtf8)(ByteString.copyFromUtf8)
//
//  implicit def singletonExampleField[V](implicit f: ExampleField[JList[V]])
//  : ExampleField[V] = new ExampleField[V] {
//    override def read(v: FeatureOrBuilder): V = {
//      val xs = f.read(v)
//      require(xs.size() == 1)
//      xs.get(0)
//    }
//    override def write(v: V): Feature.Builder = f.write(java.util.Collections.singletonList(v))
//  }
//
//  implicit def optionExampleField[V](implicit f: ExampleField[JList[V]])
//  : ExampleField[Option[V]] = new ExampleField[Option[V]] {
//    override def read(v: FeatureOrBuilder): Option[V] = {
//      val xs = f.read(v)
//      require(xs.size() <= 1)
//      if (xs.isEmpty) None else Some(xs.get(0))
//    }
//    override def write(v: Option[V]): Feature.Builder =
//      f.write(v.foldLeft(new java.util.ArrayList[V]) { (xs, x) =>
//        xs.add(x)
//        xs
//      })
//  }

}

class ExampleFieldType[A, B](f: A => B, g: B => A) {
  def from(a: A): B = f(a)
  def to(b: B): A = g(b)
}
