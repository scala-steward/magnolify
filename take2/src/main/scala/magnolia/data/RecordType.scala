package magnolia.data

import com.google.protobuf.ByteString
import magnolia._

trait ConverterType[T, RecordReader, RecordWriter] {
  val recordType: RecordType[T, RecordReader, RecordWriter]
  def from(v: RecordReader): T = recordType.get(v, null)
  def to(v: T): RecordWriter = recordType.put(recordType.newWriter, null, v)
}

trait RecordType[V, RecordReader, RecordWriter] {
  def newWriter: RecordWriter
  val nested: Boolean = false
  def get(r: RecordReader, k: String): V
  def put(w: RecordWriter, k: String, v: V): RecordWriter
}

trait FieldType[V, FieldReader, FieldWriter] {
  def read(v: FieldReader): V
  def write(v: V): FieldWriter
}

object Test {
  case class A1(i: Long, s: String)
  case class A2(io: Option[Long], so: Option[String])
  case class A3(is: List[Long], ss: List[String])
  case class B(a1: A1, a2: A2, a3: A3)

  case class C(ao: Option[A1], as: List[A1])

  case class CX(ao: Option[A1])

  def main(args: Array[String]): Unit = {
    implicit val atString = ExampleField.atBytes(_.toStringUtf8)(ByteString.copyFromUtf8)

    implicitly[ExampleField[Long]]
    implicitly[BaseExampleField[ByteString]]
    implicitly[ExampleFieldType[ByteString, String]]
    implicitly[ExampleField[String]]
    //    val T = TableRowType
    val T = ExampleType

    val a1 = T[A1]
    println(a1.to(A1(1, "a")))
    println(a1.from(a1.to(A1(1, "a"))))

    implicitly[ExampleField[Option[Long]]]
    val a2 = T[A2]
    println(a2.to(A2(Some(1), Some("a"))))
    println(a2.from(a2.to(A2(Some(1), Some("a")))))
//
//    val a3 = T[A3]
//    println(a3.to(A3(List(1), List("a"))))
//    println(a3.from(a3.to(A3(List(1), List("a")))))
//
//    val b = T[B]
//    println(b.to(B(A1(1, "a"), A2(Some(2), Some("b")), A3(List(3), List("c")))))
//    println(b.from(b.to(B(A1(1, "a"), A2(Some(2), Some("b")), A3(List(3), List("c"))))))
//
//    val c = T[C]
//    println(c.to(C(Some(A1(1, "a")), List(A1(2, "b")))))
//    println(c.from(c.to(C(Some(A1(1, "a")), List(A1(2, "b"))))))
  }
}
