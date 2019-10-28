package magnolia.data

import magnolia._

trait ConverterType[T, RecordReader, RecordWriter] {
  val recordType: RecordType[T, RecordReader, RecordWriter]
  def from(v: RecordReader): T = recordType.get(v, null)
  def to(v: T): RecordWriter = recordType.put(recordType.newBuilder, null, v)
}

trait RecordType[V, RecordReader, RecordWriter] {
  def newBuilder: RecordWriter
  def get(r: RecordReader, k: String): V
  def put(w: RecordWriter, k: String, v: V): RecordWriter
}

trait FieldType[V, FieldReader, FieldWriter] {
  def read(v: FieldReader): V
  def write(v: V): FieldWriter
}

//trait RecordTypeCompanion[V] {
//  type Typeclass[V] <:
//}

object Test {
  case class A1(i: Int, s: String)
  case class A2(io: Option[Int], so: Option[String])
  case class A3(is: List[Int], ss: List[String])
  case class B(a1: A1, a2: A2, a3: A3)

  def main(args: Array[String]): Unit = {
    implicitly[TableRowField[Int]]
    implicitly[TableRowField[String]]
    implicitly[TableRowField[Option[Int]]]
    implicitly[TableRowField[List[Int]]]

    val a1 = TableRowType[A1]
    println(a1.to(A1(1, "a")))
    println(a1.from(a1.to(A1(1, "a"))))

    val a2 = TableRowType[A2]
    println(a2.to(A2(Some(1), Some("a"))))
    println(a2.from(a2.to(A2(Some(1), Some("a")))))

    val a3 = TableRowType[A3]
    println(a3.to(A3(List(1), List("a"))))
    println(a3.from(a3.to(A3(List(1), List("a")))))

    val b = TableRowType[B]
    println(b.to(B(A1(1, "a"), A2(Some(2), Some("b")), A3(List(3), List("c")))))
//    println(b.from(b.to(B(A1(1, "a"), A2(Some(2), Some("b")), A3(List(3), List("c"))))))
  }
}
