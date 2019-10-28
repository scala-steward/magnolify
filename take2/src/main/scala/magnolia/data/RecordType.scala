package magnolia.data

import magnolia._

trait ConverterType[T, RecordReader, RecordWriter] {
  def from(v: RecordReader): T
  def to(v: T): RecordWriter
}

trait RecordType[V, RecordReader, RecordWriter, FieldReader, FieldWriter] {
  type FT <: FieldType[V, FieldReader, FieldWriter]
  def newBuilder: RecordWriter
  def get(r: RecordReader, k: String)(implicit f: FT): V
  def put(w: RecordWriter, k: String, v: V)(implicit f: FT): RecordWriter
}

trait FieldType[V, FieldReader, FieldWriter] {
  def read(v: FieldReader): V
  def write(v: V): FieldWriter
}

//trait RecordTypeCompanion[V] {
//  type Typeclass[V] <:
//}