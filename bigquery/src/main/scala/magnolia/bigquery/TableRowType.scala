package magnolia.bigquery

import com.google.api.services.bigquery.model.TableFieldSchema
import com.google.api.services.bigquery.model.TableRow
import com.google.api.services.bigquery.model.TableSchema
import com.google.common.io.BaseEncoding
import magnolia._
import magnolia.data.Converter
import magnolia.shims._

import scala.collection.JavaConverters._
import scala.language.experimental.macros

sealed trait TableRowType[T] extends Converter.Record[T, TableRow] {
  protected type R = TableRow
  def apply(r: R): T = from(r)
  def apply(t: T): R = to(t)
  def schema: TableSchema = ???
  override protected def empty: R = new TableRow
  override protected def from(r: R): T = ???
  override protected def to(t: T): R = ???
}

object TableRowType {
  type Typeclass[T] = TableRowField[T]

  def combine[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] = new Typeclass[T] {
    override def schema: TableSchema = new TableSchema().setFields(
      caseClass.parameters.map(p => p.typeclass.fieldSchema.setName(p.label)).asJava)

    override protected def from(r: R): T =
      caseClass.construct(p => p.typeclass.get(r, p.label))

    override protected def to(t: T): R =
      caseClass.parameters.foldLeft(empty) { (r, p) =>
        p.typeclass.put(r, p.label, p.dereference(t))
        r
      }

    override def fieldSchema: TableFieldSchema =
      new TableFieldSchema().setType("STRUCT").setMode("REQUIRED")
    override def fromField(v: Any): T = {
      val r = empty
      r.putAll(v.asInstanceOf[java.util.Map[String, Any]])
      this.from(r)
    }
    override def toField(v: T): Any = to(v)
  }

  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] = ???

  implicit def apply[T]: TableRowType[T] = macro Magnolia.gen[T]

}

sealed trait TableRowField[V]
  extends TableRowType[V]
  with Converter.Field[V, TableRow] { self =>
  override def get(r: R, k: String): V = fromField(r.get(k))
  override def put(r: R, k: String, v: V): Unit = r.put(k, toField(v))

  def fieldSchema: TableFieldSchema
  def fromField(v: Any): V
  def toField(v: V): Any

  def imap[U](f: V => U)(g: U => V): TableRowField[U] = new TableRowField[U] {
    override def fieldSchema: TableFieldSchema = self.fieldSchema
    override def fromField(v: Any): U = f(self.fromField(v))
    override def toField(v: U): Any = self.toField(g(v))
  }
}

object TableRowField {
  def apply[V](implicit f: TableRowField[V]): TableRowField[V] = f

  def at[V](tpe: String)(f: Any => V)(g: V => Any): TableRowField[V] = new TableRowField[V] {
    override def fieldSchema: TableFieldSchema = new TableFieldSchema()
      .setType(tpe)
      .setMode("REQUIRED")
    override def fromField(v: Any): V = f(v)
    override def toField(v: V): Any = g(v)
  }

  implicit val trfBool = at[Boolean]("BOOL")(_.toString.toBoolean)(identity)
  implicit val trfInt = at[Int]("INT64")(_.toString.toInt)(identity)
  implicit val trfLong = at[Long]("INT64")(_.toString.toLong)(identity)
  implicit val trfFloat = at[Float]("FLOAT64")(_.toString.toFloat)(identity)
  implicit val trfDouble = at[Double]("FLOAT64")(_.toString.toDouble)(identity)
  implicit val trfString = at[String]("STRING")(_.toString)(identity)

  import NumericConverter._
  implicit val trfNumeric = at[BigDecimal]("NUMERIC")(toBigDecimal)(fromBigDecimal)

  implicit val trfByteArray = at[Array[Byte]]("BYTES")(
    x => BaseEncoding.base64().decode(x.toString))(
    x => BaseEncoding.base64().encode(x))

  import TimestampConverter._
  implicit val trfInstant = at("TIMESTAMP")(toInstant)(fromInstant)
  implicit val trfDate = at("DATE")(toLocalDate)(fromLocalDate)
  implicit val trfTime = at("TIME")(toLocalTime)(fromLocalTime)
  implicit val trfDateTime = at("DATETIME")(toLocalDateTime)(fromLocalDateTime)

  implicit def trfOption[V](implicit f: TableRowField[V]): TableRowField[Option[V]] =
    new TableRowField[Option[V]] {
      override def fieldSchema: TableFieldSchema = f.fieldSchema.setMode("NULLABLE")
      override def fromField(v: Any): Option[V] = ???
      override def toField(v: Option[V]): Any = ???
      override def get(r: R, k: String): Option[V] =
        Option(r.get(k)).map(f.fromField)
      override def put(r: R, k: String, v: Option[V]): Unit =
        v.foreach(x => r.put(k, f.toField(x)))
    }

  implicit def trfSeq[V, S[V]](implicit f: TableRowField[V],
                               toSeq: S[V] => Seq[V],
                               fc: FactoryCompat[V, S[V]]): TableRowField[S[V]] =
    new TableRowField[S[V]] {
      override def fieldSchema: TableFieldSchema = f.fieldSchema.setMode("REPEATED")
      override def fromField(v: Any): S[V] = ???
      override def toField(v: S[V]): Any = ???
      override def get(r: R, k: String): S[V] = r.get(k) match {
        case null => fc.newBuilder.result()
        case xs: java.util.List[_] => fc.build(xs.asScala.iterator.map(f.fromField))
      }
      override def put(r: R, k: String, v: S[V]): Unit =
        r.put(k, toSeq(v).map(f.toField).asJava)
    }
}
