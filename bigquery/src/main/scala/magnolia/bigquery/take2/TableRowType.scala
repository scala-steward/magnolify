package magnolia.bigquery.take2

import com.google.api.services.bigquery.model.TableRow
import com.google.common.io.BaseEncoding
import magnolia._
import magnolia.bigquery.TimestampConverter
import magnolia.bigquery.TimestampConverter._

import scala.collection.JavaConverters._
import scala.language.experimental.macros

trait Converter[T, R] extends Serializable

object Converter {
  trait Record[T, R] extends Converter[T, R] {
    def empty: R
    def from(r: R): T
    def to(t: T): R
  }

  trait Field[V, R] extends Converter[V, R] {
    def get(r: R, k: String): V
    def put(r: R, k: String, v: V): R
  }
}

trait TableRowType[T] extends Converter[T, TableRow] {
  type R = TableRow
  def apply(r: R): T = this match {
    case tc: TableRowRecord[T] => tc.from(r)
  }
  def apply(t: T): R = this match {
    case tc: TableRowRecord[T] => tc.to(t)
  }
}

trait TableRowRecord[T] extends TableRowType[T] with Converter.Record[T, TableRow] {
  override def empty: TableRow = new TableRow
}

class TableRowField[V](val getFn: Any => V, val putFn: V => Any)
  extends TableRowType[V] with Converter.Field[V, TableRow] {
  override def get(r: R, k: String): V = getFn(r.get(k))
  override def put(r: R, k: String, v: V): R = {
    r.put(k, putFn(v))
    r
  }

  def imap[U](f: V => U)(g: U => V): TableRowField[U] =
    new TableRowField[U](v => f(getFn(v)), v => putFn(g(v)))
}

object TableRowType extends Implicits {
  type Typeclass[T] = TableRowType[T]

  def combine[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] = new TableRowRecord[T] {
    override def from(r: R): T = caseClass.construct { p =>
      p.typeclass match {
        case tc: TableRowRecord[p.PType] =>
          val copy = tc.empty
          copy.putAll(r.get(p.label).asInstanceOf[java.util.Map[String, Any]])
          copy
        case tc: TableRowField[p.PType] =>
          tc.get(r, p.label)
      }
    }

    override def to(t: T): R = caseClass.parameters.foldLeft(empty) { (r, p) =>
      p.typeclass match {
        case tc: TableRowRecord[p.PType] =>
          r.put(p.label, tc.to(p.dereference(t)))
          r
        case tc: TableRowField[p.PType] => tc.put(r, p.label, p.dereference(t))
      }
    }
  }

  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] = ???

  implicit def apply[T]: TableRowType[T] = macro Magnolia.gen[T]
}

trait Implicits {
  def at[V](f: Any => V)(g: V => Any): TableRowType[V] = new TableRowField[V](f, g)

  implicit val trfBool = at[Boolean](_.toString.toBoolean)(identity)
  implicit val trfInt = at[Int](_.toString.toInt)(identity)
  implicit val trfLong = at[Long](_.toString.toLong)(identity)
  implicit val trfFloat = at[Float](_.toString.toFloat)(identity)
  implicit val trfDouble = at[Double](_.toString.toDouble)(identity)
  implicit val trfString = at[String](_.toString)(identity)
  implicit val trfByteArray = at[Array[Byte]](
    x => BaseEncoding.base64().decode(x.toString))(
    x => BaseEncoding.base64().encode(x))
//
//  import TimestampConverter._
//  implicit val trfInstant = at(toInstant)(fromInstant)
//  implicit val trfDate = at(toLocalDate)(fromLocalDate)
//  implicit val trfTime = at(toLocalTime)(fromLocalTime)
//  implicit val trfDateTime = at(toLocalDateTime)(fromLocalDateTime)
}

object Test {

  case class A(i: Int, b: Boolean, s: String)
  case class B(a: A, i: Int)

  def main(args: Array[String]): Unit = {
    implicitly[TableRowType[Int]]
    implicitly[TableRowType[A]]
    val t = implicitly[TableRowType[B]]
    println(t(B(A(1, false, "a"), 123)))
  }
}