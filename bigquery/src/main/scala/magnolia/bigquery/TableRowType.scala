package magnolia.bigquery

import com.google.api.services.bigquery.model.TableRow
import com.google.common.io.BaseEncoding
import magnolia._
import magnolia.data.Converter
import magnolia.shims._

import scala.collection.JavaConverters._
import scala.language.experimental.macros

sealed trait TableRowType[T] extends Converter[T, TableRow] {
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

class TableRowField[V](val getFn: Any => V)(val putFn: V => Any)
  extends TableRowType[V] with Converter.Field[V, TableRow] {
  override def get(r: R, k: String): V = getFn(r.get(k))
  override def put(r: R, k: String, v: V): R = {
    r.put(k, putFn(v))
    r
  }

  def imap[U](f: V => U)(g: U => V): TableRowField[U] =
    new TableRowField[U](v => f(getFn(v)))(v => putFn(g(v)))
}

object TableRowType extends TableRowTypeImplicits {
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

trait TableRowTypeImplicits {
  def at[V](f: Any => V)(g: V => Any): TableRowType[V] = new TableRowField[V](f, g)

  implicit val trtBool = at[Boolean](_.toString.toBoolean)(identity)
  implicit val trtInt = at[Int](_.toString.toInt)(identity)
  implicit val trtLong = at[Long](_.toString.toLong)(identity)
  implicit val trtFloat = at[Float](_.toString.toFloat)(identity)
  implicit val trtDouble = at[Double](_.toString.toDouble)(identity)
  implicit val trtString = at[String](_.toString)(identity)
  implicit val trtByteArray = at[Array[Byte]](
    x => BaseEncoding.base64().decode(x.toString))(
    x => BaseEncoding.base64().encode(x))

  import TimestampConverter._
  implicit val trtInstant = at(toInstant)(fromInstant)
  implicit val trtDate = at(toLocalDate)(fromLocalDate)
  implicit val trtTime = at(toLocalTime)(fromLocalTime)
  implicit val trtDateTime = at(toLocalDateTime)(fromLocalDateTime)

  implicit def trfOption[V](implicit t: TableRowType[V]): TableRowType[Option[V]] = {
    t match {
      case t: TableRowRecord[V] => new TableRowRecord[Option[V]] {
        override def from(r: R): Option[V] = ???
        override def to(t: Option[V]): R = ???
      }
      case t: TableRowField[V] => new TableRowField[Option[V]](t.getFn)(t.putFn) {

      }
    }
  }
//    new TableRowField[Option[V]]({ v =>
//      f.
//    })()
//      override def fromField(v: Any): Option[V] = ???
//      override def toField(v: Option[V]): Any = ???
//      override def get(r: R, k: String): Option[V] =
//        Option(r.get(k)).map(f.fromField)
//      override def put(r: R, k: String, v: Option[V]): Unit =
//        v.foreach(x => r.put(k, f.toField(x)))
//    }

//  implicit def trfSeq[V, S[V]](implicit f: TableRowField[V],
//                               toSeq: S[V] => Seq[V],
//                               fc: FactoryCompat[V, S[V]]): TableRowField[S[V]] =
//    new TableRowField[S[V]] {
//      override def fromField(v: Any): S[V] = ???
//      override def toField(v: S[V]): Any = ???
//      override def get(r: R, k: String): S[V] = r.get(k) match {
//        case null => fc.newBuilder.result()
//        case xs: java.util.List[_] => fc.build(xs.asScala.iterator.map(f.fromField))
//      }
//      override def put(r: R, k: String, v: S[V]): Unit =
//        r.put(k, toSeq(v).map(f.toField).asJava)
//    }
}
