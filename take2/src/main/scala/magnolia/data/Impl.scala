package magnolia.data

import com.google.api.services.bigquery.model.TableRow
import magnolia._
import magnolia.shims.FactoryCompat

import scala.collection.JavaConverters._
import scala.language.experimental.macros

object Impl

////////////////////////////////////////
// BigQuery
////////////////////////////////////////

trait TableRowType[T] extends ConverterType[T, TableRow, TableRow]

class TableRowRecord[V] extends RecordType[V, TableRow, TableRow, Any, Any] {
  override type FT = TableRowField[V]
  override def newBuilder: TableRow = new TableRow
  override def get(r: TableRow, k: String)(implicit f: FT): V = f.read(r.get(k))
  override def put(w: TableRow, k: String, T: V)(implicit f: FT): TableRow = {
    if (T != null) {
      w.put(k, f.write(T))
    }
    w
  }
}

object TableRowRecord {
  type Typeclass[V] = TableRowRecord[V]

  def combine[V](caseClass: CaseClass[Typeclass, V]): Typeclass[V] = new Typeclass[V] {
    override def get(r: TableRow, k: String)(implicit f: TableRowField[V]): V = {
      ???
    }

    override def put(w: TableRow, k: String, T: V)(implicit f: TableRowField[V]): TableRow = {
      ???
    }
  }

  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] = ???

  implicit def apply[T]: Typeclass[T] = macro Magnolia.gen[T]
}

trait TableRowField[V] extends FieldType[V, Any, Any]

object TableRowField {
  def at[V](f: Any => V)(g: V => Any): TableRowField[V] = new TableRowField[V] {
    override def read(v: Any): V = f(v)
    override def write(v: V): Any = g(v)
  }
  implicit val intTableRowField = at[Int](_.toString.toInt)(identity)
  implicit val stringTableRowField = at[String](_.toString)(identity)

  implicit def optionTableRowField[V](implicit f: TableRowField[V]): TableRowField[Option[V]] =
    new TableRowField[Option[V]] {
      override def read(v: Any): Option[V] = v.asInstanceOf[java.util.List[Any]] match {
        case null => None
        case xs if xs.isEmpty => None
        case xs if xs.size == 1 => Some(f.read(xs.get(0)))
        case _ => throw new IllegalStateException("Optional field with more than 1 values")
      }
      override def write(v: Option[V]): Any = v match {
        case None => null
        case Some(x) => f.write(x)
      }
    }

  implicit def seqTableRowField[V, S[V]](implicit f: TableRowField[V],
                                         ts: S[V] => Seq[V],
                                         fc: FactoryCompat[V, S[V]]): TableRowField[S[V]] =
    new TableRowField[S[V]] {
      override def read(v: Any): S[V] = v.asInstanceOf[java.util.List[Any]] match {
        case null => fc.newBuilder.result()
        case xs => fc.build(xs.asScala.iterator.map(f.read))
      }
      override def write(v: S[V]): Any = ts(v).asJava
    }
}

////////////////////////////////////////
// TensorFlow
////////////////////////////////////////