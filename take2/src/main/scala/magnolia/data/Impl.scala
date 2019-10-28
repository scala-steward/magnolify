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

object TableRowType {
  def apply[T](implicit rt: TableRowRecord[T]): TableRowType[T] = new TableRowType[T] {
    override val recordType: RecordType[T, TableRow, TableRow] = rt
  }
}

trait TableRowRecord[V] extends RecordType[V, TableRow, TableRow] {
  override def newBuilder: TableRow = new TableRow
}

object TableRowRecord {
  type Typeclass[V] = TableRowRecord[V]

  def combine[V](caseClass: CaseClass[Typeclass, V]): Typeclass[V] = new Typeclass[V] {
    override def get(r: TableRow, k: String): V =
      caseClass.construct { p => p.typeclass.get(r, p.label) }

    override def put(w: TableRow, k: String, v: V): TableRow =
      caseClass.parameters.foldLeft(w) { (w, p) =>
        println("PUT", w, p.label, p.dereference(v))
        p.typeclass.put(w, p.label, p.dereference(v))
      }
  }

  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] = ???

  implicit def apply[T]: Typeclass[T] = macro Magnolia.gen[T]

  implicit def singletonTableRowRecord[V](implicit f: TableRowField[V]): TableRowRecord[V] =
    new TableRowRecord[V] {
      override def get(r: TableRow, k: String): V = f.read(r.get(k))
      override def put(w: TableRow, k: String, v: V): TableRow = {
        if (v != null) {
          w.put(k, f.write(v))
        }
        w
      }
    }
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
      override def read(v: Any): Option[V] = Option(v).map(f.read)
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