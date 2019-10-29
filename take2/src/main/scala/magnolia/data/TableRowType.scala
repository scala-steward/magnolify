package magnolia.data

import com.google.api.services.bigquery.model.TableRow
import magnolia._
import magnolia.shims.FactoryCompat

import scala.collection.JavaConverters._
import scala.language.experimental.macros

trait TableRowType[T] extends ConverterType[T, TableRow, TableRow]

object TableRowType {
  def apply[T](implicit rt: TableRowRecord[T]): TableRowType[T] = new TableRowType[T] {
    override val recordType: RecordType[T, TableRow, TableRow] = rt
  }
}

trait TableRowRecord[V] extends RecordType[V, TableRow, TableRow] {
  override def newWriter: TableRow = new TableRow
}

object TableRowRecord extends LowPriorityTableRowImplicits {
  type Typeclass[V] = TableRowRecord[V]

  def combine[V](caseClass: CaseClass[Typeclass, V]): Typeclass[V] = new Typeclass[V] {
    override val nested: Boolean = true

    override def get(r: TableRow, k: String): V = caseClass.construct { p =>
      if (p.typeclass.nested) {
        p.typeclass.get(r.get(p.label).asInstanceOf[TableRow], null)
      } else {
        p.typeclass.get(r, p.label)
      }
    }

    override def put(w: TableRow, k: String, v: V): TableRow =
      caseClass.parameters.foldLeft(w) { (w, p) =>
        if (p.typeclass.nested) {
          w.put(p.label, p.typeclass.put(newWriter, null, p.dereference(v)))
          w
        } else {
          p.typeclass.put(w, p.label, p.dereference(v))
        }
      }
  }

  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] = ???

  implicit def apply[T]: Typeclass[T] = macro Magnolia.gen[T]
}

trait LowPriorityTableRowImplicits {
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

  implicit val longTableRowField = at[Long](_.toString.toLong)(identity)
  implicit val stringTableRowField = at[String](_.toString)(identity)

  implicit def recordTableRowField[V](implicit r: TableRowRecord[V]): TableRowField[V] =
    new TableRowField[V] {
      override def read(v: Any): V = r.get(v.asInstanceOf[TableRow], null)
      override def write(v: V): Any = r.put(r.newWriter, null, v)
    }

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
      override def write(v: S[V]): Any = ts(v).map(f.write).asJava
    }
}
