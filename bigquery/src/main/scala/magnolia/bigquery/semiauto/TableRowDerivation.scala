package magnolia.bigquery.semiauto

import com.google.api.services.bigquery.model.TableRow
import magnolia._
import magnolia.shims._

import scala.collection.JavaConverters._
import scala.language.experimental.macros

trait TableRowMappable[V] {
  type M = TableRow
  def empty: M = new TableRow

  def from(v: Any): V
  def to(v: V): Any

  def get(m: M, k: String): V = from(m.get(k))
  def put(m: M, k: String, v: V): Unit = m.put(k, to(v))
}

object TableRowMappable {
  implicit def optionalTableRowM[V](implicit trm: TableRowMappable[V])
  : TableRowMappable[Option[V]] =
    new TableRowMappable[Option[V]] {
      override def from(v: Any): Option[V] = ???
      override def to(v: Option[V]): Any = ???

      override def get(m: M, k: String): Option[V] =
        Option(m.get(k)).map(trm.from)
      override def put(m: M, k: String, v: Option[V]): Unit =
        v.foreach(x => m.put(k, trm.to(x)))
    }

  implicit def seqTableRowM[V, S[V]](implicit trm: TableRowMappable[V],
                                     toSeq: S[V] => Seq[V],
                                     fc: FactoryCompat[V, S[V]]): TableRowMappable[S[V]] =
    new TableRowMappable[S[V]] {
      override def from(v: Any): S[V] = ???
      override def to(v: S[V]): Any = ???

      override def get(m: M, k: String): S[V] = m.get(k) match {
        case null => fc.newBuilder.result()
        case xs: java.util.List[Any] =>
          fc.newBuilder.addAll(xs.asScala.iterator.map(trm.from)).result()

      }
      override def put(m: M, k: String, v: S[V]): Unit =
        m.put(k, toSeq(v).iterator.map(trm.to).asJava)
    }

  def at[V](fromFn: Any => V, toFn: V => Any): TableRowMappable[V] = new TableRowMappable[V] {
    override def from(v: Any): V = fromFn(v)
    override def to(v: V): Any = toFn(v)
  }
  implicit def mkM[V]: TableRowMappable[V] = at(_.asInstanceOf[V], _.asInstanceOf[Any])
}

object TableRowDerivation {
  type Typeclass[T] = TableRowMappable[T]

  def combine[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] = new Typeclass[T] {
    override def from(v: Any): T =
      caseClass.construct { p =>
        p.typeclass.get(v.asInstanceOf[this.M], p.label)
      }

    override def to(v: T): Any =
      caseClass.parameters.foldLeft(this.empty) { (m, p) =>
        p.typeclass.put(m, p.label, p.dereference(v))
        m
      }
  }

  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] = ???

  implicit def apply[T]: Typeclass[T] = macro Magnolia.gen[T]
}
