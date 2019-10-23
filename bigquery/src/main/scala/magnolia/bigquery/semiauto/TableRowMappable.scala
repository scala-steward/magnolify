package magnolia.bigquery.semiauto

import com.google.api.services.bigquery.model.TableRow
import magnolia._
import magnolia.shims._

import scala.collection.JavaConverters._
import scala.language.experimental.macros

trait TableRowMappable[V] {
  type M = TableRow
  def empty: M = new TableRow

  // for record values
  def apply(m: M): V = ???
  def apply(v: V): M = ???

  // for leaf values
  def fromLeaf(v: Any): V
  def toLeaf(v: V): Any

  def get(m: M, k: String): V = fromLeaf(m.get(k))
  def put(m: M, k: String, v: V): Unit = m.put(k, toLeaf(v))
}

object TableRowMappable {
  implicit def optionalTableRowM[V](implicit trm: TableRowMappable[V])
  : TableRowMappable[Option[V]] =
    new TableRowMappable[Option[V]] {
      override def fromLeaf(v: Any): Option[V] = ???
      override def toLeaf(v: Option[V]): Any = ???

      override def get(m: M, k: String): Option[V] =
        Option(m.get(k)).map(trm.fromLeaf)
      override def put(m: M, k: String, v: Option[V]): Unit =
        v.foreach(x => m.put(k, trm.toLeaf(x)))
    }

  implicit def seqTableRowM[V, S[V]](implicit trm: TableRowMappable[V],
                                     toSeq: S[V] => Seq[V],
                                     fc: FactoryCompat[V, S[V]]): TableRowMappable[S[V]] =
    new TableRowMappable[S[V]] {
      override def fromLeaf(v: Any): S[V] = ???
      override def toLeaf(v: S[V]): Any = ???

      override def get(m: M, k: String): S[V] = m.get(k) match {
        case null => fc.newBuilder.result()
        case xs: java.util.List[Any] =>
          fc.newBuilder.addAll(xs.asScala.iterator.map(trm.fromLeaf)).result()
      }
      override def put(m: M, k: String, v: S[V]): Unit =
        m.put(k, toSeq(v).iterator.map(trm.toLeaf).asJava)
    }

  def at[V](fromFn: Any => V, toFn: V => Any): TableRowMappable[V] = new TableRowMappable[V] {
    override def fromLeaf(v: Any): V = fromFn(v)
    override def toLeaf(v: V): Any = toFn(v)
  }

  implicit val trmBool = at[Boolean](_.toString.toBoolean, identity)
  implicit val trmInt = at[Int](_.toString.toInt, identity)
  implicit val trmLong = at[Long](_.toString.toLong, identity)
  implicit val trmFloat = at[Float](_.toString.toFloat, identity)
  implicit val trmDouble = at[Double](_.toString.toDouble, identity)
  implicit val trmString = at[String](_.toString, identity)

  // Magnolia

  type Typeclass[T] = TableRowMappable[T]

  def combine[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] = new Typeclass[T] {
    override def apply(m: M): T =
      caseClass.construct { p =>
        p.typeclass.get(m, p.label)
      }

    override def apply(v: T): M =
      caseClass.parameters.foldLeft(this.empty) { (m, p) =>
        p.typeclass.put(m, p.label, p.dereference(v))
        m
      }

    override def fromLeaf(v: Any): T = ???
    override def toLeaf(v: T): Any = ???
  }

  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] = ???

  implicit def apply[T]: Typeclass[T] = macro Magnolia.gen[T]
}
