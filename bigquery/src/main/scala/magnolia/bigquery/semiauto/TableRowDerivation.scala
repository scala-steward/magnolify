package magnolia.bigquery.semiauto

import magnolia._

import scala.language.experimental.macros

//trait Mappable[M, V] {
//  def empty: M
//  def get(m: M, k: String): V
//  def put(m: M, k: String, v: V): Unit
//}
//trait TableRowMappable[V] extends Mappable[java.util.Map[String, Any], V] {
trait TableRowMappable[V] {
  type M = java.util.Map[String, Any]
  def from(v: Any): V
  def to(v: V): Any

  def empty: M = new java.util.LinkedHashMap[String, Any]
  def get(m: M, k: String): V = from(m.get(k))
  def put(m: M, k: String, v: V): Unit = m.put(k, to(v))
}

object TableRowMappable {
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

object Test {
  case class A(i: Int, s: String)
  case class B(i: Int, s: String, n: Option[Boolean], r: List[String])
  def main(args: Array[String]): Unit = {
    val m = TableRowDerivation[A]
    val a = m.to(A(1, "123"))
    println(a)
    val b = m.from(a)
    println(b)

    val m1 = TableRowDerivation[B]
    val a1 = m1.to(B(1, "123", Some(true), List("a", "b")))
    println(a1)
    val b1 = m1.from(a1)
    println(b1)
  }
}
