package magnolia.data

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
