package magnolia.bigquery.test

import com.google.api.services.bigquery.model.TableRow
import magnolia.bigquery.semiauto.{TableRowDerivation, TableRowMappable}
import magnolia.scalacheck.auto._
import magnolia.test.Simple._
import org.scalacheck._

object TableRowDerivationSpec {

}

object Test {
  def test[T: Arbitrary : TableRowMappable]: Unit = {
    val g = implicitly[Arbitrary[T]].arbitrary
    val m = implicitly[TableRowMappable[T]]
    val r1 = g.sample.get
    println(r1)
    val tr = m.to(r1)
    println(r1)
    val r2 = m.from(tr)
    println(r2)
  }

  def main(args: Array[String]): Unit = {
    val r = new TableRow().set("i", 1)
    val m = TableRowDerivation[Nullable]
    println(m.from(r))
    import scala.collection.JavaConverters._
    val r1 = new TableRow().set("i", List(1, 2, 3).asJava)
    val m1 = TableRowDerivation[Repeated]
    println(m1.from(r1))
//    test[Integers]
//    test[Numbers]
//    test[Required]
//    test[Nullable]
//    test[Repeated]
//    test[Nested]
  }
}
