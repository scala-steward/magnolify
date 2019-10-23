package magnolia.bigquery.test

import com.google.api.services.bigquery.model.TableRow
import magnolia.bigquery.auto._
import magnolia.scalacheck.auto._
import magnolia.test.Simple._
import org.scalacheck._

import scala.reflect._

object TableRowTypeSpec extends Properties("TableRowType") {

  private def test[T: Arbitrary : ClassTag](implicit tpe: TableRowType[T]): Unit = {
    val name = classTag[T].runtimeClass.getSimpleName

    property(name) = Prop.forAll { t: T =>
      val tr = tpe(t)
      val copy = tpe(tr)
      copy == t
    }
  }

  test[Integers]
  test[Required]
  test[Nullable]
//  test[Nested]
}
