/*
 * Copyright 2021 Spotify AB.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package magnolify.parquet.test

import java.time._

import cats._
import magnolify.cats.auto._
import magnolify.avro._
import magnolify.avro.unsafe._
import magnolify.parquet._
import magnolify.parquet.unsafe._
import magnolify.scalacheck.auto._
import magnolify.shims.JavaConverters._
import magnolify.test._
import magnolify.test.Simple._
import magnolify.test.Time._
import org.apache.avro.Schema
import org.apache.avro.generic.GenericRecord
import org.apache.hadoop.conf.Configuration
import org.apache.parquet.avro.{AvroParquetReader, AvroParquetWriter, AvroReadSupport}
import org.scalacheck._

import scala.reflect.ClassTag

class AvroParquetSuite extends MagnolifySuite {
  private def test[T: Arbitrary: ClassTag](implicit
    at: AvroType[T],
    pt: ParquetType[T],
    eq: Eq[T]
  ): Unit = {
    val name = className[T]
    val conf = new Configuration()
    AvroReadSupport.setAvroReadSchema(conf, shade(at.schema))

    property(s"$name.avro2parquet") {
      Prop.forAll { t: T =>
        val r = at(t)

        val out = new TestOutputFile
        val writer = AvroParquetWriter.builder[GenericRecord](out).withSchema(at.schema).build()
        writer.write(r)
        writer.close()

        val in = new TestInputFile(out.getBytes)
        val reader = pt.readBuilder(in).build()
        val copy = reader.read()
        val next = reader.read()
        reader.close()
        Prop.all(eq.eqv(t, copy), next == null)
      }
    }

    property(s"$name.parquet2avro") {
      Prop.forAll { t: T =>
        val out = new TestOutputFile
        val writer = pt.writeBuilder(out).build()
        writer.write(t)
        writer.close()

        val in = new TestInputFile(out.getBytes)
        val reader = AvroParquetReader.builder[GenericRecord](in).withConf(conf).build()

        val copy = reader.read()
        val next = reader.read()
        reader.close()
        Prop.all(eq.eqv(t, at(copy)), next == null)
      }
    }
  }

  // Shade namespaces to prevent reader from confusing case classes as specific records
  // E.g. java.lang.NoSuchMethodException: magnolify.test.Simple$Integers.<init>()
  private def shade(schema: Schema): Schema = schema.getType match {
    case Schema.Type.RECORD =>
      val fields = schema.getFields.asScala.map { f =>
        new Schema.Field(f.name(), shade(f.schema()), f.doc(), f.defaultVal())
      }.asJava
      val ns = "shaded." + schema.getNamespace
      Schema.createRecord(schema.getName, schema.getDoc, ns, false, fields)
    case Schema.Type.ENUM =>
      val ns = "shaded." + schema.getNamespace
      Schema.createEnum(schema.getName, schema.getDoc, ns, schema.getEnumSymbols)
    case Schema.Type.ARRAY =>
      Schema.createArray(shade(schema.getElementType))
    case Schema.Type.MAP =>
      Schema.createMap(shade(schema.getValueType))
    case Schema.Type.UNION =>
      Schema.createUnion(schema.getTypes.asScala.map(shade).asJava)
    case _ => schema
  }

  test[Integers]
  test[Floats]
  test[Required]
  test[Nullable]

  {
    import magnolify.parquet.ParquetArray.AvroCompat._
    test[Repeated]
    test[Nested]
  }

  test[Unsafe]

  {
    import Collections._
    import magnolify.parquet.ParquetArray.AvroCompat._

    test[Collections]
    test[MoreCollections]
  }

  {
    import Enums._
    test[Enums]
  }

  {
    implicit val eqByteArray: Eq[Array[Byte]] = Eq.by(_.toList)
    test[ParquetTypes]
  }

  {
    import magnolify.avro.logical.bigquery._
    // Precision = number of digits, so 5 means -99999 to 99999
    val nines = math.pow(10, 38).toLong - 1
    implicit val arbBigDecimal: Arbitrary[BigDecimal] =
      Arbitrary(Gen.choose(-nines, nines).map(BigDecimal(_)))
    implicit val pfBigDecimal: ParquetField[BigDecimal] = ParquetField.decimalBinary(38, 9)
    test[DecimalBinary]
  }

  test[AvroParquetLogical]

  {
    import magnolify.avro.logical.millis._
    import magnolify.parquet.logical.millis._
    test[AvroParquetTimeMillis]
  }

  {
    import magnolify.avro.logical.micros._
    import magnolify.parquet.logical.micros._
    test[AvroParquetTimeMicros]
  }
}

case class AvroParquetLogical(d: LocalDate)
case class AvroParquetTimeMillis(i: Instant, dt: LocalDateTime, t: LocalTime)
case class AvroParquetTimeMicros(i: Instant, dt: LocalDateTime, t: LocalTime)