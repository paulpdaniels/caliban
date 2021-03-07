package caliban.client

import scala.util.Try
import java.util.UUID
import caliban.client.CalibanClientError.DecodingError
import caliban.client.__Value._
import io.circe.Json

import java.time.{ Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, OffsetTime, ZonedDateTime }
import java.time.format.DateTimeFormatter
import java.time.temporal.Temporal
import scala.annotation.implicitNotFound
import scala.util.control.NonFatal

/**
 * Typeclass that defines how to decode a scalar from a GraphQL response into a proper value of type `A`.
 */
@implicitNotFound(
  """Cannot find a ScalarDecoder for type ${A}.
     
Caliban needs it to know how to decode a scalar of type ${A}.
"""
)
trait ScalarDecoder[+A] { self =>
  def decode(value: __Value): Either[DecodingError, A]
}

object ScalarDecoder extends TemporalDecoders {
  implicit val int: ScalarDecoder[Int] = {
    case __NumberValue(value) =>
      Try(value.toIntExact).toEither.left.map(ex => DecodingError(s"Can't build an Int from input $value", Some(ex)))
    case other => Left(DecodingError(s"Can't build an Int from input $other"))
  }
  implicit val long: ScalarDecoder[Long] = {
    case __NumberValue(value) =>
      Try(value.toLongExact).toEither.left.map(ex => DecodingError(s"Can't build a Long from input $value", Some(ex)))
    case other => Left(DecodingError(s"Can't build a Long from input $other"))
  }
  implicit val bigInt: ScalarDecoder[BigInt] = {
    case __NumberValue(value) =>
      Try(value.toBigIntExact).toEither.left
        .map(ex => DecodingError(s"Can't build a BigInt from input $value", Some(ex)))
        .flatMap {
          case None    => Left(DecodingError(s"Can't build a BigInt from input $value"))
          case Some(v) => Right(v)
        }
    case other => Left(DecodingError(s"Can't build a BigInt from input $other"))
  }
  implicit val float: ScalarDecoder[Float] = {
    case __NumberValue(value) => Right(value.toFloat)
    case other                => Left(DecodingError(s"Can't build a Float from input $other"))
  }
  implicit val double: ScalarDecoder[Double] = {
    case __NumberValue(value) => Right(value.toDouble)
    case other                => Left(DecodingError(s"Can't build a Double from input $other"))
  }
  implicit val bigDecimal: ScalarDecoder[BigDecimal] = {
    case __NumberValue(value) => Right(value)
    case other                => Left(DecodingError(s"Can't build a BigDecimal from input $other"))
  }
  implicit val boolean: ScalarDecoder[Boolean] = {
    case __BooleanValue(value) => Right(value)
    case other                 => Left(DecodingError(s"Can't build a Boolean from input $other"))
  }
  implicit val string: ScalarDecoder[String] = {
    case __StringValue(value) => Right(value)
    case other                => Left(DecodingError(s"Can't build a String from input $other"))
  }
  implicit val uuid: ScalarDecoder[UUID] = {
    case __StringValue(value) =>
      Try(UUID.fromString(value)).toEither.left
        .map(ex => DecodingError(s"Can't build a UUID from input $value", Some(ex)))
    case other => Left(DecodingError(s"Can't build a UUID from input $other"))
  }
  implicit val unit: ScalarDecoder[Unit] = {
    case __ObjectValue(Nil) => Right(())
    case other              => Left(DecodingError(s"Can't build Unit from input $other"))
  }
  implicit val json: ScalarDecoder[Json] = value => Right(__Value.valueEncoder(value))

}

trait TemporalDecoders {
  private abstract class TemporalDecoder[A](name: String) extends ScalarDecoder[A] {
    protected[this] def parseUnsafe(input: String): A

    override def decode(input: __Value): Either[DecodingError, A] = input match {
      case __StringValue(value) =>
        try Right(parseUnsafe(value))
        catch {
          case NonFatal(e) =>
            val message = e.getMessage
            if (message.eq(null)) Left(DecodingError(s"Can't build $name from $value", innerThrowable = Some(e)))
            else Left(DecodingError(s"Can't build $name from $value ($message)", innerThrowable = Some(e)))
        }
      case _ =>
        Left(DecodingError(s"Can't build a $name from $input"))
    }
  }

  private object TemporalDecoder {
    def apply[T <: Temporal](name: String)(parse: String => T): TemporalDecoder[T] = new TemporalDecoder[T](name) {
      override protected[this] def parseUnsafe(input: String): T = parse(input)
    }
  }

  final def localDateWithFormatter(formatter: DateTimeFormatter): ScalarDecoder[LocalDate] =
    TemporalDecoder("LocalDate")(LocalDate.parse(_, formatter))
  final def localTimeWithFormatter(formatter: DateTimeFormatter): ScalarDecoder[LocalTime] =
    TemporalDecoder("LocalTime")(LocalTime.parse(_, formatter))
  final def localDateTimeWithFormatter(formatter: DateTimeFormatter): ScalarDecoder[LocalDateTime] =
    TemporalDecoder("LocalDateTime")(LocalDateTime.parse(_, formatter))
  final def offsetTimeWithFormatter(formatter: DateTimeFormatter): ScalarDecoder[OffsetTime] =
    TemporalDecoder("OffsetTime")(OffsetTime.parse(_, formatter))
  final def offsetDateTimeWithFormatter(formatter: DateTimeFormatter): ScalarDecoder[OffsetDateTime] =
    TemporalDecoder("OffsetDateTime")(OffsetDateTime.parse(_, formatter))
  final def zonedDateTimeWithFormatter(formatter: DateTimeFormatter): ScalarDecoder[ZonedDateTime] =
    TemporalDecoder("ZonedDateTime")(ZonedDateTime.parse(_, formatter))

  lazy val instantEpoch: ScalarDecoder[Instant] = {
    case i: __NumberValue => Right(Instant.ofEpochMilli(i.value.toLong))
    case value            => Left(DecodingError(s"Can't build an Instant from $value"))
  }

  implicit lazy val instant: ScalarDecoder[Instant]             = TemporalDecoder("Instant")(Instant.parse)
  implicit lazy val localDate: ScalarDecoder[LocalDate]         = TemporalDecoder("LocalDate")(LocalDate.parse)
  implicit lazy val localTime: ScalarDecoder[LocalTime]         = TemporalDecoder("LocalTime")(LocalTime.parse)
  implicit lazy val localDateTime: ScalarDecoder[LocalDateTime] = TemporalDecoder("LocalDateTime")(LocalDateTime.parse)
  implicit lazy val offsetTime: ScalarDecoder[OffsetTime]       = TemporalDecoder("OffsetTime")(OffsetTime.parse)
  implicit lazy val zonedDateTime: ScalarDecoder[ZonedDateTime] = TemporalDecoder("ZonedDateTime")(ZonedDateTime.parse)
  implicit lazy val offsetDateTime: ScalarDecoder[OffsetDateTime] =
    TemporalDecoder("OffsetDateTime")(OffsetDateTime.parse)

}
