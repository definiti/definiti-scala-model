package definiti.native

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import my.person.Person
import my.person.Person.allVerifications
import spray.json.{DefaultJsonProtocol, JsString, JsValue, JsonFormat, RootJsonFormat, deserializationError}

object JsonSpraySupport extends DefaultJsonProtocol {
  private val datetimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME

  implicit val dateTimeFormat: JsonFormat[LocalDateTime] = new JsonFormat[LocalDateTime] {
    override def write(obj: LocalDateTime): JsValue = JsString(obj.format(datetimeFormatter))

    override def read(json: JsValue): LocalDateTime = json match {
      case JsString(jsString) =>
        try {
          LocalDateTime.parse(jsString, datetimeFormatter)
        } catch {
          case _: Throwable => deserializationError(s"Expected ISO date, got: $jsString")
        }
      case x =>
        deserializationError(s"String expected, got: $x")
    }
  }

  def enumFormat[A <: Enumeration, B](enum: A, enumWithName: String => B): RootJsonFormat[B] = new RootJsonFormat[B] {
    override def write(obj: B): JsValue = JsString(obj.toString)

    override def read(json: JsValue): B = json match {
      case JsString(string) =>
        try {
          enumWithName(string)
        } catch {
          case _: Throwable => deserializationError(s"One of ${enum.values.mkString("('", "', '", "')")} expected, got: $string")
        }
      case x => deserializationError(s"String expected, got: $x")
    }
  }

  def formatWithValidation[A](defaultFormat: RootJsonFormat[A], verification: Verification[A]): RootJsonFormat[A] = {
    new RootJsonFormat[A] {
      def write(obj: A): JsValue = defaultFormat.write(obj)
      def read(json: JsValue): A = {
        verification.verify(defaultFormat.read(json)) match {
          case Valid(value) => value
          case Invalid(errors) => deserializationError(errors.mkString("\n"))
        }
      }
    }
  }
}