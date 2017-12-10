package definiti.native

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import play.api.libs.json._

object JsonPlaySupport {
  private val datetimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME

  implicit lazy val dateFormat: Format[LocalDateTime] = Format(dateReads, dateWrites)

  private lazy val dateReads: Reads[LocalDateTime] = Reads { jsValue =>
    jsValue.validate[String] flatMap { jsString =>
      try {
        JsSuccess(LocalDateTime.parse(jsString, datetimeFormatter))
      } catch {
        case _: Throwable => JsError(Seq(JsPath() -> Seq(JsonValidationError(s"Expected ISO date, got: $jsString"))))
      }
    }
  }

  private lazy val dateWrites: Writes[LocalDateTime] = Writes[LocalDateTime] {
    date => JsString(date.format(datetimeFormatter))
  }

  def formatWithValidation[A](defaultFormat: OFormat[A], verification: Verification[A]): OFormat[A] = {
    val reads: Reads[A] = Reads { json =>
      defaultFormat.reads(json).flatMap { value =>
        verification.verify(value) match {
          case Valid(value) => JsSuccess(value)
          case Invalid(errors) => JsError(Seq(JsPath() -> Seq(JsonValidationError(errors))))
        }
      }
    }
    OFormat(reads, defaultFormat)
  }
}
