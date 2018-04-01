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
          case Valid(validValue) => JsSuccess(validValue)
          case Invalid(errors) => JsError(
            errors.map { error =>
              pathToJsonPath(error.path) -> error.messages.map(messageToJsonValidationError)
            }
          )
        }
      }
    }
    OFormat(reads, defaultFormat)
  }

  private def pathToJsonPath(path: String): JsPath = {
    val normalizedPath = path
      .replaceAll("\\]\\.", ".")
      .replaceAll("\\[", ".")

    val parts = normalizedPath.split("\\.")
    val nodes = parts.map { part =>
      try {
        IdxPathNode(part.toInt)
      } catch {
        case _: Exception => KeyPathNode(part)
      }
    }
    JsPath(nodes.toList)
  }

  def messageToJsonValidationError(message: Message): JsonValidationError = message match {
    case Message0(key) => JsonValidationError(key)
    case Message1(key, p1) => JsonValidationError(key, p1)
    case Message2(key, p1, p2) => JsonValidationError(key, p1, p2)
    case Message3(key, p1, p2, p3) => JsonValidationError(key, p1, p2, p3)
    case Message4(key, p1, p2, p3, p4) => JsonValidationError(key, p1, p2, p3, p4)
    case Message5(key, p1, p2, p3, p4, p5) => JsonValidationError(key, p1, p2, p3, p4, p5)
    case Message6(key, p1, p2, p3, p4, p5, p6) => JsonValidationError(key, p1, p2, p3, p4, p5, p6)
    case Message7(key, p1, p2, p3, p4, p5, p6, p7) => JsonValidationError(key, p1, p2, p3, p4, p5, p6, p7)
    case Message8(key, p1, p2, p3, p4, p5, p6, p7, p8) => JsonValidationError(key, p1, p2, p3, p4, p5, p6, p7, p8)
    case Message9(key, p1, p2, p3, p4, p5, p6, p7, p8, p9) => JsonValidationError(key, p1, p2, p3, p4, p5, p6, p7, p8, p9)
    case Message10(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10) => JsonValidationError(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)
    case Message11(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11) => JsonValidationError(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)
    case Message12(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12) => JsonValidationError(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)
    case Message13(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13) => JsonValidationError(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13)
    case Message14(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14) => JsonValidationError(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14)
    case Message15(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15) => JsonValidationError(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15)
    case Message16(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16) => JsonValidationError(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16)
    case Message17(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17) => JsonValidationError(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17)
    case Message18(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18) => JsonValidationError(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18)
    case Message19(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19) => JsonValidationError(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19)
    case Message20(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20) => JsonValidationError(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20)
    case Message21(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21) => JsonValidationError(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21)
  }
}
