package definiti.native

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import spray.json.{DefaultJsonProtocol, JsArray, JsBoolean, JsNumber, JsObject, JsString, JsValue, JsonFormat, RootJsonFormat, deserializationError}

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

  case class ValidationDeserializationException(errors: Seq[Error]) extends RuntimeException {
    override def getMessage: String = {
      JsObject(
        errors.map { error =>
          error.path -> JsArray(error.messages.map(messageToJsObject): _*)
        }: _*
      ).compactPrint
    }

    override def toString: String = getMessage
  }

  def formatWithValidation[A](defaultFormat: RootJsonFormat[A], verification: Verification[A]): RootJsonFormat[A] = {
    new RootJsonFormat[A] {
      def write(obj: A): JsValue = defaultFormat.write(obj)
      def read(json: JsValue): A = {
        verification.verify(defaultFormat.read(json)) match {
          case Valid(value) => value
          case Invalid(errors) => throw ValidationDeserializationException(errors)
        }
      }
    }
  }
  
  def messageToJsObject(message: Message): JsObject = message match {
    case Message0(key) => JsObject("key" -> JsString(key))
    case Message1(key, p1) => JsObject("key" -> JsString(key), "parameters" -> JsArray(Seq(p1).map(parameterToJson): _*))
    case Message2(key, p1, p2) => JsObject("key" -> JsString(key), "parameters" -> JsArray(Seq(p1, p2).map(parameterToJson): _*))
    case Message3(key, p1, p2, p3) => JsObject("key" -> JsString(key), "parameters" -> JsArray(Seq(p1, p2, p3).map(parameterToJson): _*))
    case Message4(key, p1, p2, p3, p4) => JsObject("key" -> JsString(key), "parameters" -> JsArray(Seq(p1, p2, p3, p4).map(parameterToJson): _*))
    case Message5(key, p1, p2, p3, p4, p5) => JsObject("key" -> JsString(key), "parameters" -> JsArray(Seq(p1, p2, p3, p4, p5).map(parameterToJson): _*))
    case Message6(key, p1, p2, p3, p4, p5, p6) => JsObject("key" -> JsString(key), "parameters" -> JsArray(Seq(p1, p2, p3, p4, p5, p6).map(parameterToJson): _*))
    case Message7(key, p1, p2, p3, p4, p5, p6, p7) => JsObject("key" -> JsString(key), "parameters" -> JsArray(Seq(p1, p2, p3, p4, p5, p6, p7).map(parameterToJson): _*))
    case Message8(key, p1, p2, p3, p4, p5, p6, p7, p8) => JsObject("key" -> JsString(key), "parameters" -> JsArray(Seq(p1, p2, p3, p4, p5, p6, p7, p8).map(parameterToJson): _*))
    case Message9(key, p1, p2, p3, p4, p5, p6, p7, p8, p9) => JsObject("key" -> JsString(key), "parameters" -> JsArray(Seq(p1, p2, p3, p4, p5, p6, p7, p8, p9).map(parameterToJson): _*))
    case Message10(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10) => JsObject("key" -> JsString(key), "parameters" -> JsArray(Seq(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10).map(parameterToJson): _*))
    case Message11(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11) => JsObject("key" -> JsString(key), "parameters" -> JsArray(Seq(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11).map(parameterToJson): _*))
    case Message12(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12) => JsObject("key" -> JsString(key), "parameters" -> JsArray(Seq(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12).map(parameterToJson): _*))
    case Message13(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13) => JsObject("key" -> JsString(key), "parameters" -> JsArray(Seq(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13).map(parameterToJson): _*))
    case Message14(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14) => JsObject("key" -> JsString(key), "parameters" -> JsArray(Seq(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14).map(parameterToJson): _*))
    case Message15(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15) => JsObject("key" -> JsString(key), "parameters" -> JsArray(Seq(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15).map(parameterToJson): _*))
    case Message16(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16) => JsObject("key" -> JsString(key), "parameters" -> JsArray(Seq(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16).map(parameterToJson): _*))
    case Message17(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17) => JsObject("key" -> JsString(key), "parameters" -> JsArray(Seq(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17).map(parameterToJson): _*))
    case Message18(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18) => JsObject("key" -> JsString(key), "parameters" -> JsArray(Seq(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18).map(parameterToJson): _*))
    case Message19(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19) => JsObject("key" -> JsString(key), "parameters" -> JsArray(Seq(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19).map(parameterToJson): _*))
    case Message20(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20) => JsObject("key" -> JsString(key), "parameters" -> JsArray(Seq(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20).map(parameterToJson): _*))
    case Message21(key, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21) => JsObject("key" -> JsString(key), "parameters" -> JsArray(Seq(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21).map(parameterToJson): _*))
  }

  def parameterToJson(value: Any): JsValue = value match {
    case boolean: Boolean => JsBoolean(boolean)
    case number: BigDecimal => JsNumber(number)
    case _ => JsString(value.toString)
  }
}