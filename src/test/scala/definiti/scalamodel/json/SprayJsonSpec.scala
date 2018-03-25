package definiti.scalamodel.json

import definiti.scalamodel.{JsonConfiguration, JsonFormat}
import definiti.scalamodel.helpers.{ConfigurationMock, EndToEndSpec}

class SprayJsonSpec extends EndToEndSpec {
  "The generator" should "generate the simple json of all defined type when no validation" in {
    processSample("json.spray.withoutValidation", ConfigurationMock(
      json = JsonConfiguration(
        format = JsonFormat.spray,
        validation = false
      )
    ))
  }

  it should "generate the json of all defined type with flat validation" in {
    processSample("json.spray.withValidation", ConfigurationMock(
      json = JsonConfiguration(
        format = JsonFormat.spray,
        validation = true
      )
    ))
  }
}