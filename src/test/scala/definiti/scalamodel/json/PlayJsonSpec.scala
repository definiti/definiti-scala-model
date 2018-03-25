package definiti.scalamodel.json

import definiti.scalamodel.{JsonConfiguration, JsonFormat}
import definiti.scalamodel.helpers.{ConfigurationMock, EndToEndSpec}

class PlayJsonSpec extends EndToEndSpec {
  "The generator" should "generate the simple json of all defined types when no validation" in {
    processSample("json.play.withoutValidation", ConfigurationMock(
      json = JsonConfiguration(
        format = JsonFormat.play,
        validation = false
      )
    ))
  }

  it should "generate the json of all defined types with flat validation" in {
    processSample("json.play.withValidation", ConfigurationMock(
      json = JsonConfiguration(
        format = JsonFormat.play,
        validation = true
      )
    ))
  }
}