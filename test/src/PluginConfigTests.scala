package explicitlyinferred

import utest.*

object PluginConfigTests extends TestSuite {
  import CompilerPluginTestSupport.expectIllegalArgument

  val tests = Tests {
    test("parses effect defaults") {
      val config = PluginConfig.parse(List("effectTypeRegex=.*Effect"))

      assert(config.effect.effectTypeRegex.pattern() == ".*Effect")
      assert(config.scope == Scope.Members)
      assert(config.effect.errorTypeParam == "E")
      assert(config.effect.resultTypeParam == "A")
      assert(config.effect.typeNameStyle == TypeNameStyle.Simple)
      assert(config.effect.markers == Markers("types", "/types"))
      assert(config.methodMatcher.matches("anything"))
    }

    test("requires effectTypeRegex") {
      val error = expectIllegalArgument(PluginConfig.parse(Nil))
      assert(error.getMessage.contains("effectTypeRegex"))
    }

    test("parses repeatable and last-wins options") {
      val config = PluginConfig.parse(List(
        "effectTypeRegex=First",
        "effectTypeRegex=Second",
        "scope=all",
        "errorTypeParam=Failure",
        "resultTypeParam=Success",
        "additionalErrorType=One",
        "additionalErrorType=Two",
        "excludeErrorTypeRegex=Internal.*",
        "excludeErrorTypeRegex=Hidden.*",
        "typeNameStyle=full",
        "startMarker=effect-types",
        "endMarker=/effect-types"
      ))

      assert(config.effect.effectTypeRegex.pattern() == "Second")
      assert(config.scope == Scope.All)
      assert(config.effect.errorTypeParam == "Failure")
      assert(config.effect.resultTypeParam == "Success")
      assert(config.effect.additionalErrorTypes == List("One", "Two"))
      assert(config.effect.excludedErrorTypes.map(_.pattern()) == List("Internal.*", "Hidden.*"))
      assert(config.effect.typeNameStyle == TypeNameStyle.Full)
      assert(config.effect.markers == Markers("effect-types", "/effect-types"))
    }

    test("rejects removed return-comment options") {
      Seq(
        "mode=effectScaladoc",
        "maxTypeLength=80",
        "managedTag=@inferredReturnType",
        "showTypeArgs=true",
        "showTypeParamNames=true",
        "effectStartMarker=types",
        "effectEndMarker=/types"
      ).foreach { option =>
        val error = expectIllegalArgument(
          PluginConfig.parse(List("effectTypeRegex=.*Effect", option))
        )
        assert(error.getMessage.contains("Unknown explicitlyInferred option"))
      }
    }

    test("rejects invalid scalar options") {
      Seq(
        "effectTypeRegex=(",
        "scope=package",
        "errorTypeParam=not valid",
        "resultTypeParam=",
        "additionalErrorType=",
        "excludeErrorTypeRegex=(",
        "typeNameStyle=qualified",
        "startMarker=",
        "endMarker=line1\nline2",
        "startMarker=bad--marker"
      ).foreach { option =>
        expectIllegalArgument(
          PluginConfig.parse(List("effectTypeRegex=.*Effect", option))
        )
      }
    }

    test("rejects equal markers") {
      val error = expectIllegalArgument(
        PluginConfig.parse(List("effectTypeRegex=.*Effect", "startMarker=same", "endMarker=same"))
      )
      assert(error.getMessage.contains("markers must be different"))
    }
  }
}
