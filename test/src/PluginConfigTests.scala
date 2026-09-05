package explicitlyinferred

import utest.*

object PluginConfigTests extends TestSuite {
  import CompilerPluginTestSupport.expectIllegalArgument

  private val required = "typeParam=L:Left:dealias"

  val tests = Tests {
    test("parses neutral defaults") {
      val config = PluginConfig.parse(List(required))
      val parameter = config.documentation.parameters.head

      assert(config.documentation.typeRegex.isEmpty)
      assert(config.scope == Scope.Members)
      assert(parameter.name == "L")
      assert(parameter.heading == "Left")
      assert(parameter.aliasPolicy == AliasPolicy.Dealias)
      assert(parameter.additionalTypes.isEmpty)
      assert(parameter.excludedTypes.isEmpty)
      assert(config.documentation.typeNameStyle == TypeNameStyle.Simple)
      assert(config.documentation.markers == Markers("types", "/types"))
      assert(config.methodMatcher.matches("anything"))
    }

    test("requires a type parameter mapping") {
      val error = expectIllegalArgument(PluginConfig.parse(Nil))
      assert(error.getMessage.contains("typeParam"))
    }

    test("parses ordered mappings and targeted controls") {
      val config = PluginConfig.parse(List(
        "additionalType=L:Fallback:Value",
        "excludeTypeRegex=R:Internal:.*",
        "typeRegex=First",
        "typeRegex=Second",
        "scope=all",
        "typeParam=L:Left: side:dealias",
        "typeParam=R:Right:preserve",
        "typeNameStyle=full",
        "startMarker=inferred-types",
        "endMarker=/inferred-types"
      ))

      assert(config.documentation.typeRegex.map(_.pattern()).contains("Second"))
      assert(config.scope == Scope.All)
      assert(config.documentation.parameters.map(_.name) == List("L", "R"))
      assert(config.documentation.parameters.map(_.heading) == List("Left: side", "Right"))
      assert(config.documentation.parameters.map(_.aliasPolicy) == List(AliasPolicy.Dealias, AliasPolicy.Preserve))
      assert(config.documentation.parameters.head.additionalTypes == List("Fallback:Value"))
      assert(config.documentation.parameters(1).excludedTypes.map(_.pattern()) == List("Internal:.*"))
      assert(config.documentation.typeNameStyle == TypeNameStyle.Full)
      assert(config.documentation.markers == Markers("inferred-types", "/inferred-types"))
    }

    test("rejects effect-specific options") {
      Seq(
        "effectTypeRegex=.*Effect",
        "errorTypeParam=E",
        "resultTypeParam=A",
        "additionalErrorType=Unexpected",
        "excludeErrorTypeRegex=.*Internal"
      ).foreach { option =>
        val error = expectIllegalArgument(PluginConfig.parse(List(required, option)))
        assert(error.getMessage.contains("Unknown explicitlyInferred option"))
      }
    }

    test("rejects removed legacy options") {
      Seq(
        "mode=effectScaladoc",
        "maxTypeLength=80",
        "managedTag=@inferredReturnType",
        "showTypeArgs=true",
        "showTypeParamNames=true",
        "effectStartMarker=types",
        "effectEndMarker=/types"
      ).foreach { option =>
        val error = expectIllegalArgument(PluginConfig.parse(List(required, option)))
        assert(error.getMessage.contains("Unknown explicitlyInferred option"))
      }
    }

    test("rejects malformed scalar and mapping options") {
      Seq(
        "typeRegex=(",
        "scope=package",
        "typeParam=missing-separators",
        "typeParam=L::preserve",
        "typeParam=not valid:Label:preserve",
        "typeParam=L:Label:unknown",
        "typeParam=L:Label::preserve",
        "additionalType=L:",
        "additionalType=missing-target",
        "excludeTypeRegex=missing-target",
        "excludeTypeRegex=L:(",
        "typeNameStyle=qualified",
        "startMarker=",
        "endMarker=line1\nline2",
        "startMarker=bad--marker"
      ).foreach { option =>
        expectIllegalArgument(PluginConfig.parse(List(required, option)))
      }
    }

    test("rejects duplicate mappings and unknown targets") {
      Seq(
        List(required, "typeParam=L:Other:preserve"),
        List(required, "typeParam=R:Left:preserve"),
        List(required, "additionalType=R:Value"),
        List(required, "excludeTypeRegex=R:.*")
      ).foreach(options => expectIllegalArgument(PluginConfig.parse(options)))
    }

    test("rejects equal markers") {
      val error = expectIllegalArgument(
        PluginConfig.parse(List(required, "startMarker=same", "endMarker=same"))
      )
      assert(error.getMessage.contains("markers must be different"))
    }
  }
}
