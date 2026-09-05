package explicitlyinferred

import utest.*

object MethodMatcherTests extends TestSuite {
  import CompilerPluginTestSupport.expectIllegalArgument

  val tests = Tests {
    test("defaults to matching every method") {
      assert(new MethodMatcher.Builder().result().matches("anything"))
    }

    test("matches ordered regex stages") {
      val builder = new MethodMatcher.Builder
      builder.addRegex("keep.*")
      builder.addRegex(".*123")
      val matcher = builder.result()

      assert(matcher.matches("keep123"))
      assert(!matcher.matches("keepABC"))
      assert(!matcher.matches("skip123"))
    }

    test("feeds numbered and named rewrites into later stages") {
      val numbered = new MethodMatcher.Builder
      numbered.addRegex("prefix\\.(.*)")
      numbered.addRewrite("$1")
      numbered.addRegex("keep")

      val named = new MethodMatcher.Builder
      named.addRegex("prefix\\.(?<name>.*)")
      named.addRewrite("${name}")
      named.addRegex("keep")

      assert(numbered.result().matches("prefix.keep"))
      assert(named.result().matches("prefix.keep"))
    }

    test("applies a full-match rewrite once") {
      val builder = new MethodMatcher.Builder
      builder.addRegex("(.*)")
      builder.addRewrite("[$1]")
      builder.addRegex("\\[keep\\]")
      assert(builder.result().matches("keep"))
    }

    test("rejects structurally invalid rewrite pipelines") {
      expectIllegalArgument(new MethodMatcher.Builder().addRewrite("$1"))

      val noCapture = new MethodMatcher.Builder
      noCapture.addRegex("keep")
      expectIllegalArgument(noCapture.addRewrite("$1"))

      val duplicate = new MethodMatcher.Builder
      duplicate.addRegex("(keep)")
      duplicate.addRewrite("$1")
      expectIllegalArgument(duplicate.addRewrite("$1"))

      val trailing = new MethodMatcher.Builder
      trailing.addRegex("(keep)")
      trailing.addRewrite("$1")
      expectIllegalArgument(trailing.result())
    }

    test("validates numbered references eagerly and named references on match") {
      val numbered = new MethodMatcher.Builder
      numbered.addRegex("(keep)")
      expectIllegalArgument(numbered.addRewrite("$2"))

      val named = new MethodMatcher.Builder
      named.addRegex("prefix\\.(?<name>keep)")
      named.addRewrite("${missing}")
      named.addRegex("keep")
      val matcher = named.result()

      assert(!matcher.matches("skip"))
      expectIllegalArgument(matcher.matches("prefix.keep"))
    }
  }
}
