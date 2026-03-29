# explicitly-inferred

`explicitly-inferred` is a Scala 3 compiler plugin that adds normalized inferred return-type comments during `-rewrite`.

## Usage

`explicitly-inferred` is a compiler plugin, so it is published once per exact Scala compiler version. The Maven coordinates look like:

```text
com.yoohaemin:explicitly-inferred_<scala-version>:<plugin-version>
```

For example, Scala `3.8.2` resolves:

```text
com.yoohaemin:explicitly-inferred_3.8.2:<plugin-version>
```

Supported Scala compiler versions are:

```text
3.5.0, 3.5.1, 3.5.2,
3.6.0, 3.6.1, 3.6.2, 3.6.3, 3.6.4,
3.7.0, 3.7.1, 3.7.2, 3.7.3, 3.7.4,
3.8.0, 3.8.1, 3.8.2
```

Use full-version cross publishing when you add it to your build.

Mill:

```scala
def scalacPluginIvyDeps = Agg(
  ivy"com.yoohaemin:::explicitly-inferred:<plugin-version>"
)
```

sbt:

```scala
addCompilerPlugin(
  "com.yoohaemin" %% "explicitly-inferred" % "<plugin-version>" cross CrossVersion.full
)
```

The plugin rewrites source files in place, so you must compile with `-rewrite`. This project itself is built with `-no-indent` and `-old-syntax`. If you wire the plugin through a build tool as a compiler plugin dependency, the build tool provides the `-Xplugin` path and you only need to add the syntax flags you want, the plugin options, and `-rewrite`.

The raw compiler flags look like this:

```text
-no-indent
-old-syntax
-Xplugin:/path/to/explicitly-inferred.jar
-rewrite
```

When a `def` has no explicit return type and matches the configured filters, the plugin inserts or updates a managed block comment immediately above it.

Before:

```scala
object Sample {
  def value = 1
}
```

After:

```scala
object Sample {
  /*
   * @inferredReturnType Int
   */
  def value = 1
}
```

### Basic Flags

Start with the defaults:

```text
-P:inferredReturnComment:methodRegex=.*
-P:inferredReturnComment:managedTag=@inferredReturnType
```

That is usually enough to rewrite inferred member defs with the default managed tag.

### Option Reference

| Option | Default | Meaning |
| --- | --- | --- |
| `methodRegex=<java-regex>` | `.*` | Matches the full simple method name. Repeat it to build a left-to-right name-matching pipeline. |
| `methodRegexRewrite=<java-replacement>` | none | Rewrites the name matched by the immediately preceding capturing `methodRegex` before the next regex stage runs. |
| `scope=members\|all\|nonPrivate` | `members` | Controls which defs are eligible: class/object members only, all defs including locals, or non-private members only. |
| `maxTypeLength=<positive-int>` | `80` | Keeps the managed entry on one line when it fits; otherwise emits a managed multiline block. |
| `managedTag=<single-line-text>` | `@inferredReturnType` | Changes the marker used for managed entries. |
| `showTypeArgs=true\|false` | `true` | Shows or suppresses type arguments in rendered types. |
| `showTypeParamNames=true\|false` | `true` | Shows type parameter labels such as `A = Int` instead of positional type arguments. |

### Regex Pipeline

`methodRegex` uses Java regex and matches the full simple def name, not a substring. If you want substring-like behavior, write that explicitly in the regex, for example `.*keep.*`.

`methodRegex` is repeatable and evaluated left to right. Each stage sees the current name. If a stage has a matching `methodRegexRewrite`, the rewritten name becomes the input to the next `methodRegex`.

Rules:

- `methodRegexRewrite` must immediately follow a capturing `methodRegex`.
- A rewrite is invalid after a regex with no capture groups.
- A trailing rewrite with no next `methodRegex` stage is invalid.
- Rewrites use Java replacement syntax, so both numbered (`$1`) and named (`${name}`) groups work.
- Rewrites may also be literal text.

Simple prefix stripping:

```text
-P:inferredReturnComment:methodRegex=prefix\.(keep)
-P:inferredReturnComment:methodRegexRewrite=$1
-P:inferredReturnComment:methodRegex=keep
```

Named capture groups:

```text
-P:inferredReturnComment:methodRegex=prefix\.(?<name>keep)
-P:inferredReturnComment:methodRegexRewrite=${name}
-P:inferredReturnComment:methodRegex=keep
```

Multiple rewrite stages:

```text
-P:inferredReturnComment:methodRegex=prefix\.(.*)
-P:inferredReturnComment:methodRegexRewrite=$1
-P:inferredReturnComment:methodRegex=(.*)\.(.*)
-P:inferredReturnComment:methodRegexRewrite=$2
-P:inferredReturnComment:methodRegex=keep
```

Literal rewrites are also valid:

```text
-P:inferredReturnComment:methodRegex=prefix\.(keep)
-P:inferredReturnComment:methodRegexRewrite=renamed
-P:inferredReturnComment:methodRegex=renamed
```

### Common Configurations

Rewrite only methods named `keep`:

```text
-P:inferredReturnComment:methodRegex=keep
```

Include local defs as well as members:

```text
-P:inferredReturnComment:scope=all
```

Skip private members while keeping protected ones:

```text
-P:inferredReturnComment:scope=nonPrivate
```

Use a custom managed tag:

```text
-P:inferredReturnComment:managedTag=@explicitlyInferred
```

Force multiline output earlier:

```text
-P:inferredReturnComment:maxTypeLength=20
```

Hide type parameter labels:

```text
-P:inferredReturnComment:showTypeParamNames=false
```

Hide type arguments entirely:

```text
-P:inferredReturnComment:showTypeArgs=false
```

### Behavior Notes

- The plugin only touches defs with inferred return types. If a def already has `: Type`, it is left alone.
- Managed comments are updated in place, so rerunning with the same settings is idempotent.
- The plugin skips synthetic defs.
- With the default `scope=members`, local defs are not rewritten.
- If `managedTag` is customized, only entries with that tag are treated as managed on subsequent rewrites.

### Troubleshooting

If nothing changes, check these first:

- `-rewrite` is present.
- The def has no explicit return type.
- The def name matches the full `methodRegex` pipeline.
- The current `scope` includes that def.
- A `methodRegexRewrite` is attached only to a capturing regex and is followed by another `methodRegex`.
- Replacement references such as `$2` or `${missing}` are valid for the preceding regex.

## Development

Run the test suite:

```bash
./mill 'plugin[3.8.2].test.testCached'
```

Run the test suite across every supported Scala compiler version:

```bash
./mill 'plugin[__].test.testCached'
```

Publish to the local Ivy repository:

```bash
./mill 'plugin[__].publishLocal'
```

## Release

The release workflow publishes every supported compiler-plugin artifact to Maven Central when a `vX.Y.Z` tag is pushed. It uses Mill's `SonatypeCentralPublishModule/publishAll` entrypoint with `plugin[__].publishArtifacts`, so all Scala-version variants are signed and uploaded as one Central bundle.

```bash
git tag v0.1.0
git push origin master --follow-tags
```

The equivalent manual release command is:

```bash
./mill mill.javalib.SonatypeCentralPublishModule/publishAll \
  --publishArtifacts 'plugin[__].publishArtifacts' \
  --bundleName "com.yoohaemin-explicitly-inferred-v0.1.0"
```

Use a new version when releasing the corrected compiler-plugin coordinates. The already-published `0.1.0-M1` artifact uses library-style `_3` coordinates and should be treated as superseded rather than reused.

Before the first release, configure the GitHub repository secrets used by Mill:

- `MILL_PGP_PASSPHRASE`
- `MILL_PGP_SECRET_BASE64`
- `MILL_SONATYPE_USERNAME`
- `MILL_SONATYPE_PASSWORD`

You also need a verified `com.yoohaemin` namespace in Maven Central and a GPG key exported in the format Mill expects.
