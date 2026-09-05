# explicitly-inferred

`explicitly-inferred` is a Scala 3 compiler plugin that writes inferred effect errors and results into managed Scaladoc regions during `-rewrite`.

## Installation

Compiler plugins are published for an exact Scala compiler version. This project currently supports Scala `3.9.0`:

```text
com.yoohaemin:explicitly-inferred_3.9.0:<plugin-version>
```

Use full-version cross publishing when adding the plugin to a build.

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

The plugin rewrites source files in place, so compilation must include `-rewrite`. Build tools supply the `-Xplugin` path for compiler-plugin dependencies. Raw compiler usage looks like:

```text
-Xplugin:/path/to/explicitly-inferred.jar
-rewrite
-P:explicitlyInferred:effectTypeRegex=zio\.prelude\.fx\.ZPure
```

## Usage

`effectTypeRegex` is required and matches the full name of the inferred effect constructor. The plugin reads error and result arguments by type-parameter name, then creates or updates a managed Scaladoc region.

```text
-P:explicitlyInferred:effectTypeRegex=zio\.prelude\.fx\.ZPure
-P:explicitlyInferred:errorTypeParam=E
-P:explicitlyInferred:resultTypeParam=A
-P:explicitlyInferred:typeNameStyle=owner
```

Before:

```scala
def create = null.asInstanceOf[ZPure[Any, Nothing, Foo | Bar, Unit]]
```

After:

```scala
/**
  * <!-- types -->
  * Errors:
  *   - Bar
  *   - Foo
  *
  * Returns:
  *   - Unit
  * <!-- /types -->
  */
def create = null.asInstanceOf[ZPure[Any, Nothing, Foo | Bar, Unit]]
```

Existing prose and Scaladoc tags outside the managed region are preserved. Union members are normalized, deduplicated, and sorted.

### Options

| Option | Default | Meaning |
| --- | --- | --- |
| `effectTypeRegex=<java-regex>` | required | Matches the full effect constructor name. |
| `methodRegex=<java-regex>` | `.*` | Matches the full simple method name. Repeatable as a pipeline. |
| `methodRegexRewrite=<java-replacement>` | none | Rewrites the preceding capturing regex match before the next method stage. |
| `scope=members\|all\|nonPrivate` | `members` | Selects members, all defs including locals, or non-private members. |
| `errorTypeParam=<name>` | `E` | Names the effect type parameter containing errors. |
| `resultTypeParam=<name>` | `A` | Names the effect type parameter containing the result. |
| `additionalErrorType=<display-name>` | none | Adds an error entry. Repeatable. |
| `excludeErrorTypeRegex=<java-regex>` | none | Removes matching inferred errors. Repeatable. |
| `typeNameStyle=simple\|owner\|full` | `simple` | Controls qualification of rendered type names. |
| `startMarker=<text>` | `types` | Sets the opening managed-region marker body. |
| `endMarker=<text>` | `/types` | Sets the closing managed-region marker body. |

Marker values are safe single-line HTML-comment bodies. The default values render as `<!-- types -->` and `<!-- /types -->`:

```text
-P:explicitlyInferred:startMarker=effect-types
-P:explicitlyInferred:endMarker=/effect-types
```

### Method Pipeline

`methodRegex` stages use Java regular expressions, match the entire current method name, and execute from left to right. An immediately following `methodRegexRewrite` uses Java replacement syntax and feeds its result into the next stage.

```text
-P:explicitlyInferred:methodRegex=prefix\.(?<name>.*)
-P:explicitlyInferred:methodRegexRewrite=${name}
-P:explicitlyInferred:methodRegex=create
```

Rules:

- A rewrite must immediately follow a regex containing at least one capture group.
- A rewrite must be followed by another regex stage.
- Numbered references such as `$1` are validated during option parsing.
- Named references such as `${name}` are validated when a matching stage applies them.

### Error Filtering

Additional public errors and excluded implementation errors can be configured independently:

```text
-P:explicitlyInferred:additionalErrorType=UnexpectedError
-P:explicitlyInferred:excludeErrorTypeRegex=.*ShortCircuit
```

Exclusion patterns are tested against both the rendered name and full type name.

### Behavior

- Only defs with inferred return types are considered.
- Synthetic defs are skipped.
- `scope=members` excludes local defs; `scope=all` includes them.
- Existing managed regions are replaced in place, making repeated rewrites idempotent.
- Attached block comments are converted to Scaladoc while preserving existing content.
- Comments are inserted above annotations, including multiline annotations.

If no documentation is written, verify that `-rewrite` is enabled, `effectTypeRegex` matches the full constructor name, E/A parameter names are correct, and the method passes the method and scope filters.

## Development

Run the Scala 3.9.0 test suite:

```bash
./mill 'plugin[3.9.0].test.testCached'
```

CI runs the suite on JDK `17`, `21`, `25`, and `26`.

Publish locally:

```bash
./mill 'plugin[3.9.0].publishLocal'
```

## Release

Pushing a `vX.Y.Z` tag publishes the Scala 3.9.0 artifact to Maven Central:

```bash
git tag v0.1.0
git push origin master --follow-tags
```

The equivalent Mill command is:

```bash
./mill mill.javalib.SonatypeCentralPublishModule/publishAll \
  --publishArtifacts 'plugin[3.9.0].publishArtifacts' \
  --bundleName "com.yoohaemin-explicitly-inferred-v0.1.0"
```

The release workflow uses these repository secrets:

- `MILL_PGP_PASSPHRASE`
- `MILL_PGP_SECRET_BASE64`
- `MILL_SONATYPE_USERNAME`
- `MILL_SONATYPE_PASSWORD`

Publishing also requires a verified `com.yoohaemin` Maven Central namespace and the corresponding public signing key.
