# explicitly-inferred

`explicitly-inferred` is a Scala 3 compiler plugin that writes selected inferred type arguments into managed Scaladoc regions during `-rewrite`.

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
-P:explicitlyInferred:typeParam=L:Left:dealias
-P:explicitlyInferred:typeParam=R:Right:preserve
```

## Usage

Each `typeParam` selects a formal type-parameter name, assigns its Scaladoc heading, and chooses whether aliases are preserved or expanded. Mappings are repeatable and rendered in option order.

```text
-P:explicitlyInferred:typeParam=L:Left:dealias
-P:explicitlyInferred:typeParam=R:Right:preserve
-P:explicitlyInferred:typeRegex=example\.Container
-P:explicitlyInferred:typeNameStyle=owner
```

Before:

```scala
def create = null.asInstanceOf[Container[Any, Foo | Bar, Unit]]
```

After:

```scala
/**
  * <!-- types -->
  * Left:
  *   - Bar
  *   - Foo
  *
  * Right:
  *   - Unit
  * <!-- /types -->
  */
def create = null.asInstanceOf[Container[Any, Foo | Bar, Unit]]
```

Existing prose and Scaladoc tags outside the managed region are preserved. Union members are normalized, deduplicated, and sorted.

### Options

| Option | Default | Meaning |
| --- | --- | --- |
| `typeParam=<name>:<heading>:preserve\|dealias` | required | Selects and labels a formal type parameter. Repeatable. |
| `typeRegex=<java-regex>` | none | Restricts the full name of the outer parameterized type. |
| `additionalType=<name>:<display-name>` | none | Adds an entry to the named parameter's section. Repeatable. |
| `excludeTypeRegex=<name>:<java-regex>` | none | Removes matching entries from the named parameter's section. Repeatable. |
| `methodRegex=<java-regex>` | `.*` | Matches the full simple method name. Repeatable as a pipeline. |
| `methodRegexRewrite=<java-replacement>` | none | Rewrites the preceding capturing regex match before the next method stage. |
| `scope=members\|all\|nonPrivate` | `members` | Selects members, all defs including locals, or non-private members. |
| `typeNameStyle=simple\|owner\|full` | `simple` | Controls qualification of rendered type names. |
| `startMarker=<text>` | `types` | Sets the opening managed-region marker body. |
| `endMarker=<text>` | `/types` | Sets the closing managed-region marker body. |

Marker values are safe single-line HTML-comment bodies. The default values render as `<!-- types -->` and `<!-- /types -->`:

```text
-P:explicitlyInferred:startMarker=inferred-types
-P:explicitlyInferred:endMarker=/inferred-types
```

### Parameter Mappings

The compact mapping syntax is `<formal-name>:<heading>:<alias-policy>`. The plugin appends `:` to the heading in Scaladoc. `preserve` keeps source-facing aliases, while `dealias` expands aliases before rendering union members.

Without `typeRegex`, any outer parameterized inferred return type is eligible. A candidate is skipped unless its constructor contains every configured formal parameter. Nested constructors are not searched.

Mappings and their headings must be unique. Additional values and exclusion patterns target a configured formal parameter and may appear before or after its mapping:

```text
-P:explicitlyInferred:additionalType=L:Fallback
-P:explicitlyInferred:excludeTypeRegex=L:.*Internal
```

Exclusion patterns are tested against both the rendered name and full type name. Top-level `Nothing` is treated as an empty inferred set and appears only when a section has no inferred or additional entries.

Versions before `0.1.0-M8` used effect-specific options. Replace them as follows; the old names are rejected:

| Before M8 | M8 and later |
| --- | --- |
| `effectTypeRegex=<regex>` | `typeRegex=<regex>` |
| `errorTypeParam=E` | `typeParam=E:Errors:dealias` |
| `resultTypeParam=A` | `typeParam=A:Returns:preserve` |
| `additionalErrorType=<value>` | `additionalType=E:<value>` |
| `excludeErrorTypeRegex=<regex>` | `excludeTypeRegex=E:<regex>` |

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

### Behavior

- Only defs with inferred return types are considered.
- Only the outer inferred return-type constructor is inspected.
- Every configured type-parameter mapping must resolve on that constructor.
- Synthetic defs are skipped.
- `scope=members` excludes local defs; `scope=all` includes them.
- Existing managed regions are replaced in place, making repeated rewrites idempotent.
- Attached block comments are converted to Scaladoc while preserving existing content.
- Comments are inserted above annotations, including multiline annotations.

If no documentation is written, verify that `-rewrite` is enabled, every `typeParam` name exists on the outer constructor, any `typeRegex` matches its full name, and the method passes the method and scope filters.

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

### Performance Benchmark

The manual benchmark compares median Scala compiler process time with and without the plugin. Run the full matrix with two warmups and seven measured iterations per mode:

```bash
./mill benchmark.run
```

The scenarios isolate different plugin costs:

- `many-methods` processes 1,000 inferred parameterized methods.
- `many-comments` adds an existing Scaladoc comment to every method.
- `large-union` renders a balanced 256-member union for 100 methods.
- `early-mismatch` rejects 5,000 methods at the first stage of an eight-stage method pipeline.

For a quicker targeted run, select one scenario and reduce the iteration counts:

```bash
./mill benchmark.run --warmups 1 --iterations 3 --scenario large-union
```

Compare `overhead_ms` (`plugin_ms - baseline_ms`) between revisions on the same idle machine, JDK, and Scala version. The baseline column includes compiler startup and normal compilation, while the ratio reports total plugin time divided by total baseline time.

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
