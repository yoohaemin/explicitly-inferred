# explicitly-inferred

`explicitly-inferred` is a Scala 3 compiler plugin that adds normalized inferred return-type comments during `-rewrite`.

## Usage

Add the plugin artifact to your build and pass it to the Scala 3 compiler. The published artifact coordinates are:

```text
com.yoohaemin::explicitly-inferred:<version>
```

Example compiler flags:

```text
-Xplugin:/path/to/explicitly-inferred.jar
-rewrite
-P:inferredReturnComment:methodRegex=.*
-P:inferredReturnComment:managedTag=@inferredReturnType
```

`methodRegex` is repeatable and evaluated left to right. Add `methodRegexRewrite` immediately after a capturing `methodRegex` to rewrite the matched name before the next stage.

```text
-P:inferredReturnComment:methodRegex=prefix\.(keep)
-P:inferredReturnComment:methodRegexRewrite=$1
-P:inferredReturnComment:methodRegex=keep
```

## Development

Run the test suite:

```bash
./mill test
```

Publish to the local Ivy repository:

```bash
./mill publishLocal
```

## Release

The release workflow publishes to Maven Central when a `vX.Y.Z` tag is pushed.

```bash
git tag v0.1.0
git push origin master --follow-tags
```

Before the first release, configure the GitHub repository secrets used by Mill:

- `MILL_PGP_PASSPHRASE`
- `MILL_PGP_SECRET_BASE64`
- `MILL_SONATYPE_USERNAME`
- `MILL_SONATYPE_PASSWORD`

You also need a verified `com.yoohaemin` namespace in Maven Central and a GPG key exported in the format Mill expects.
