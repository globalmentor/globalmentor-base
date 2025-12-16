# GlobalMentor Base Libraries

Multi-module repository containing GlobalMentor's foundational Java libraries: core utilities, annotation processing support, CSV handling, and JVM profiling.

## Modules

| Module | Description |
|--------|-------------|
| [core](core/) | Foundational utilities: preconditions, byte sequences, URI/path manipulation, Optional/Stream extensions, naming convention conversion, and more. |
| [java-model](java-model/) | Annotation processor development support for `javax.lang.model`. |
| [text-csv](text-csv/) | RFC 4180-compliant CSV parsing and serialization. |
| [management](management/) | JVM management and stack profiling utilities. |

## Quick Orientation

**Most developers** will primarily use **globalmentor-core**, which provides:
- Precondition checks (`Conditions.checkArgument()`, `IO.check()`)
- Stream collectors for single-element extraction (`Streams.toOnly()`, `toFindOnly()`)
- `Optional` extensions (`Optionals.optionally()`, `fold()`)
- Naming convention conversion (`CompoundTokenization.CAMEL_CASE.to(KEBAB_CASE, ...)`)
- Type-safe regex groups (`RegularExpression.NumberedCapturingGroup`)
- Immutable byte sequences (`ByteSequence`)
- Cryptographic hashing helpers (`MessageDigests.SHA_256.hash()`)

**Annotation processor authors** should add **globalmentor-java-model** for `BaseAnnotationProcessor` and element/type utilities.

**CSV file handling** is provided by **globalmentor-text-csv** with RFC 4180 compliance.

**Performance profiling** is available via **globalmentor-management** for stack sampling analysis.

## Issues

Issues tracked by [JIRA](https://globalmentor.atlassian.net/projects/JAVA).

## Changelog

- v0.6.0: (2019-04-29) First release with base POM using GlobalMentor open-source root POM as parent.
