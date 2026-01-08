# GlobalMentor Core Java Library

Foundational utilities for Java development, including precondition checks, immutable byte sequences, cryptographic hashing helpers, URI/path manipulation, and text processing.

## Quick Reference

**Argument validation:** `com.globalmentor.java.Conditions` — `checkArgument()`, `checkState()` throw `IllegalArgumentException`/`IllegalStateException` with formatted messages; `checkIndexBounds()` for array/list index validation.

**I/O validation:** `com.globalmentor.io.IO` — `check()` throws `IOException` with formatted messages for I/O preconditions.

**Immutable byte data:** `com.globalmentor.java.ByteSequence` — value-semantics byte sequence analogous to `CharSequence`; supports `startsWith()`, `toHexString()`, `toBase64String()`. Factory: `ByteSequence.copyOf()` or `Bytes.asByteSequence()` for non-copying view.

**Cryptographic hashing:** `com.globalmentor.security.MessageDigests` — algorithm constants (`SHA_256`, `MD5`) with fluent `hash()` and `checksum()` methods returning immutable `Hash` values.

**Single-element streams:** `com.globalmentor.util.stream.Streams` — `toOnly()` collector requires exactly one element; `toFindOnly()` reducer returns `Optional` but throws if multiple; `toFindOnlyOrElse()` for graceful handling without exceptions.

**Optional extensions:** `com.globalmentor.util.Optionals` — `optionally(condition)` converts boolean to `Optional`; `filterAsInstance()` filters and casts; `fold()` combines two optionals; `isPresentAndEquals()` for present-and-match check.

**Naming convention conversion:** `com.globalmentor.lex.CompoundTokenization` — convert between `CAMEL_CASE`, `KEBAB_CASE`, `SNAKE_CASE`, `PASCAL_CASE`, `CONSTANT_CASE`. E.g., `CAMEL_CASE.to(KEBAB_CASE, "fooBar")` → `"foo-bar"`.

**Type-safe regex groups:** `com.globalmentor.text.RegularExpression.NumberedCapturingGroup` — define capturing groups as enum values; extract with `group.findIn(matcher)` instead of magic numbers. See `EmailAddress.EmailAddressRegEx` for example.

**Filename manipulation:** `com.globalmentor.io.Filenames` — multi-extension parsing via `extensions()` iterator; `getBase()`/`changeBase()` for base filename; `addExtension()`/`changeExtension()`; `encodeCrossPlatformFilename()` for reserved-character escaping.

**URI/path operations:** `com.globalmentor.net.URIs` — RFC 3986 URI manipulation including `resolve()`, `findPathExtension()`, `isCollectionURI()`. `com.globalmentor.io.Paths` — NIO path utilities including `changeBase()`, `isSubPath()`.

**Media types:** `com.globalmentor.net.MediaType` — RFC 6838-compliant MIME type parsing with `findCharset()`, `hasBaseType()`.

**Type-safe value wrappers:** `com.globalmentor.model.Newtype` — marker interface for the "newtype" idiom; wrap types like `UUID` in domain-specific records (e.g., `UserId`, `WidgetRef`) for compile-time type safety while enabling transparent serialization.

**Character sets:** `com.globalmentor.java.Characters` — immutable character sets with Unicode/ASCII constants (`DIGIT_CHARACTERS`, `ALPHA_CHARACTERS`); `com.globalmentor.text.ABNF` for RFC 2234 character classes.

**Functional I/O:** `com.globalmentor.io.function` — `IOFunction`, `IOConsumer`, `IOSupplier` etc. that declare `IOException`, enabling functional patterns with I/O operations.

## Overview

### `com.globalmentor.java`

Core Java language utilities and extensions.

#### Precondition Checks

`Conditions` provides expressive precondition and state validation methods that throw appropriate exceptions with formatted messages. All methods support `String.format()`-style message formatting.

**Boolean condition checks:**

```java
import static com.globalmentor.java.Conditions.*;

checkArgument(count >= 0, "Count `%d` cannot be negative.", count);
checkState(isInitialized(), "Service not initialized.");
```

**Type and null checks:**

```java
String typed = checkArgumentIsInstance(obj, String.class); // returns cast value
String notNull = checkArgumentNotNull(value, "Value `%s` required.", name);
```

**String validation:**

```java
String notEmpty = checkArgumentNotEmpty(input); // throws if ""
String notBlank = checkArgumentNotBlank(input); // throws if whitespace-only
```

**Numeric range checks:**

```java
int positive = checkArgumentPositive(count);
int nonNegative = checkArgumentNotNegative(index);
int bounded = checkArgumentRange(value, 0, 100);
checkIndexBounds(index, array.length);
```

**Optional presence:**

```java
T value = checkArgumentPresent(optional, "Expected value for `%s`.", key);
```

#### Byte Sequences

`ByteSequence` is an immutable, value-semantics interface for byte data—analogous to `CharSequence` for characters. Use `Bytes.asByteSequence()` for a non-copying view or `ByteSequence.copyOf()` for defensive copies.

```java
ByteSequence bytes = ByteSequence.copyOf(data);
bytes.startsWith(prefix);
bytes.toByteBuffer();    // read-only view
bytes.toHexString();     // "0a1b2c..."
bytes.toBase64String();  // "Chs..."
```

#### Character Utilities

`Characters` is an immutable set of characters with searching and matching capabilities. It includes comprehensive Unicode and ASCII character constants.

```java
import static com.globalmentor.java.Characters.*;

Characters digits = Characters.ofRange('0', '9');
boolean isDigit = DIGIT_CHARACTERS.contains(c);
```

`CharSequences` provides text manipulation for any `CharSequence`, and `StringBuilders` offers efficient mutable string operations.

#### Primitive Wrappers

Utility classes `Bytes`, `Integers`, `Longs`, `Doubles`, and `Booleans` extend JDK primitive wrapper functionality with additional conversion, parsing, and byte-array operations.

```java
long value = Longs.fromBytes(bytes, offset);
byte[] bytes = Integers.toBytes(intValue);
```

#### Classes and Objects

`Classes` provides reflection utilities including type hierarchy traversal, resource loading, and method introspection. `Objects` extends `java.util.Objects` with instance checking and cloning support.

```java
Optional<String> found = Objects.asInstance(obj, String.class);
Stream<Class<?>> hierarchy = Classes.getProperAncestorClasses(MyClass.class);
```

#### Enums

`Enums` provides serialization-friendly naming conversions and safe parsing with `Optional` returns.

```java
Optional<MyEnum> found = Enums.asEnum(MyEnum.class, "VALUE");
String serialized = Enums.getSerializationName(MyEnum.FILE_NOT_FOUND); // "file-not-found"
```

### `com.globalmentor.collections`

Utilities for the Java collections framework (`java.util`).

#### Collection Utilities

`Collections`, `Lists`, `Maps`, and `Sets` extend JDK collection utilities with additional operations for adding/removing from iterables and creating reversed views.

```java
import static com.globalmentor.collections.Lists.*;

Iterable<E> reversed = reversing(list);
```

#### Iterables

The `iterables` subpackage provides `Iterables` utilities including `findFirst()` and `findOnly()` for safe element access.

```java
Optional<T> first = Iterables.findFirst(iterable);
Optional<T> only = Iterables.findOnly(iterable); // throws if more than one
```

#### Iterators

The `iterators` subpackage provides specialized iterators including `JoinIterator` for concatenating iterators, `FilterIterator` for predicate-based filtering, and `ReverseIterator` for backward list traversal.

```java
Iterator<E> joined = new JoinIterator<>(iterator1, iterator2);
```

#### Comparators

The `comparators` subpackage includes `ExplicitOrderComparator` for custom ordering and `SortOrder` for ascending/descending sort control.

#### Thread-Safe Collections (legacy)

Decorator classes like `DecoratorReadWriteLockMap` and `SynchronizedCollectionDecorator` provide thread-safe collection wrappers. `ReverseMap` maintains bidirectional key-value mappings.

#### Suffix Trees

`CharSequenceSuffixTree` implements an efficient suffix tree data structure for substring operations.

### `com.globalmentor.io`

I/O utilities for `java.io` and `java.nio`.

#### Precondition Checks

`IO.check()` validates I/O conditions, throwing `IOException` on failure.

```java
import static com.globalmentor.io.IO.*;

check(file.exists(), "File `%s` not found.", file);
```

#### Path and File Utilities

`Paths` provides NIO path manipulation including base changing, subpath validation, and filename comparisons. `Files` extends `java.nio.file.Files` with backup operations, extension handling, and file attribute constants.

```java
Path newPath = Paths.changeBase(path, oldBase, newBase);
boolean isSub = Paths.isSubPath(basePath, subPath);
```

#### Filename Utilities

`Filenames` provides comprehensive filename manipulation including multi-extension parsing, base filename operations, dotfile detection, cross-platform encoding, and specialized comparators.

**Multi-extension iteration:** Iterate over compound extensions from most-specific to least-specific:

```java
Filenames.extensions("archive.tar.gz").forEach(ext -> {
    // First: "tar.gz", then: "gz"
    Optional<MediaType> type = MediaType.findForFilenameExtension(ext);
});
```

**Base filename operations:** Extract, modify, or append to the base filename (portion before all extensions):

```java
String base = Filenames.getBase("document.backup.pdf"); // "document"
String localized = Filenames.appendBase("config.json", "_fr"); // "config_fr.json"
String renamed = Filenames.changeBase("old.tar.gz", "new"); // "new.tar.gz"
```

**Extension operations:**

```java
Optional<String> ext = Filenames.findExtension("data.csv"); // Optional.of("csv")
String changed = Filenames.changeExtension("image.png", "jpg"); // "image.jpg"
String added = Filenames.addExtension("backup", "tar.gz"); // "backup.tar.gz"
boolean isPdf = Filenames.hasExtension("Doc.PDF", "pdf"); // true (case-insensitive)
```

**Extension normalization and comparison:** The nested `Extensions` class handles case-insensitive extension comparison:

```java
String normalized = Filenames.Extensions.normalize("TXT"); // "txt"
boolean same = Filenames.Extensions.equals("JPG", "jpg"); // true
```

**Dotfile detection:**

```java
boolean hidden = Filenames.isDotfileFilename(".gitignore"); // true
boolean notDotfile = Filenames.isDotfileFilename("."); // false (special directory)
```

**Cross-platform filename encoding:** Escape reserved characters for safe filenames across operating systems:

```java
String safe = Filenames.encodeCrossPlatformFilename("file:name?.txt"); // "file^3Aname^3F.txt"
String original = Filenames.decodeFilename(safe); // "file:name?.txt"
boolean valid = Filenames.isCrossPlatformFilename("document.pdf"); // true
```

**Filename comparators:** Sort filenames by base name or extension:

```java
List<String> sorted = filenames.stream()
    .sorted(Filenames.comparator(Locale.getDefault()))
    .toList();
```

#### Stream Utilities

`InputStreams` and `OutputStreams` provide stream reading/writing utilities. `InputStreams.readByteSequence()` reads stream content into an immutable `ByteSequence`.

```java
ByteSequence content = InputStreams.readByteSequence(inputStream);
```

#### Byte Order Mark

`ByteOrderMark` detects and handles Unicode BOMs for UTF-8, UTF-16, and UTF-32 encodings.

```java
Optional<ByteOrderMark> bom = ByteOrderMark.detect(bytes);
Charset charset = bom.map(ByteOrderMark::getCharset).orElse(UTF_8);
```

#### Functional Interfaces for I/O

The `function` subpackage provides functional interface equivalents (`IOFunction`, `IOConsumer`, `IOSupplier`, etc.) that declare `IOException`, enabling functional programming patterns with I/O operations.

```java
IOFunction<Path, String> reader = path -> Files.readString(path);
```

#### Parsing (legacy)

`ParseReader` and `ReaderParser` provide character-by-character parsing with lookahead capabilities.

### `com.globalmentor.net`

Network types and utilities for `java.net`.

#### URI Utilities

`URIs` provides comprehensive URI manipulation following RFC 3986. The class is extensive (2000+ lines); key operations are categorized below.

**Scheme operations:**

```java
import static com.globalmentor.net.URIs.*;

Optional<String> scheme = findScheme(uri);
boolean isFile = hasScheme(uri, FILE_SCHEME); // case-insensitive per RFC 3986
URI checked = checkArgumentScheme(uri, "https"); // precondition check
URI changed = changeScheme(uri, "https");
```

**Path operations:**

```java
String rawPath = getRawPath(uri); // encoded path
String name = getRawName(uri); // filename portion
Optional<String> ext = findPathExtension(uri);
boolean isCollection = isCollectionURI(uri); // ends with '/'
URI parent = getParentURI(uri);
URI child = resolve(baseUri, relativePath);
URI changed = changeRawPath(uri, "/new/path");
```

**Query operations:**

```java
Optional<String> query = findRawQuery(uri);
List<URIQueryParameter> params = getQueryParameters(uri);
URI withQuery = appendQueryParameter(uri, "key", "value");
URI noQuery = removeQuery(uri);
```

**Fragment operations:**

```java
Optional<String> fragment = findRawFragment(uri);
URI withFragment = changeRawFragment(uri, "section");
URI noFragment = removeFragment(uri);
```

**URI construction:**

```java
URI built = createURI(scheme, host, path); // multiple overloads available
```

#### URI Path

`URIPath` represents hierarchical URI paths with encoding requirements, supporting path resolution and manipulation.

```java
URIPath path = URIPath.of("foo/bar/");
URIPath resolved = path.resolve("baz.txt");
```

#### Media Types

`MediaType` provides RFC 6838-compliant Internet media type handling with parameter support, charset detection, and proper equality semantics.

```java
MediaType type = MediaType.parse("text/html; charset=utf-8");
Optional<Charset> charset = type.findCharset();
boolean matches = type.hasBaseType("text", "html");
```

#### Value Classes

- `EmailAddress` — RFC 5322-compliant email address value class with local-part and domain access.
- `DomainName` — DNS domain name representation per RFC 1035.
- `Host` — Host/port combination for network addresses.

```java
EmailAddress email = EmailAddress.fromString("user@example.com");
String domain = email.getDomain();
```

#### HTTP Utilities

`HTTP` provides HTTP protocol constants (methods, headers, status codes) per RFC 9110. `http.HttpResponses` provides response handling utilities.

### `com.globalmentor.security`

Cryptographic utilities for `java.security`.

#### Message Digests and Hashes

`MessageDigests` simplifies computing cryptographic hashes with algorithm constants (`MD5`, `SHA_256`, etc.) and fluent methods. `Hash` encapsulates digest output as an immutable `ByteSequence`.

```java
import static com.globalmentor.security.MessageDigests.*;

Hash hash = SHA_256.hash("content");
String checksum = SHA_256.checksum(path);
```

#### Nonces

`Nonce` and `DefaultNonce` provide cryptographically random single-use tokens.

### `com.globalmentor.text`

Text processing utilities.

#### ABNF Constants

`ABNF` defines character sets from RFC 2234 (Augmented BNF), including `ALPHA_CHARACTERS`, `DIGIT_CHARACTERS`, and control characters.

#### Regular Expressions

`RegularExpression` provides pattern building utilities and an innovative type-safe capturing group facility that eliminates the error-prone practice of using magic numbers for regex group access.

**Type-safe capturing groups with `NumberedCapturingGroup`:** Define an enum implementing `NumberedCapturingGroup` to create self-documenting, refactoring-safe regex group access:

```java
// Define regex groups as an enum—pattern and groups are co-located
enum UserIdRegEx implements RegularExpression.NumberedCapturingGroup {
    PREFIX_GROUP, NUMBER_GROUP, SUFFIX_GROUP;

    public static final Pattern PATTERN = Pattern.compile("([A-Z]+)(\\d+)([a-z]*)");
}

// Usage: type-safe group extraction
Matcher matcher = UserIdRegEx.PATTERN.matcher("USR12345abc");
if (matcher.matches()) {
    String prefix = UserIdRegEx.PREFIX_GROUP.findIn(matcher).orElseThrow(); // "USR"
    String number = UserIdRegEx.NUMBER_GROUP.findIn(matcher).orElseThrow(); // "12345"
    Optional<String> suffix = UserIdRegEx.SUFFIX_GROUP.findIn(matcher); // Optional.of("abc")
}
```

This pattern is used throughout the library—see `EmailAddress.EmailAddressRegEx` for a real-world example that extracts local-part and domain from RFC 5322 email addresses.

**Precondition matching:**

```java
// Validate and return matcher in one step
Matcher matcher = RegularExpression.checkArgumentMatches(input, pattern, "Invalid format: `%s`", input);
```

**Pattern building utilities:** Create character classes and escape special characters:

```java
String digitClass = RegularExpression.characterClassOf('0', '1', '2', '3'); // "[0123]"
String notVowels = RegularExpression.characterClassNotOf(Characters.of("aeiou")); // "[^aeiou]"
String escaped = RegularExpression.escapePatternString("file.txt"); // "file\\.txt"
```

**Convenient matching:**

```java
Optional<Matcher> found = RegularExpression.findMatch(pattern, input);
found.ifPresent(m -> process(m.group(1)));
```

#### Syntax Exceptions

`ArgumentSyntaxException` signals parsing failures with optional input position information.

#### Case Conversion

`Case` provides case transformation utilities for strings.

#### Text Formatting (legacy)

`TextFormatter`, `W3CDateFormat`, and `RomanNumerals` provide specialized formatting.

### `com.globalmentor.lex`

Lexical analysis and compound token handling.

#### Compound Tokenization

`CompoundTokenization` provides a powerful generalization for splitting and joining compound tokens across different naming conventions (e.g., `fooBar`, `foo-bar`, `FOO_BAR`). This is invaluable for code generation, configuration mapping, and API transformations.

**Predefined tokenizations:**

| Constant | Example | Notes |
|----------|---------|-------|
| `CAMEL_CASE` | `fooBarBaz` | General camelCase (preserves first segment case) |
| `DROMEDARY_CASE` | `fooBarBaz` | Lowercase first segment |
| `PASCAL_CASE` | `FooBarBaz` | Uppercase first letter of each segment |
| `KEBAB_CASE` | `foo-bar-baz` | Hyphen-delimited, lowercase |
| `SNAKE_CASE` | `foo_bar_baz` | Underscore-delimited, lowercase |
| `CONSTANT_CASE` | `FOO_BAR_BAZ` | Underscore-delimited, uppercase (for constants) |
| `DOT_CASE` | `foo.bar.baz` | Dot-delimited, case-preserving |

**Splitting and joining:**

```java
import static com.globalmentor.lex.CompoundTokenization.*;

List<String> segments = CAMEL_CASE.split("fooBarBaz"); // ["foo", "Bar", "Baz"]
String joined = KEBAB_CASE.join(segments); // "foo-bar-baz"
```

**Direct conversion between tokenizations:**

```java
// Convert Java field name to JSON property name
String jsonKey = CAMEL_CASE.to(SNAKE_CASE, "createdAt"); // "created_at"

// Convert environment variable to Java constant
String constant = SNAKE_CASE.to(CONSTANT_CASE, "max_connections"); // "MAX_CONNECTIONS"

// Convenience methods for common conversions
String kebab = CAMEL_CASE.toKebabCase("backgroundColor"); // "background-color"
String snake = PASCAL_CASE.toSnakeCase("UserAccount"); // "user_account"
```

**Custom tokenizations:** Create domain-specific tokenizations with custom delimiters or transformations:

```java
// Colon-delimited with uppercase transformation
CompoundTokenization NAMESPACE_CASE = CompoundTokenization
    .namedDelimitedByWithSegmentTransformation("namespace::case", ':', TRANSFORM_TO_UPPERCASE);

String namespace = KEBAB_CASE.to(NAMESPACE_CASE, "my-module-name"); // "MY::MODULE::NAME"
```

**CamelCase implementation:** The `CamelCase` class handles the complexity of splitting camelCase tokens by detecting case boundaries while properly handling sequences of uppercase letters (acronyms work best when formatted like `oldUrlMapper` rather than `oldURLMapper`).

### `com.globalmentor.model`

Domain model utilities and common patterns.

#### Newtype

`Newtype<V>` is a marker interface for the "newtype" idiom—a type-safe wrapper around a single value. It originated in Haskell and was popularized by Rust. Use it when multiple domain concepts share the same underlying type (e.g., `UUID`) but should not be interchangeable.

```java
public record UserId(UUID value) implements Newtype<UUID> {
    public UserId { requireNonNull(value); }
}

public record WidgetRef(UUID value) implements Newtype<UUID> {
    public WidgetRef { requireNonNull(value); }
}
```

Now a method expecting `UserId` won't accept `WidgetRef`, even though both wrap `UUID`. Serialization frameworks can recognize `Newtype` and serialize as the bare value rather than a wrapper object.

**Construction precedence:** Frameworks should instantiate newtypes using (1) a `public static of(V)` factory method if present, or (2) the single-argument constructor.

**Constraints:** The wrapped type `V` must not itself implement `Newtype`. Null values should be rejected in the constructor.

#### Locales and Language Tags

`Locales` manipulates `java.util.Locale` objects. `LanguageTags` handles RFC 5646 language tag parsing and formatting.

```java
Optional<String> lang = Locales.findLanguage(locale);
Locale primaryLang = Locales.toPrimaryLanguageLocale(locale);
```

#### UUIDs

`UUIDs` provides UUID byte-array conversion and URN formatting.

```java
UUID uuid = UUIDs.fromBytes(bytes);
byte[] bytes = UUIDs.toBytes(uuid);
```

#### Common Interfaces

- `Named<N>` — Objects with a name.
- `IDed<I>` — Objects with an identifier.
- `Labeled` — Objects with a human-readable label.
- `Modifiable` — Objects tracking modification state.
- `Range<T>` — Comparable value ranges with containment checking.

### `com.globalmentor.function`

Functional programming utilities for `java.util.function`.

#### Higher-Order Functions

`Functions` provides function adapters including counting consumers for stream processing.

```java
import static com.globalmentor.function.Functions.*;

stream.forEach(countingConsumer((item, count) -> process(item, count)));
```

#### Lazy Suppliers

`LazySupplier` defers computation until first access with thread-safe initialization.

### `com.globalmentor.util`

Utilities for `java.util`.

#### Optionals

`Optionals` extends `Optional` with filtering, folding, and conditional operations that complement Java's built-in `Optional` methods.

**Conditional-to-Optional conversion:** Convert boolean conditions to `Optional` for fluent, functional conditional logic:

```java
import static com.globalmentor.util.Optionals.*;

// Functional conditional—like JavaScript "truthy" coercion to Optional
Optional<Path> foundLogFile = optionally(loggingEnabled).map(_ -> logDirectory.resolve("app.log"));
```

**Type-safe instance filtering:** Filter and cast optional values in one operation:

```java
Optional<String> typed = filterAsInstance(optional, String.class);
// equivalent to: optional.filter(String.class::isInstance).map(String.class::cast)
```

**Higher-order fold operation:** Combine two optionals with a combiner function, or return whichever is present:

```java
Optional<BigDecimal> total = fold(optionalPrice1, optionalPrice2, BigDecimal::add);
// Returns combined value if both present; otherwise returns whichever is present
```

**Optional chaining:** Chain optional values without requiring a supplier:

```java
Optional<Config> config = or(primaryConfig, fallbackConfig);
// Similar to Optional.or() but works with already-computed optionals
```

**Present-and-equals check:**

```java
if (isPresentAndEquals(optional, expectedValue)) { /* matched */ }
```

#### Streams

`stream.Streams` provides stream collectors and reduction operators for single-element constraints, stream zipping, and controlled handling of multiple-element scenarios.

**Single-element extraction:** Require exactly one element or at most one element:

```java
import static com.globalmentor.util.stream.Streams.*;

// Require exactly one element (throws if zero or more than one)
String only = stream.collect(toOnly());

// Find at most one element (throws if more than one; empty if none)
Optional<String> found = stream.reduce(toFindOnly());

// Custom exception for multiple elements
Optional<User> user = stream.reduce(toFindOnly(() -> new DuplicateUserException("Multiple users found")));
```

**Graceful handling of multiple elements:** For annotation processing and similar scenarios where exceptions are not desired:

```java
// Perform action (e.g., log warning) if multiple found; return empty Optional
Optional<Config> config = stream.collect(toFindOnlyOrElse(() -> messager.printWarning("Multiple configs")));

// Find any element but notify if there were multiple
Optional<Handler> handler = stream.reduce(toFindAnyWhenMany(() -> log.warn("Ambiguous handler")));
```

**Stream zipping:** Combine two streams element-by-element (like Python's `zip()`):

```java
Stream<Pair<String, Integer>> zipped = zip(names, scores, Pair::of);

// With default values for shorter stream
Stream<String> labeled = zip(labels, values, "", 0, (label, value) -> label + ": " + value);
```

#### Preferences

`prefs.PreferencesUtilities` provides `java.util.prefs.Preferences` helpers for class-based preference node access.

### `com.globalmentor.beans`

JavaBeans utilities for `java.beans`.

#### Bound Properties

`BoundPropertyObject` provides automatic support for bound and constrained properties with lazy listener management. `GenericPropertyChangeEvent` adds type-safe property change events.

### `com.globalmentor.event`

Event handling utilities.

`EventListenerManager` stores and retrieves event listeners with type-safe registration and thread-safe access.

### `com.globalmentor.time`

Date and time utilities for `java.time`.

`Dates`, `Calendars`, `Durations`, and `TimeZones` provide date/time manipulation. `ElapsingTime` tracks elapsed time intervals.

### `com.globalmentor.iso`

ISO standard definitions.

- `ISO8601` — Date/time format constants per ISO 8601.
- `ISO639` — Language code constants.

### `com.globalmentor.reflect`

Reflection utilities.

`AnnotatedElements` provides annotation discovery across class hierarchies.

### `com.globalmentor.math`

Mathematical utilities.

- `Luhn` — Luhn algorithm checksum validation (credit cards, etc.).
- `Fraction` — Rational number representation.

### `com.globalmentor.si`

SI unit definitions.

`SIPrefix` and `SIUnit` define International System of Units constants.

### `com.globalmentor.jnlp` (legacy)

Java Web Start (JNLP) utilities. Deprecated with JDK 11+ removal of Web Start.

## Issues

Issues tracked by [JIRA](https://globalmentor.atlassian.net/projects/JAVA).

## Changelog

- 0.5.5: (2017-12-15)
	* [JAVA-50](https://globalmentor.atlassian.net/browse/JAVA-50): Clarify parent hierarchy access for URIs.
	* [JAVA-48](https://globalmentor.atlassian.net/browse/JAVA-48): Remove <prerequisites> sections from POMs.
	* [JAVA-47](https://globalmentor.atlassian.net/browse/JAVA-47): Update JSR 305 dependency; change "provided" to "optional".
	* [JAVA-46](https://globalmentor.atlassian.net/browse/JAVA-46): Update version of Hamcrest optional dependency.
	* [JAVA-43](https://globalmentor.atlassian.net/browse/JAVA-43): Improve naming and semantics of reader parser methods.
- 0.5.4: (2017-05-13)
	* [JAVA-42](https://globalmentor.atlassian.net/browse/JAVA-42): Upgraded ABNF definitions to use `Characters` class.
	* [JAVA-36](https://globalmentor.atlassian.net/browse/JAVA-36): Created Unicode `CodePointCharacter` value class.
	* [JAVA-34](https://globalmentor.atlassian.net/browse/JAVA-34): Implemented functional interfaces with support for `IOException`s.
	* [JAVA-28](https://globalmentor.atlassian.net/browse/JAVA-28): Add Unicode separator characters to `Characters` class.
	* [JAVA-27](https://globalmentor.atlassian.net/browse/JAVA-27): Added methods to backup a file on `com.globalmentor.io.Files`.
- 0.5.3: (2017-02-06)
	* [JAVA-20](https://globalmentor.atlassian.net/browse/JAVA-20): Deleted classes that refer to the brand new `com.globalmentor:calendar-calculator` project.
	* [JAVA-12](https://globalmentor.atlassian.net/browse/JAVA-12): Fixed errors found on creation of unit tests for `com.globalmentor.java.Conditions` in [JAVA-11](https://globalmentor.atlassian.net/browse/JAVA-11).
	* [JAVA-11](https://globalmentor.atlassian.net/browse/JAVA-11): Added unit tests for all the methods from `com.globalmentor.java.Conditions`.
	* [JAVA-10](https://globalmentor.atlassian.net/browse/JAVA-10): Added support to surrogate pairs on the method `com.globalmentor.java.StringBuilders.escapeHex(…)`.
	* [JAVA-5](https://globalmentor.atlassian.net/browse/JAVA-5): Changed formatting patterns used on condition check from `com.globalmentor.java.Conditions`.
	* [JAVA-4](https://globalmentor.atlassian.net/browse/JAVA-4): Added arguments to `com.globalmentor.java.Conditions.checkState()` and its unit tests.
	* [JAVA-3](https://globalmentor.atlassian.net/browse/JAVA-3): Improved efficiency of suffix tree edge check in `com.globalmentor.collections.CharSequenceSuffixTree.addEdge(CharSequenceEdge)`.
- 0.5.2:
	* Added utilities class for `Optional<T>`.
- 0.5.1:
	* [JAVA-8](https://globalmentor.atlassian.net/browse/JAVA-8): `com.globalmentor.collections.iterators.JoinIterator.hasNext()` returns `null`.
	* [JAVA-7](https://globalmentor.atlassian.net/browse/JAVA-7): Fixed bug in `com.globalmentor.io.ByteOrderMark.detect(byte[])` that would mistake UTF-32LE for UTF-16LE.
	* [JAVA-2](https://globalmentor.atlassian.net/browse/JAVA-2): Fixed hex decoding in `com.globalmentor.java.CharSequences.unescapeHex(…)` for filename unescaping.
- 0.5.0: Initial final release after conversion from Subversion.
