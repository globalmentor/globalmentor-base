# Todo

## Legacy Code Cleanup

- [ ] `com.globalmentor.java.Strings` — Contains `StringBuffer` usage that should be converted to `StringBuilder`. See TODO comment at line 42.
- [ ] `com.globalmentor.text.W3CDateFormat` — Uses `StringBuffer` per `DateFormat` contract; consider documenting as legacy.
- [ ] `com.globalmentor.jnlp.JNLP` — Java Web Start was removed in JDK 11. Mark entire package as deprecated or remove.

## Deprecated Code for Removal

- [ ] `com.globalmentor.io.InputStreams.EMPTY_INPUT_STREAM` — Deprecated in favor of `InputStream.nullInputStream()`.
- [ ] `com.globalmentor.io.InputStreams.readBytes(InputStream)` — Deprecated in favor of `InputStream.readAllBytes()`.
- [ ] `com.globalmentor.io.InputStreams.readBytes(InputStream, int)` — Deprecated in favor of `InputStream.readNBytes(int)`.
- [ ] `com.globalmentor.io.ByteOrderMark.getBytes()` — Deprecated in favor of `toByteSequence().toByteArray()`.
- [ ] `com.globalmentor.io.ByteOrderMark.getByteCount()` — Deprecated in favor of `length()`.
- [ ] `com.globalmentor.security.Hash.getBytes()` — Deprecated in favor of `toByteArray()`.
- [ ] `com.globalmentor.java.Bytes.startsWith(byte[], byte[])` — Deprecated in favor of `ByteSequence.startsWith()`.
- [ ] `com.globalmentor.java.Classes` getter/setter constants — Deprecated for removal in favor of a separate library.
- [ ] `com.globalmentor.text.ArgumentSyntaxException.getInput()` and `getIndex()` — Deprecated in favor of `findInput()` and `findIndex()`.
- [ ] `com.globalmentor.net.URIs.checkScheme()` — Deprecated in favor of `checkArgumentScheme()` with case-insensitive comparison.
- [ ] `com.globalmentor.net.EmailAddress` pattern constants — Deprecated in favor of `EmailAddressRegEx`.

## Code Quality Issues

- [ ] `com.globalmentor.collections.Maps.putAll()` — Typo: `entriy` should be `entry` (line 59).
- [ ] `com.globalmentor.event.EventListenerManager` — TODO comment notes that `WeakHashSet` is problematic for anonymous listeners.
- [ ] `com.globalmentor.beans.BoundPropertyObject` — Uses `synchronized` on `this` in several places; consider more fine-grained locking.

## Documentation Issues

- [ ] `com.globalmentor.io.ParseReader` and `com.globalmentor.io.ReaderParser` overlap in functionality; clarify intended use cases.
- [ ] `com.globalmentor.collections.ReverseMap` — Needs clearer documentation of thread-safety guarantees.

## API Improvements

- [ ] `com.globalmentor.model.Range` — Uses nullable bounds for infinity; consider dedicated sentinel values or `Optional`.
- [ ] `com.globalmentor.java.Objects.checkType()` — Consider returning `Optional` instead of casting with potential `ClassCastException`.

## Consistency Issues

- [ ] Mixed use of `@Deprecated(forRemoval = true)` and plain `@Deprecated` — standardize deprecation approach.
- [ ] Some classes use JSpecify `@NonNull`, others use no null annotations — consider consistent annotation policy.

## Test Coverage

- [ ] Verify test coverage for `ByteSequence` implementations.
- [ ] Verify test coverage for `CompoundTokenization` edge cases.
- [ ] `com.globalmentor.collections.CharSequenceSuffixTree` — Has limited test coverage per changelog.
