# GlobalMentor Core Java Library

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
