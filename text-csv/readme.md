# GlobalMentor Text CSV Library

RFC 4180-compliant CSV parsing and serialization utilities.

## Quick Reference

**CSV parsing:** `com.globalmentor.text.csv.CsvParser` — `parseLine()` parses a CSV line into `Field[]` with proper quote handling and escape processing.

**CSV serialization:** `com.globalmentor.text.csv.CsvSerializer` — `serialize()` writes objects as a CSV record; `encodeField()` handles quoting and escaping.

**File operations:** `com.globalmentor.text.csv.CSV` — `appendRecord()` appends records to a file with optional header initialization; constants for media type, delimiters, and filename extension.

## Overview

### `com.globalmentor.text.csv`

CSV parsing and serialization per [RFC 4180](https://www.rfc-editor.org/rfc/rfc4180.html).

#### Constants

`CSV` provides RFC 4180 constants and file utilities:

```java
MediaType csvType = CSV.CSV_MEDIA_TYPE; // text/csv
String extension = CSV.CSV_FILENAME_EXTENSION; // "csv"
```

**Appending records to files:**

```java
// Append a record (creates file if needed)
CSV.appendRecord(path, "value1", "value2", 123);

// Append with headers (headers written only if file doesn't exist)
String[] headers = {"Name", "Value", "Count"};
CSV.appendRecord(path, headers, "value1", "value2", 123);
```

#### Parsing

`CsvParser` parses CSV lines into typed fields:

```java
CsvParser parser = new CsvParser();
CsvParser.Field[] fields = parser.parseLine("foo,\"bar,baz\",123");
// fields[0].toString() → "foo"
// fields[1].toString() → "bar,baz" (quotes removed, comma preserved)
// fields[2].toString() → "123"
```

The parser handles:
- Quoted fields with embedded commas, quotes, and newlines
- Escaped quotes (doubled: `""` → `"`)
- Empty fields

#### Serialization

`CsvSerializer` writes objects as CSV records:

```java
CsvSerializer.serialize(writer, "name", "value with, comma", 123);
// Output: name,"value with, comma",123\r\n
```

**Field encoding:**

```java
String encoded = CsvSerializer.encodeField("value with \"quotes\"");
// → "\"value with \"\"quotes\"\"\""
```

## Issues

Issues tracked by [JIRA](https://globalmentor.atlassian.net/projects/JAVA).
