# Todo

## API Improvements

- [ ] `com.globalmentor.text.csv.CSV.appendRecord()` — Has a race condition for header writing documented in `@implNote`; consider atomic file creation or advisory locking.

## Test Coverage

- [ ] `com.globalmentor.text.csv.CsvSerializer` — No unit tests for serialization methods.
- [ ] `com.globalmentor.text.csv.CSV.appendRecord()` — No integration tests for file operations.
