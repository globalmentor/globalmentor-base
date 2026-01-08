Sometimes it is beneficial to wrap a type such as `UUID` in a thin wrapper such as a `record` in order to provide safety, e.g. between a `User` identifier and a `Widget` identifier. There might be a `UserRef` record and a `WidgetRef` record, each with a single `UUID` field. (In Rust this is called the “newtype” idiom, a term that comes from Haskell. See e.g. [Rust Design Patterns § 3.1.3. Newtype](https://rust-unofficial.github.io/patterns/patterns/behavioural/newtype.html).)

This structures might be serialized as the bare `UUID`, but this ticket is solely for the newtype facility itself. Create some sort of way to identify these “newtype” types, with a design to facilitate serialization as the wrapped value.

For example here is a `Widget` newtype:

```java
/**
 * A type-safe reference to a widget.
 * @apiNote This is similar to the "newtype" idiom in Rust, which originated in Haskell.
 * @param uuid The widget UUID.
 * @see <a href="https://rust-unofficial.github.io/patterns/patterns/behavioural/newtype.html">Rust Design Patterns § 3.1.3. Newtype</a>
 * @see <a href="https://wiki.haskell.org/Newtype">HaskellWiki: Newtype</a>
 */
public record WidgetRef(UUID uuid) {
}
```

## Result

Created `com.globalmentor.model.Newtype<V>`, a marker interface for the newtype idiom. Records implementing `Newtype` gain compile-time type safety while enabling serialization frameworks to treat them as transparent wrappers.

### Usage

```java
public record WidgetRef(UUID value) implements Newtype<UUID> {
    public WidgetRef { requireNonNull(value); }
}

public record UserId(UUID value) implements Newtype<UUID> {
    public UserId { requireNonNull(value); }
}
```

### Key Design Decisions

- **Property name:** The wrapped value must be named `value` to satisfy the interface. The constant `Newtype.VALUE_PROPERTY_NAME` provides this for framework use.
- **Null contract:** `@NonNull` on the type parameter and return type. Implementations should reject null in the constructor.
- **Construction precedence:** Frameworks should use (1) `public static of(V)` factory if present, then (2) single-argument constructor. The `of()` method allows validation, normalization, or interning.
- **No nested newtypes:** The wrapped type `V` must not itself implement `Newtype`. Frameworks must treat this as an error.
- **Boxed primitives:** Since generics require reference types, records wrapping primitives must use boxed types (e.g., `Integer` not `int`). The `of()` factory may accept the primitive for convenience.

### Out of Scope

The `Newtypes` utility class (for `resolveValueType()` and `create()`) was deferred to a separate introspection/conversion library where it will have concrete consumers.
```
