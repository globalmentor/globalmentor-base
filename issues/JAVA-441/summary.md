## LLM Work Summary for `JAVA-441`: Add `Newtype<V>` interface.

Created `Newtype<V>` marker interface in `com.globalmentor.model` to support the newtype idiom—wrapping types like `UUID` in domain-specific records (e.g., `UserId`, `WidgetRef`) for compile-time type safety while enabling transparent serialization. The interface is minimal (single `value()` method) but required careful design around several non-obvious constraints.

### Key Design Decisions

The wrapped property name was standardized to `value` across all newtype implementations (not arbitrary as originally envisioned), with the constant `VALUE_PROPERTY_NAME` provided for framework use. This simplification aids serialization frameworks and eliminates ambiguity. Type parameter `V` and return type are annotated `@NonNull` (JSpecify) to enforce null-rejection at the type level—implementors should validate in the constructor using compact constructor syntax.

Construction precedence was documented as (1) `public static of(V)` factory method if present, (2) single-argument constructor, allowing frameworks to instantiate newtypes reflectively while supporting custom behavior like validation or instance interning.

### Critical Constraint: No Nested Newtypes

Discussion surfaced that `Newtype<UserId>` (wrapping another newtype) adds no type safety—it's just indirection. The constraint that `V` must not itself implement `Newtype` was established as a rule, with frameworks required to treat violations as errors. This constraint cannot be enforced at compile time in Java; it's a usage discipline. Note: This became a focal discussion point because initial edge-case warnings conflated legitimate Java patterns (inheritance, generics) with Newtype-specific issues.

### Deferred Work

The `Newtypes` utility class for `resolveValueType()` and `create()` was deferred to a separate introspection/conversion library where it will have concrete consumers. This avoids speculative design and allows the utility to be shaped by actual framework integration needs.

### Test Coverage

Initial test coverage was removed after recognizing it tested only Java record behavior, not the `Newtype` interface itself. The interface is a simple marker with one accessor—there is no contract to verify beyond compiler enforcement.
