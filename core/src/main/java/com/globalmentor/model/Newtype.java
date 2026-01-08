/*
 * Copyright © 2026 GlobalMentor, Inc. <https://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.model;

import org.jspecify.annotations.*;

/**
 * A type-safe wrapper around a single value, providing nominal type distinction without adding additional semantics.
 * <p>The "newtype" idiom originated in Haskell and was popularized by Rust. It addresses the problem of using generic types like {@link java.util.UUID} for
 * multiple distinct domain concepts. For example, both a <code>userId</code> and a <code>widgetId</code> might be represented as UUIDs, but they should not be
 * interchangeable in code. A newtype wrapper provides compile-time type safety by creating distinct types for each concept.</p>
 * <p>The primary motivations for using newtypes are:</p>
 * <ul>
 * <li><strong>Type safety:</strong> Prevents accidental misuse of values. A method expecting a <code>WidgetRef</code> will not accept a <code>UserId</code>,
 * even though both wrap UUIDs.</li>
 * <li><strong>Efficient serialization:</strong> Serialization frameworks can recognize newtypes and serialize them as the wrapped value directly (e.g., as a
 * bare UUID string in JSON) rather than as a wrapper object.</li>
 * </ul>
 * <p>This interface is designed for easy implementation using Java {@link Record records}. Implementations should reject <code>null</code> values in the
 * constructor:</p>
 * <pre><code>
 * public record WidgetRef(UUID value) implements Newtype&lt;UUID&gt; {
 *     public WidgetRef { requireNonNull(value); }
 * }
 * </code></pre>
 * <p>For deserialization or other reflective instantiation, frameworks should use the following precedence to construct a newtype instance:</p>
 * <ol>
 * <li>A <code>public static</code> factory method named <code>of</code> accepting a single argument of the wrapped type (or, for boxed primitives, the
 * corresponding primitive type).</li>
 * <li>A single-argument constructor accepting the wrapped type.</li>
 * </ol>
 * <p>Because <code>Newtype</code> uses generics, the wrapped type must be a reference type. A record wrapping a primitive value must use the corresponding
 * boxed type (e.g., <code>Integer</code> rather than <code>int</code>) for the record component to satisfy the interface. However, the <code>of()</code>
 * factory method may accept the primitive type for convenience, relying on auto-boxing.</p>
 * <p>The <code>of()</code> factory method allows implementors to provide custom behavior such as validation, normalization, or interning of common values:</p>
 * <pre><code>
 * public record StatusCode(Integer value) implements Newtype&lt;Integer&gt; {
 *     private static final StatusCode OK = new StatusCode(200);
 *     private static final StatusCode NOT_FOUND = new StatusCode(404);
 *
 *     public static StatusCode of(final int value) {
 *         return switch(value) {
 *             case 200 -&gt; OK;
 *             case 404 -&gt; NOT_FOUND;
 *             default -&gt; new StatusCode(value);
 *         };
 *     }
 * }
 * </code></pre>
 * <h2>Constraints</h2>
 * <ul>
 * <li><strong>No nested newtypes:</strong> The wrapped type <code>V</code> must not itself implement <code>Newtype</code>. Frameworks must treat this as an
 * error.</li>
 * </ul>
 * <h2>Notes for Framework Implementors</h2>
 * <ul>
 * <li><strong>Inherited implementation:</strong> A newtype may inherit <code>Newtype&lt;V&gt;</code> from a supertype rather than implementing it directly.
 * Resolving the wrapped type <code>V</code> may require walking the type hierarchy and substituting type variables.</li>
 * </ul>
 * @apiNote This is similar to Swift's {@code RawRepresentable} protocol.
 * @param <V> The type of the wrapped value; must not itself implement <code>Newtype</code>.
 * @see <a href="https://rust-unofficial.github.io/patterns/patterns/behavioural/newtype.html">Rust Design Patterns § 3.1.3. Newtype</a>
 * @see <a href="https://wiki.haskell.org/Newtype">HaskellWiki: Newtype</a>
 */
public interface Newtype<@NonNull V> {

	/** The name of the wrapped value property. */
	public static final String VALUE_PROPERTY_NAME = "value";

	/**
	 * Returns the wrapped value.
	 * @return The underlying value wrapped by this newtype; never <code>null</code>.
	 */
	public @NonNull V value();

}
