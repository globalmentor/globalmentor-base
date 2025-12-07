/*
 * Copyright © 2025 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.java;

import static com.globalmentor.java.Bytes.*;
import static com.globalmentor.java.Conditions.*;
import static java.lang.Byte.*;
import static java.lang.Math.*;

import java.io.*;
import java.nio.ByteBuffer;
import java.util.Arrays;
import java.util.stream.IntStream;

import org.jspecify.annotations.*;

/**
 * A readable sequence of byte values.
 * <p>This interface provides uniform, read-only access to many different kinds of byte sequences. Implementations are expected to be immutable, providing value
 * semantics for binary data.</p>
 * <p>This interface refines the general contracts of the {@link #equals(Object)} and {@link #hashCode()} methods. Two {@code ByteSequence} instances are
 * considered equal if and only if they have the same length and contain the same byte values in the same order. The hash code of a {@code ByteSequence} is
 * defined to be equivalent to {@link Arrays#hashCode(byte[])} applied to the result of {@link #toByteArray()}.</p>
 * @apiNote This interface is analogous to {@link CharSequence} but for characters. Unlike {@link CharSequence}, which does not define an equality contract,
 *          {@code ByteSequence} does refine equality semantics. This ensures that {@code byteSequence1.equals(byteSequence2)} implies
 *          {@code byteSequence1.hashCode() == byteSequence2.hashCode()} for any two {@code ByteSequence} instances, allowing {@code ByteSequence} instances to
 *          be used reliably as elements in sets or as keys in maps.
 * @see CharSequence
 */
public interface ByteSequence {

	/**
	 * Returns an empty byte sequence.
	 * @return An empty byte sequence.
	 */
	public static ByteSequence empty() {
		return AbstractByteArrayByteSequence.EMPTY;
	}

	/**
	 * Constructs a byte sequence from a defensive copy of the given bytes.
	 * @apiNote This method is analogous to {@link java.util.List#copyOf(java.util.Collection)}, which creates an immutable copy of the input. For a non-copying
	 *          alternative when the caller guarantees the array will not be modified, see {@link Bytes#asByteSequence(byte[])}.
	 * @param bytes The bytes to copy.
	 * @return A byte sequence containing a copy of the given bytes.
	 * @throws NullPointerException if the given bytes array is {@code null}.
	 * @see Bytes#asByteSequence(byte[])
	 */
	public static ByteSequence copyOf(@NonNull final byte[] bytes) {
		if(bytes.length == 0) {
			return empty();
		}
		return asByteSequence(bytes.clone());
	}

	/**
	 * Constructs a byte sequence from the remaining bytes in the given {@link ByteBuffer}.
	 * <p>This method reads from the buffer's current position to its limit, advancing the position to the limit. The returned byte sequence contains a copy of
	 * the bytes; subsequent modifications to the buffer do not affect the returned sequence.</p>
	 * @param buffer The byte buffer from which to copy the remaining bytes.
	 * @return A byte sequence containing a copy of the buffer's remaining bytes.
	 * @throws NullPointerException if the buffer is {@code null}.
	 */
	public static ByteSequence copyOf(@NonNull final ByteBuffer buffer) {
		if(!buffer.hasRemaining()) {
			return empty();
		}
		final byte[] bytes = new byte[buffer.remaining()];
		buffer.get(bytes);
		return asByteSequence(bytes);
	}

	/**
	 * Compares two {@code ByteSequence} instances lexicographically, treating bytes as unsigned values in the range 0–255.
	 * <p><strong>Important:</strong> Lexicographic comparison is <em>not</em> the same as numeric comparison. This method compares bytes in sequence order, not
	 * as representations of numbers. If the sequences differ in length, the shorter sequence is considered less than the longer sequence if all bytes match up to
	 * the length of the shorter sequence. For example, {@code [1, 2]} is less than {@code [1, 2, 3]}.</p>
	 * @apiNote This method is commonly used for sorting, searching, and ordering byte sequences in collections and data structures. It is useful for building
	 *          sorted collections, binary search implementations, tries, and other algorithms that require deterministic ordering.
	 * @param byteSequence1 The first byte sequence.
	 * @param byteSequence2 The second byte sequence.
	 * @return A negative value if {@code byteSequence1} is lexicographically less than {@code byteSequence2}, zero if they are equal, or a positive value if
	 *         {@code byteSequence1} is lexicographically greater than {@code byteSequence2}.
	 * @throws NullPointerException if either argument is {@code null}.
	 */
	public static int compare(@NonNull final ByteSequence byteSequence1, @NonNull final ByteSequence byteSequence2) {
		final int length1 = byteSequence1.length();
		final int length2 = byteSequence2.length();
		final int minLength = min(length1, length2);
		for(int i = 0; i < minLength; i++) {
			final int cmp = Integer.compare(toUnsignedInt(byteSequence1.byteAt(i)), toUnsignedInt(byteSequence2.byteAt(i)));
			if(cmp != 0) {
				return cmp;
			}
		}
		return Integer.compare(length1, length2);
	}

	/**
	 * Returns the length of this byte sequence.
	 * @return The number of bytes in this sequence.
	 */
	int length();

	/**
	 * Returns the byte value at the specified index.
	 * @param index The index of the byte value to return, zero-based.
	 * @return The byte value at the specified index.
	 * @throws IndexOutOfBoundsException if the index is negative or not less than {@link #length()}.
	 */
	byte byteAt(int index);

	/**
	 * Returns a {@code ByteSequence} that is a subsequence of this sequence. The subsequence starts at the specified {@code start} index and extends to the byte
	 * at index {@code end - 1}. The length of the returned sequence is {@code end - start}.
	 * @param start The start index, inclusive.
	 * @param end The end index, exclusive.
	 * @return The specified subsequence.
	 * @throws IndexOutOfBoundsException if {@code start} or {@code end} are negative, if {@code end} is greater than {@link #length()}, or if {@code start} is
	 *           greater than {@code end}.
	 */
	ByteSequence subSequence(int start, int end);

	/**
	 * Returns a copy of the bytes in this sequence as a new byte array.
	 * <p>The returned array is guaranteed to be free from other references; the caller is free to modify it.</p>
	 * @return A new byte array containing the bytes in this sequence.
	 */
	byte[] toByteArray();

	/**
	 * Returns a read-only {@link ByteBuffer} view of this byte sequence.
	 * <p>The returned buffer's position is zero, its limit and capacity are equal to this sequence's length, and its byte order is
	 * {@link java.nio.ByteOrder#BIG_ENDIAN BIG_ENDIAN}. The buffer is read-only; attempts to modify it will throw {@link java.nio.ReadOnlyBufferException}.</p>
	 * @apiNote This method is useful for interoperability with APIs that accept {@link ByteBuffer}, such as NIO channels and third-party libraries like the AWS
	 *          SDK's {@code SdkBytes.fromByteBuffer()}.
	 * @implSpec The default implementation creates a new byte array via {@link #toByteArray()}, wraps it in a buffer, and returns a read-only view.
	 *           Implementations backed by an array may override this to return a zero-copy read-only view of the underlying array.
	 * @return A read-only {@link ByteBuffer} containing the bytes in this sequence.
	 */
	default ByteBuffer toByteBuffer() {
		return ByteBuffer.wrap(toByteArray()).asReadOnlyBuffer();
	}

	/**
	 * Returns an {@link InputStream} for reading the bytes in this sequence.
	 * <p>The returned stream supports {@link InputStream#mark(int)} and {@link InputStream#reset()}. The stream is independent of this byte sequence; closing the
	 * stream has no effect on this sequence.</p>
	 * @apiNote The name {@code asInputStream()} (rather than {@code toInputStream()}) signals that this method provides a <em>view</em> of the underlying data
	 *          rather than a converted copy. However, the default implementation does involve a copy; see {@code @implSpec} for details.
	 * @implSpec The default implementation creates a new byte array via {@link #toByteArray()} and wraps it in a {@link ByteArrayInputStream}. Implementations
	 *           backed by an array may override this to wrap the underlying array directly, providing a zero-copy view.
	 * @return An {@link InputStream} for reading this byte sequence.
	 */
	default InputStream asInputStream() {
		return new ByteArrayInputStream(toByteArray());
	}

	/**
	 * Returns {@code true} if this byte sequence is empty.
	 * @implSpec The default implementation returns the result of {@code length() == 0}.
	 * @return {@code true} if {@link #length()} is {@code 0}, otherwise {@code false}.
	 */
	default boolean isEmpty() {
		return length() == 0;
	}

	/**
	 * Returns a stream of byte values from this sequence, with each byte zero-extended to an {@code int} in the range 0–255.
	 * @implSpec The default implementation creates a stream from {@link #toByteArray()}, which may involve copying. Implementations backed by an array should
	 *           override this for efficiency.
	 * @return An {@link IntStream} of unsigned byte values from this sequence.
	 */
	default IntStream bytes() {
		final byte[] bytes = toByteArray();
		return IntStream.range(0, bytes.length).map(i -> toUnsignedInt(bytes[i]));
	}

	/**
	 * Copies bytes from this sequence into the given destination array.
	 * @implSpec The default implementation invokes {@link #byteAt(int)} in a loop. Implementations backed by an array should override this using
	 *           {@link System#arraycopy(Object, int, Object, int, int)} for efficiency.
	 * @param srcBegin The start index in this sequence, inclusive.
	 * @param srcEnd The end index in this sequence, exclusive.
	 * @param dest The destination array.
	 * @param destBegin The start index in the destination array.
	 * @throws IndexOutOfBoundsException if any of the following is true:
	 *           <ul>
	 *           <li>{@code srcBegin} is negative</li>
	 *           <li>{@code destBegin} is negative</li>
	 *           <li>{@code srcBegin} is greater than {@code srcEnd}</li>
	 *           <li>{@code srcEnd} is greater than {@link #length()}</li>
	 *           <li>{@code destBegin + (srcEnd - srcBegin)} is greater than {@code dest.length}</li>
	 *           </ul>
	 * @throws NullPointerException if {@code dest} is {@code null}.
	 */
	default void getBytes(final int srcBegin, final int srcEnd, @NonNull final byte[] dest, final int destBegin) {
		checkIndexRangeBounds(srcBegin, srcEnd, length());
		checkIndexRangeBounds(destBegin, destBegin + (srcEnd - srcBegin), dest.length);
		for(int i = srcBegin; i < srcEnd; i++) {
			dest[destBegin + (i - srcBegin)] = byteAt(i);
		}
	}

	/**
	 * Determines whether this byte sequence starts with the specified prefix.
	 * @param prefix The prefix to check.
	 * @return {@code true} if this sequence starts with the given prefix, {@code false} otherwise.
	 * @throws NullPointerException if the prefix is {@code null}.
	 * @see #isPrefixOf(ByteSequence)
	 */
	default boolean startsWith(@NonNull final ByteSequence prefix) {
		final int prefixLength = prefix.length();
		if(length() < prefixLength) {
			return false;
		}
		for(int i = 0; i < prefixLength; i++) {
			if(byteAt(i) != prefix.byteAt(i)) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Determines whether this byte sequence starts with the specified prefix bytes.
	 * @param prefix The prefix bytes to check.
	 * @return {@code true} if this sequence starts with the given prefix, {@code false} otherwise.
	 * @throws NullPointerException if the prefix is {@code null}.
	 * @see #isPrefixOf(byte[])
	 */
	default boolean startsWith(@NonNull final byte[] prefix) {
		if(length() < prefix.length) {
			return false;
		}
		for(int i = 0; i < prefix.length; i++) {
			if(byteAt(i) != prefix[i]) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Determines whether this byte sequence ends with the specified suffix.
	 * @param suffix The suffix to check.
	 * @return {@code true} if this sequence ends with the given suffix, {@code false} otherwise.
	 * @throws NullPointerException if the suffix is {@code null}.
	 * @see #isSuffixOf(ByteSequence)
	 */
	default boolean endsWith(@NonNull final ByteSequence suffix) {
		final int suffixLength = suffix.length();
		final int offset = length() - suffixLength;
		if(offset < 0) {
			return false;
		}
		for(int i = 0; i < suffixLength; i++) {
			if(byteAt(offset + i) != suffix.byteAt(i)) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Determines whether this byte sequence ends with the specified suffix bytes.
	 * @param suffix The suffix bytes to check.
	 * @return {@code true} if this sequence ends with the given suffix, {@code false} otherwise.
	 * @throws NullPointerException if the suffix is {@code null}.
	 * @see #isSuffixOf(byte[])
	 */
	default boolean endsWith(@NonNull final byte[] suffix) {
		final int offset = length() - suffix.length;
		if(offset < 0) {
			return false;
		}
		for(int i = 0; i < suffix.length; i++) {
			if(byteAt(offset + i) != suffix[i]) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Determines whether this byte sequence is a prefix of the specified byte sequence.
	 * <p>This is the inverse of {@link #startsWith(ByteSequence)}: {@code a.startsWith(b)} is equivalent to {@code b.isPrefixOf(a)}.</p>
	 * @param bytes The byte sequence to check.
	 * @return {@code true} if this sequence is a prefix of the given sequence, {@code false} otherwise.
	 * @throws NullPointerException if the byte sequence is {@code null}.
	 * @see #startsWith(ByteSequence)
	 */
	default boolean isPrefixOf(@NonNull final ByteSequence bytes) {
		return bytes.startsWith(this);
	}

	/**
	 * Determines whether this byte sequence is a prefix of the specified byte array.
	 * <p>This is the inverse perspective of prefix checking: it asks whether <em>this</em> sequence appears at the start of the given array.</p>
	 * @param bytes The byte array to check.
	 * @return {@code true} if this sequence is a prefix of the given array, {@code false} otherwise.
	 * @throws NullPointerException if the byte array is {@code null}.
	 * @see #startsWith(byte[])
	 */
	default boolean isPrefixOf(@NonNull final byte[] bytes) {
		final int thisLength = length();
		if(bytes.length < thisLength) {
			return false;
		}
		for(int i = 0; i < thisLength; i++) {
			if(byteAt(i) != bytes[i]) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Determines whether this byte sequence is a suffix of the specified byte sequence.
	 * <p>This is the inverse of {@link #endsWith(ByteSequence)}: {@code a.endsWith(b)} is equivalent to {@code b.isSuffixOf(a)}.</p>
	 * @param bytes The byte sequence to check.
	 * @return {@code true} if this sequence is a suffix of the given sequence, {@code false} otherwise.
	 * @throws NullPointerException if the byte sequence is {@code null}.
	 * @see #endsWith(ByteSequence)
	 */
	default boolean isSuffixOf(@NonNull final ByteSequence bytes) {
		return bytes.endsWith(this);
	}

	/**
	 * Determines whether this byte sequence is a suffix of the specified byte array.
	 * <p>This is the inverse perspective of suffix checking: it asks whether <em>this</em> sequence appears at the end of the given array.</p>
	 * @param bytes The byte array to check.
	 * @return {@code true} if this sequence is a suffix of the given array, {@code false} otherwise.
	 * @throws NullPointerException if the byte array is {@code null}.
	 * @see #endsWith(byte[])
	 */
	default boolean isSuffixOf(@NonNull final byte[] bytes) {
		final int thisLength = length();
		final int offset = bytes.length - thisLength;
		if(offset < 0) {
			return false;
		}
		for(int i = 0; i < thisLength; i++) {
			if(byteAt(i) != bytes[offset + i]) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Determines whether this byte sequence contains the same bytes as the given byte array.
	 * @param bytes The byte array to compare with.
	 * @return {@code true} if this sequence has the same length and contains the same byte values as the given array, {@code false} otherwise.
	 * @throws NullPointerException if the byte array is {@code null}.
	 */
	default boolean equalsBytes(@NonNull final byte[] bytes) {
		if(length() != bytes.length) {
			return false;
		}
		for(int i = 0; i < bytes.length; i++) {
			if(byteAt(i) != bytes[i]) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Compares the specified object with this byte sequence for equality. Returns {@code true} if and only if the specified object is also a
	 * {@code ByteSequence}, both sequences have the same length, and all corresponding byte values are equal.
	 * <p>This definition ensures that the equals method works properly across different implementations of the {@code ByteSequence} interface.</p>
	 * @param obj The object to compare with this sequence.
	 * @return {@code true} if the specified object is equal to this byte sequence.
	 * @see #hashCode()
	 */
	@Override
	boolean equals(Object obj);

	/**
	 * Returns the hash code value for this byte sequence. The hash code is defined to be equivalent to {@link Arrays#hashCode(byte[])} applied to the result of
	 * {@link #toByteArray()}.
	 * <p>This ensures that {@code byteSequence1.equals(byteSequence2)} implies {@code byteSequence1.hashCode() == byteSequence2.hashCode()} for any two
	 * {@code ByteSequence} instances {@code byteSequence1} and {@code byteSequence2}, as required by the general contract of {@link Object#hashCode()}.</p>
	 * @return The hash code value for this byte sequence.
	 * @see #equals(Object)
	 * @see Arrays#hashCode(byte[])
	 */
	@Override
	int hashCode();

}
