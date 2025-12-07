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

import static com.globalmentor.java.Bytes.NO_BYTES;
import static com.globalmentor.java.Conditions.*;
import static java.util.Objects.*;

import java.io.*;
import java.nio.ByteBuffer;
import java.util.Arrays;
import java.util.stream.IntStream;

import org.jspecify.annotations.*;

/**
 * Abstract base class for {@link ByteSequence} implementations backed by a byte array.
 * <p>This class provides efficient implementations of all {@link ByteSequence} methods using direct byte array access. Subclasses can access the underlying
 * byte array via the {@link #getByteArray()} method, enabling efficient operations without defensive copying.</p>
 * @apiNote Subclasses should be declared {@code final} to maintain immutability guarantees. Methods in this class are optimized to use direct array access
 *          through {@link #getByteArray()} for operations such as prefix/suffix checking and bulk copying. Subclasses <strong>must not modify</strong> the
 *          returned array from {@link #getByteArray()}. A non-{@code final} subclass could potentially violate immutability by allowing further subclassing
 *          that modifies the array.
 * @implNote The immutability of this class depends on the caller not retaining or modifying references to the byte array passed to the constructor. Use
 *           {@link ByteSequence#copyOf(byte[])} for safe construction from untrusted sources.
 * @see ByteSequence#copyOf(byte[])
 * @see Bytes#asByteSequence(byte[])
 */
public abstract class AbstractByteArrayByteSequence implements ByteSequence {

	/** Singleton empty byte sequence instance. */
	static final ByteSequence EMPTY = new AbstractByteArrayByteSequence(NO_BYTES) {};

	/**
	 * The underlying byte array. This is kept private to prevent subclasses from accidentally modifying it.
	 */
	private final byte[] bytes;

	/**
	 * Constructs a byte sequence backed by the given byte array.
	 * @apiNote This constructor does <em>not</em> make a defensive copy of the array. The caller must ensure that the array is not modified after construction.
	 *          For safe construction from untrusted sources, use {@link ByteSequence#copyOf(byte[])}.
	 * @param bytes The byte array backing this sequence.
	 * @throws NullPointerException if the byte array is {@code null}.
	 */
	protected AbstractByteArrayByteSequence(@NonNull final byte[] bytes) {
		this.bytes = requireNonNull(bytes);
	}

	/**
	 * Returns the underlying byte array.
	 * <p>The returned array <strong>must not be modified</strong>. Modifying the array will violate the immutability contract of this class.</p>
	 * @apiNote This method is provided for subclasses to enable efficient implementations. Methods in this class use direct array access through this method to
	 *          support optimizations such as using {@link Arrays#equals(byte[], int, int, byte[], int, int)} for prefix/suffix comparisons and
	 *          {@link System#arraycopy(Object, int, Object, int, int)} for bulk byte copying, avoiding unnecessary intermediate allocations.
	 * @implSpec This method returns a direct reference to the underlying byte array without making a defensive copy. Subclasses must respect the immutability
	 *           contract by not modifying the returned array.
	 * @return The underlying byte array.
	 */
	protected byte[] getByteArray() {
		return bytes;
	}

	@Override
	public int length() {
		return getByteArray().length;
	}

	@Override
	public byte byteAt(final int index) {
		final byte[] byteArray = getByteArray();
		checkIndexBounds(index, byteArray.length);
		return byteArray[index];
	}

	@Override
	public ByteSequence subSequence(final int start, final int end) {
		final byte[] byteArray = getByteArray();
		checkIndexRangeBounds(start, end, byteArray.length);
		if(start == end) {
			return ByteSequence.empty();
		}
		if(start == 0 && end == byteArray.length) {
			return this;
		}
		return Bytes.asByteSequence(Arrays.copyOfRange(byteArray, start, end));
	}

	@Override
	public byte[] toByteArray() {
		return getByteArray().clone();
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation returns a read-only {@link ByteBuffer} wrapping the underlying byte array directly, without copying. The returned buffer
	 *           cannot be used to modify the underlying data.
	 */
	@Override
	public ByteBuffer toByteBuffer() {
		return ByteBuffer.wrap(getByteArray()).asReadOnlyBuffer();
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation returns a {@link ByteArrayInputStream} wrapping the underlying byte array directly, without copying.
	 */
	@Override
	public InputStream asInputStream() {
		return new ByteArrayInputStream(getByteArray());
	}

	@Override
	public boolean isEmpty() {
		return getByteArray().length == 0;
	}

	@Override
	public IntStream bytes() {
		final byte[] byteArray = getByteArray();
		return IntStream.range(0, byteArray.length).map(i -> Byte.toUnsignedInt(byteArray[i]));
	}

	@Override
	public void getBytes(final int srcBegin, final int srcEnd, @NonNull final byte[] dest, final int destBegin) {
		final byte[] byteArray = getByteArray();
		checkIndexRangeBounds(srcBegin, srcEnd, byteArray.length);
		checkIndexRangeBounds(destBegin, destBegin + (srcEnd - srcBegin), dest.length);
		System.arraycopy(byteArray, srcBegin, dest, destBegin, srcEnd - srcBegin);
	}

	@Override
	public boolean startsWith(@NonNull final ByteSequence prefix) {
		final byte[] byteArray = getByteArray();
		final int prefixLength = prefix.length();
		if(byteArray.length < prefixLength) {
			return false;
		}
		// optimize for array-backed prefixes
		if(prefix instanceof AbstractByteArrayByteSequence arrayPrefix) {
			return Arrays.equals(byteArray, 0, prefixLength, arrayPrefix.getByteArray(), 0, prefixLength);
		}
		for(int i = 0; i < prefixLength; i++) {
			if(byteArray[i] != prefix.byteAt(i)) {
				return false;
			}
		}
		return true;
	}

	@Override
	public boolean startsWith(@NonNull final byte[] prefix) {
		final byte[] byteArray = getByteArray();
		if(byteArray.length < prefix.length) {
			return false;
		}
		return Arrays.equals(byteArray, 0, prefix.length, prefix, 0, prefix.length);
	}

	@Override
	public boolean endsWith(@NonNull final ByteSequence suffix) {
		final byte[] byteArray = getByteArray();
		final int suffixLength = suffix.length();
		final int offset = byteArray.length - suffixLength;
		if(offset < 0) {
			return false;
		}
		// optimize for array-backed suffixes
		if(suffix instanceof AbstractByteArrayByteSequence arraySuffix) {
			return Arrays.equals(byteArray, offset, byteArray.length, arraySuffix.getByteArray(), 0, suffixLength);
		}
		for(int i = 0; i < suffixLength; i++) {
			if(byteArray[offset + i] != suffix.byteAt(i)) {
				return false;
			}
		}
		return true;
	}

	@Override
	public boolean endsWith(@NonNull final byte[] suffix) {
		final byte[] byteArray = getByteArray();
		final int offset = byteArray.length - suffix.length;
		if(offset < 0) {
			return false;
		}
		return Arrays.equals(byteArray, offset, byteArray.length, suffix, 0, suffix.length);
	}

	@Override
	public boolean isPrefixOf(@NonNull final ByteSequence other) {
		if(other instanceof AbstractByteArrayByteSequence arrayOther) {
			return arrayOther.startsWith(getByteArray());
		}
		return other.startsWith(this);
	}

	@Override
	public boolean isPrefixOf(@NonNull final byte[] other) {
		final byte[] byteArray = getByteArray();
		if(other.length < byteArray.length) {
			return false;
		}
		return Arrays.equals(other, 0, byteArray.length, byteArray, 0, byteArray.length);
	}

	@Override
	public boolean isSuffixOf(@NonNull final ByteSequence other) {
		if(other instanceof AbstractByteArrayByteSequence arrayOther) {
			return arrayOther.endsWith(getByteArray());
		}
		return other.endsWith(this);
	}

	@Override
	public boolean isSuffixOf(@NonNull final byte[] other) {
		final byte[] byteArray = getByteArray();
		final int offset = other.length - byteArray.length;
		if(offset < 0) {
			return false;
		}
		return Arrays.equals(other, offset, other.length, byteArray, 0, byteArray.length);
	}

	@Override
	public boolean equalsBytes(@NonNull final byte[] other) {
		return Arrays.equals(getByteArray(), other);
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation optimizes equality checking for other {@code AbstractByteArrayByteSequence} instances by using
	 *           {@link Arrays#equals(byte[], byte[])} for direct array comparison, avoiding element-by-element iteration.
	 */
	@Override
	public final boolean equals(final Object obj) {
		if(obj == this) {
			return true;
		}
		if(!(obj instanceof ByteSequence other)) {
			return false;
		}
		final byte[] byteArray = getByteArray();
		// optimize for array-backed sequences
		if(other instanceof AbstractByteArrayByteSequence arrayOther) {
			return Arrays.equals(byteArray, arrayOther.getByteArray());
		}
		// fall back to element-by-element comparison
		if(byteArray.length != other.length()) {
			return false;
		}
		for(int i = 0; i < byteArray.length; i++) {
			if(byteArray[i] != other.byteAt(i)) {
				return false;
			}
		}
		return true;
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation returns {@link Arrays#hashCode(byte[])} of the underlying byte array.
	 */
	@Override
	public final int hashCode() {
		return Arrays.hashCode(getByteArray());
	}

	/**
	 * Returns a string representation of this byte sequence.
	 * @implSpec This implementation returns a string in the format {@code "ByteSequence[length=N]"} where {@code N} is the length of this sequence.
	 * @return A string representation of this byte sequence.
	 */
	@Override
	public String toString() {
		return "ByteSequence[length=%d]".formatted(getByteArray().length);
	}

}
