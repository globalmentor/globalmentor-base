/*
 * Copyright © 1996-2025 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.security;

import java.security.MessageDigest;
import java.util.HexFormat;

import org.jspecify.annotations.*;

import com.globalmentor.java.AbstractByteArrayByteSequence;
import com.globalmentor.java.ByteSequence;

/**
 * The encapsulation of message digest output, providing data immutability and convenience methods for updating other message digests.
 * <p>This class extends {@link AbstractByteArrayByteSequence} and thus implements {@link ByteSequence}, allowing hash values to be compared and manipulated as
 * immutable byte sequences. Two {@code Hash} instances are equal if and only if they contain the same byte values, regardless of the algorithm used to produce
 * them. A {@code Hash} can also be equal to other {@link ByteSequence} implementations containing the same bytes.</p>
 * @apiNote This class provides programming safety against mutability but not cryptographic safety against Byzantine attacks. In other words, this class is
 *          useful for preventing accidental modification of bytes due to a logic error, and is thus orders of magnitude safer than using a raw byte array.
 *          However it would be possible to create a back-door mutable instance by passing a rogue or buggy {@link MessageDigest} implementation to
 *          {@link Hash#fromDigest(MessageDigest)}. A rogue or buggy {@link MessageDigest} implementation could also change the contents of the hash bytes via
 *          its {@link MessageDigest#update(byte[])} method when {@link #updateMessageDigest(MessageDigest)} is called.
 * @implNote The immutability of this class in conjunction with {@link MessageDigests} depends on all implementations of {@link MessageDigest#digest()} and
 *           similar methods to return a byte array free from other references; and depends on all implementations of {@link MessageDigest#update(byte[])} not
 *           to change any information in the byte array passed to it.
 * @author Garret Wilson
 * @see ByteSequence
 */
public final class Hash extends AbstractByteArrayByteSequence {

	/**
	 * Hash bytes constructor.
	 * @implSpec This constructor does <em>not</em> make a defensive copy of the bytes.
	 * @param bytes The bytes of a message digest.
	 */
	private Hash(@NonNull final byte[] bytes) {
		super(bytes);
	}

	/**
	 * Constructs a hash from message digest bytes.
	 * @implSpec A defensive copy is made of the bytes.
	 * @param bytes The bytes of a message digest.
	 * @return The hash encapsulation of the message digest bytes.
	 */
	public static Hash of(@NonNull final byte[] bytes) {
		return new Hash(bytes.clone());
	}

	/**
	 * Constructs a hash from a checksum string.
	 * @param checksum A hex checksum string for the hash bytes.
	 * @return The resulting hash.
	 * @see MessageDigest#digest()
	 */
	public static Hash fromChecksum(@NonNull final CharSequence checksum) {
		return new Hash(HexFormat.of().parseHex(checksum));
	}

	/**
	 * Constructs a hash for the current contents of the message digest, equivalent to creating a {@link Hash} instance from the result of
	 * {@link MessageDigest#digest()}. The message digest is reset after this call is made.
	 * @param messageDigest The implementation of a message digest algorithm.
	 * @return The resulting hash.
	 * @see MessageDigest#digest()
	 */
	public static Hash fromDigest(@NonNull final MessageDigest messageDigest) {
		return new Hash(messageDigest.digest());
	}

	/**
	 * Updates a digest with the bytes of this hash.
	 * @implNote The immutability of this class depends on the implementations of {@link MessageDigest#update(byte[])} not to change any information in the byte
	 *           array passed to it.
	 * @param messageDigest The implementation of a message digest algorithm.
	 * @return The updated message digest.
	 * @see MessageDigest#update(byte[])
	 */
	public MessageDigest updateMessageDigest(@NonNull final MessageDigest messageDigest) {
		messageDigest.update(getByteArray());
		return messageDigest;
	}

	/**
	 * Returns the message digest bytes. The resulting byte array is guaranteed to be free from other references.
	 * @apiNote This method should only be called when the actual bytes are truly needed, because of the overhead in protecting the bytes using e.g. a defensive
	 *          copy. Otherwise try to use other methods that can access the bytes internally, such as {@link #updateMessageDigest(MessageDigest)}.
	 * @implSpec This implementation delegates to {@link #toByteArray()}.
	 * @return The message digest bytes.
	 * @deprecated Use {@link #toByteArray()} instead.
	 */
	@Deprecated(forRemoval = true)
	public byte[] getBytes() {
		return toByteArray();
	}

	/**
	 * Computes a lowercase hex checksum string for the hash bytes.
	 * @apiNote This method considers a <dfn>checksum</dfn> to be a string version of a message <dfn>digest</dfn>, as the former is often used in the context of
	 *          file contents verification.
	 * @implNote This implementation exposes the underlying mutable bytes of the hash to {@link HexFormat#formatHex(byte[])}, which is considered safe because the
	 *           class is {@code final} and part of the JDK.
	 * @return The lowercase hex checksum string of the hash bytes.
	 * @see #toByteArray()
	 */
	public String toChecksum() {
		return HexFormat.of().formatHex(getByteArray());
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation delegates to {@link #toChecksum()}.
	 */
	@Override
	public String toString() {
		return toChecksum();
	}

}
