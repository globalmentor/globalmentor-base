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

package com.globalmentor.java;

import static com.globalmentor.java.Conditions.*;

import java.util.Random;

import org.jspecify.annotations.*;

/**
 * Utilities for manipulating bytes.
 * @author Garret Wilson
 * @see ByteSequence
 */
public final class Bytes {

	/** A shared empty array of bytes. */
	public static final byte[] NO_BYTES = new byte[0];

	/**
	 * Returns a {@link ByteSequence} view of the given byte array.
	 * <p>The returned byte sequence is backed directly by the given array; no defensive copy is made. Changes to the array will be reflected in the byte
	 * sequence.</p>
	 * @apiNote This method is analogous to {@link java.util.Arrays#asList(Object...)}, which returns a list view backed by the given array. As with
	 *          {@code Arrays.asList()}, callers should ensure the array is not modified after calling this method if immutability is desired. For a
	 *          safely-copying alternative, use {@link ByteSequence#copyOf(byte[])}.
	 * @param bytes The byte array to wrap.
	 * @return A {@link ByteSequence} backed by the given array.
	 * @throws NullPointerException if the byte array is {@code null}.
	 * @see ByteSequence#copyOf(byte[])
	 */
	public static ByteSequence asByteSequence(@NonNull final byte[] bytes) {
		if(bytes.length == 0) {
			return ByteSequence.empty();
		}
		return new AbstractByteArrayByteSequence(bytes) {};
	}

	/** This class cannot be publicly instantiated. */
	private Bytes() {
	}

	/**
	 * Creates an array of random bytes.
	 * @implSpec This implementation delegates to {@link #generateRandom(int, Random)} using a default {@link Random} instance.
	 * @param length The number of bytes to create.
	 * @return A new array of the given length filled with random bytes.
	 * @throws IllegalArgumentException if the given length is negative.
	 * @see Random#nextBytes(byte[])
	 */
	public static byte[] generateRandom(final int length) {
		return generateRandom(length, new Random());
	}

	/**
	 * Creates an array of random bytes.
	 * @param length The number of bytes to create.
	 * @param random The random number generator to use.
	 * @return A new array of the given length filled with random bytes.
	 * @throws NullPointerException if the given random number generator is <code>null</code>.
	 * @throws IllegalArgumentException if the given length is negative.
	 * @see Random#nextBytes(byte[])
	 */
	public static byte[] generateRandom(final int length, @NonNull final Random random) {
		checkArgumentNotNegative(length);
		final byte[] bytes = new byte[length]; //create a new array of bytes
		random.nextBytes(bytes); //fill the byte array with random values
		return bytes;
	}

	/**
	 * Determines if the given byte array starts with the specified prefix. Neither byte array is modified by this method.
	 * @param bytes The bytes being examined.
	 * @param prefix The prefix to compare with the given bytes.
	 * @return Whether the given bytes start with the specified prefix.
	 * @deprecated Use {@link ByteSequence#startsWith(byte[])} or {@link ByteSequence#isPrefixOf(byte[])} instead.
	 */
	@Deprecated(forRemoval = true)
	public static boolean startsWith(final byte[] bytes, final byte[] prefix) {
		if(bytes.length < prefix.length) { //if the array of bytes is not as long as the specified prefix
			return false; //there aren't enough bytes to compare
		}
		for(int i = prefix.length - 1; i >= 0; --i) { //look at each of the prefix bytes
			if(bytes[i] != prefix[i]) { //if these bytes don't match
				return false; //show that there is a mismatch
			}
		}
		return true; //the array of bytes passed all the tests
	}

}
