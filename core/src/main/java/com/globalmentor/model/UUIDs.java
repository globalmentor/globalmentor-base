/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import java.net.URI;
import java.util.UUID;

import javax.annotation.*;

import com.globalmentor.java.Longs;

import static com.globalmentor.java.Conditions.*;
import static com.globalmentor.java.StringBuilders.*;
import static com.globalmentor.net.URIs.*;

/**
 * Utilities for manipulating a universally unique identifier (UUID).
 * @author Garret Wilson
 */
public final class UUIDs {

	private UUIDs() {
	}

	/** The number of bytes in a UUID. */
	public static final int BYTE_COUNT = 16;

	/** The UUID URN namespace identifier "uuid". */
	public static final String UUID_URN_NAMESPACE = "uuid";

	/**
	 * Creates a UUID from its raw binary representation in bytes.
	 * @apiNote This method requires exactly the number of bytes necessary. If some of the bytes should be ignored, use {@link #fromBytes(byte[], int)}.
	 * @implSpec This implementation delegates to {@link #fromBytes(byte[], int)}.
	 * @param bytes The bits of the UUID stored in big-endian order (most significant bits first).
	 * @return A UUID instance equivalent to the raw binary representation.
	 * @see UUID#getMostSignificantBits()
	 * @see UUID#getLeastSignificantBits()
	 */
	public static UUID fromBytes(@Nonnull final byte[] bytes) {
		checkArgument(bytes.length == BYTE_COUNT, "Exactly %d bytes are required to define a UUID.", BYTE_COUNT);
		return fromBytes(bytes, 0);
	}

	/**
	 * Creates a UUID from its raw binary representation in bytes.
	 * @param bytes The bits of the UUID stored in big-endian order (most significant bits first).
	 * @param index The initial index in the array at which the bytes are stored.
	 * @return A UUID instance equivalent to the raw binary representation.
	 * @see UUID#getMostSignificantBits()
	 * @see UUID#getLeastSignificantBits()
	 */
	public static UUID fromBytes(@Nonnull final byte[] bytes, final int index) {
		return new UUID(Longs.fromBytes(bytes, index), Longs.fromBytes(bytes, index + Long.BYTES));
	}

	/**
	 * Converts a UUID to its equivalent big-endian sequence of bytes.
	 * @implSpec This implementation delegates to {@link #toBytes(UUID, byte[])}.
	 * @param uuid The UUID to convert to bytes.
	 * @return The bytes equivalent to the UUID value, most-significant bytes first.
	 * @throws ArrayIndexOutOfBoundsException if the given array does not have sufficient bytes to store the UUID value.
	 * @see UUID#getMostSignificantBits()
	 * @see UUID#getLeastSignificantBits()
	 * @see #fromBytes(byte[])
	 */
	public static byte[] toBytes(@Nonnull final UUID uuid) {
		return toBytes(uuid, new byte[BYTE_COUNT]);
	}

	/**
	 * Converts a UUID to its equivalent big-endian sequence of bytes, storing the bytes in the given array.
	 * @implSpec This implementation delegates to {@link #toBytes(UUID, byte[], int)}.
	 * @param uuid The UUID to convert to bytes.
	 * @param bytes The array in which to store the bytes.
	 * @return The given array of bytes with the UUID stored.
	 * @throws ArrayIndexOutOfBoundsException if the given array does not have sufficient bytes to store the UUID value.
	 * @see UUID#getMostSignificantBits()
	 * @see UUID#getLeastSignificantBits()
	 * @see #fromBytes(byte[])
	 */
	public static byte[] toBytes(@Nonnull final UUID uuid, @Nonnull final byte[] bytes) {
		return toBytes(uuid, bytes, 0);
	}

	/**
	 * Converts a UUID to its equivalent big-endian sequence of bytes, storing the bytes in the given array at the given index.
	 * @param uuid The UUID to convert to bytes.
	 * @param bytes The array in which to store the bytes.
	 * @param index The index in the array at which to begin storing the bytes.
	 * @return The given array of bytes with the UUID stored.
	 * @throws ArrayIndexOutOfBoundsException if the given array, starting at the given index, does not have sufficient bytes to store the UUID value.
	 * @see UUID#getMostSignificantBits()
	 * @see UUID#getLeastSignificantBits()
	 * @see #fromBytes(byte[])
	 */
	public static byte[] toBytes(@Nonnull final UUID uuid, @Nonnull final byte[] bytes, final int index) {
		Longs.toBytes(uuid.getMostSignificantBits(), bytes, index);
		Longs.toBytes(uuid.getLeastSignificantBits(), bytes, index + Long.BYTES);
		return bytes;
	}

	/**
	 * Constructs a string of hexadecimal characters equivalent to the return value of {@link UUID#toString()} with all non-digits removed.
	 * @param uuid The UUID from which to construct a hex string.
	 * @return A pure hex string representing the UUID.
	 */
	public static String toHexString(@Nonnull final UUID uuid) {
		final StringBuilder hexStringBuilder = new StringBuilder(uuid.toString()); //create a string from the UUID
		return removeEvery(hexStringBuilder, '-').toString(); //remove all the hyphens and return the resulting string
	}

	/**
	 * Creates a URI from the UUID in the form <code>urn:uuid:<var>uuid</var></code>.
	 * @see <a href="http://www.ietf.org/rfc/rfc4122.txt">RFC 4122: A Universally Unique IDentifier (UUID) URN Namespace</a>
	 * @param uuid The UUID.
	 * @return A URI representing the UUID.
	 */
	public static URI toURI(@Nonnull final UUID uuid) {
		return createURN(UUID_URN_NAMESPACE, uuid.toString()); //construct an return the URN
	}

}
