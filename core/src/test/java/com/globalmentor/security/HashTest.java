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

package com.globalmentor.security;

import static com.globalmentor.security.MessageDigests.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.security.*;
import java.util.Arrays;

import org.junit.jupiter.api.Test;

import com.globalmentor.java.ByteSequence;

/**
 * Tests for {@link Hash}.
 */
public class HashTest {

	//## Factory methods

	/** Tests for {@link Hash#of(byte[])}. */
	@Test
	void testOf() {
		final byte[] original = {0x01, 0x02, 0x03};
		final Hash hash = Hash.of(original);
		assertThat("hash has correct length", hash.length(), is(3));
		assertThat("hash toByteArray equals original", hash.toByteArray(), is(original));
		original[0] = 0x00; // modify original to prove defensive copy
		assertThat("hash is independent of original", hash.byteAt(0), is((byte)0x01));
	}

	/** Tests for {@link Hash#fromChecksum(CharSequence)}. */
	@Test
	void testFromChecksum() {
		final Hash hash = Hash.fromChecksum("0102030405");
		assertThat("fromChecksum length", hash.length(), is(5));
		assertThat("fromChecksum bytes", hash.toByteArray(), is(new byte[] {0x01, 0x02, 0x03, 0x04, 0x05}));
	}

	/** Tests for {@link Hash#fromDigest(MessageDigest)}. */
	@Test
	void testFromDigest() throws NoSuchAlgorithmException {
		final MessageDigest md = MessageDigest.getInstance(SHA_256.getName());
		md.update("test".getBytes());
		final Hash hash = Hash.fromDigest(md);
		assertThat("fromDigest creates hash", hash.length(), is(32)); // SHA-256 produces 32 bytes
	}

	//## ByteSequence behavior

	/** Tests that {@link Hash} implements {@link ByteSequence}. */
	@Test
	void testIsInstanceOfByteSequence() {
		final Hash hash = Hash.of(new byte[] {0x01, 0x02, 0x03});
		assertThat("Hash is a ByteSequence", hash, is(instanceOf(ByteSequence.class)));
	}

	/** Tests for {@link Hash#length()}. */
	@Test
	void testLength() {
		assertThat("length of hash", Hash.of(new byte[] {0x01, 0x02, 0x03, 0x04}).length(), is(4));
	}

	/** Tests for {@link Hash#byteAt(int)}. */
	@Test
	void testByteAt() {
		final Hash hash = Hash.of(new byte[] {0x10, 0x20, 0x30});
		assertThat("byteAt(0)", hash.byteAt(0), is((byte)0x10));
		assertThat("byteAt(1)", hash.byteAt(1), is((byte)0x20));
		assertThat("byteAt(2)", hash.byteAt(2), is((byte)0x30));
	}

	/** Tests for {@link Hash#toByteArray()}. */
	@Test
	void testToByteArray() {
		final byte[] original = {0x0A, 0x0B, 0x0C};
		final Hash hash = Hash.of(original);
		final byte[] result = hash.toByteArray();
		assertThat("toByteArray equals original", result, is(original));
		result[0] = 0x00; // modify to prove defensive copy
		assertThat("toByteArray returns defensive copy", hash.byteAt(0), is((byte)0x0A));
	}

	/** Tests for deprecated {@link Hash#getBytes()}. */
	@SuppressWarnings("removal")
	@Test
	void testGetBytes() {
		final byte[] original = {0x01, 0x02, 0x03};
		final Hash hash = Hash.of(original);
		assertThat("getBytes equals toByteArray", hash.getBytes(), is(hash.toByteArray()));
	}

	//## Hash-specific methods

	/** Tests for {@link Hash#toChecksum()}. */
	@Test
	void testToChecksum() {
		final Hash hash = Hash.of(new byte[] {0x00, 0x0A, 0x0F, (byte)0xFF});
		assertThat("toChecksum lowercase hex", hash.toChecksum(), is("000a0fff"));
	}

	/** Tests for {@link Hash#toString()}. */
	@Test
	void testToString() {
		final Hash hash = Hash.of(new byte[] {0x01, 0x02, 0x03});
		assertThat("toString delegates to toChecksum", hash.toString(), is(hash.toChecksum()));
	}

	/** Tests for {@link Hash#updateMessageDigest(MessageDigest)}}. */
	@Test
	void testUpdateMessageDigest() throws NoSuchAlgorithmException {
		final Hash hash = Hash.of(new byte[] {0x01, 0x02, 0x03});
		final MessageDigest md = MessageDigest.getInstance(SHA_256.getName());
		final MessageDigest result = hash.updateMessageDigest(md);
		assertThat("updateMessageDigest returns same MessageDigest", result, is(sameInstance(md)));
		// digest was updated, so digesting should produce non-empty result
		assertThat("digest was updated", md.digest().length, is(32));
	}

	//## Equality and hash code

	/** Tests for {@link Hash#equals(Object)}. */
	@Test
	void testEquals() {
		final Hash hash1 = Hash.of(new byte[] {0x01, 0x02, 0x03});
		final Hash hash2 = Hash.of(new byte[] {0x01, 0x02, 0x03});
		final Hash hash3 = Hash.fromChecksum("010203");
		final Hash different = Hash.of(new byte[] {0x01, 0x02, 0x04});
		assertThat("equals self", hash1.equals(hash1), is(true));
		assertThat("equals equivalent Hash", hash1.equals(hash2), is(true));
		assertThat("equals Hash from checksum", hash1.equals(hash3), is(true));
		assertThat("not equals different content", hash1.equals(different), is(false));
		assertThat("not equals null", hash1.equals(null), is(false));
	}

	/** Tests that {@link Hash} equals other {@link ByteSequence} implementations with same bytes. */
	@Test
	void testEqualsCrossImplementation() {
		final Hash hash = Hash.of(new byte[] {0x01, 0x02, 0x03});
		final ByteSequence bs = ByteSequence.copyOf(new byte[] {0x01, 0x02, 0x03});
		assertThat("Hash equals ByteSequence with same bytes", hash.equals(bs), is(true));
		assertThat("ByteSequence equals Hash with same bytes", bs.equals(hash), is(true));
	}

	/** Tests for {@link Hash#hashCode()}. */
	@Test
	void testHashCode() {
		final byte[] data = {0x01, 0x02, 0x03};
		final Hash hash = Hash.of(data);
		assertThat("hashCode equals Arrays.hashCode", hash.hashCode(), is(Arrays.hashCode(data)));
	}

	/** Tests that equal hashes have equal hash codes. */
	@Test
	void testEqualsHashCodeContract() {
		final Hash hash1 = Hash.of(new byte[] {0x10, 0x20, 0x30});
		final Hash hash2 = Hash.fromChecksum("102030");
		assertThat("equal hashes", hash1.equals(hash2), is(true));
		assertThat("equal hashes have equal hashCode", hash1.hashCode(), is(hash2.hashCode()));
	}

	//## ByteSequence comparison methods inherited

	/** Tests that {@link Hash} inherits comparison methods from {@link ByteSequence}. */
	@Test
	void testStartsWith() {
		final Hash hash = Hash.of(new byte[] {0x01, 0x02, 0x03, 0x04});
		assertThat("hash starts with prefix", hash.startsWith(new byte[] {0x01, 0x02}), is(true));
		assertThat("hash does not start with non-prefix", hash.startsWith(new byte[] {0x02, 0x03}), is(false));
	}

	/** Tests that {@link Hash} inherits {@link ByteSequence#equalsBytes(byte[])}. */
	@Test
	void testEqualsBytes() {
		final Hash hash = Hash.of(new byte[] {0x01, 0x02, 0x03});
		assertThat("hash equalsBytes same content", hash.equalsBytes(new byte[] {0x01, 0x02, 0x03}), is(true));
		assertThat("hash not equalsBytes different content", hash.equalsBytes(new byte[] {0x01, 0x02, 0x04}), is(false));
	}

	/** Tests for {@link Hash#isEmpty()}. */
	@Test
	void testIsEmpty() {
		assertThat("empty hash", Hash.of(new byte[] {}).isEmpty(), is(true));
		assertThat("non-empty hash", Hash.of(new byte[] {0x01}).isEmpty(), is(false));
	}

	/** Tests for {@link Hash#subSequence(int, int)}. */
	@Test
	void testSubSequence() {
		final Hash hash = Hash.of(new byte[] {0x10, 0x20, 0x30, 0x40, 0x50});
		final ByteSequence sub = hash.subSequence(1, 4);
		assertThat("subSequence length", sub.length(), is(3));
		assertThat("subSequence content", sub.toByteArray(), is(new byte[] {0x20, 0x30, 0x40}));
	}

	//## MessageDigests integration

	/** Tests for {@link MessageDigests#hash(MessageDigest, byte[])}. */
	@Test
	void testHashFromMessageDigests() throws NoSuchAlgorithmException {
		final Hash hash = hash(SHA_256.newMessageDigest(), "test".getBytes());
		assertThat("hash from MessageDigests", hash.length(), is(32)); // SHA-256 produces 32 bytes
		assertThat("hash is instance of Hash", hash, is(instanceOf(Hash.class)));
	}

	/** Tests for {@link MessageDigests#hash(MessageDigest, CharSequence...)}. */
	@Test
	void testHashFromStrings() throws NoSuchAlgorithmException {
		final Hash hash1 = hash(SHA_256.newMessageDigest(), "hello", "world");
		final Hash hash2 = hash(SHA_256.newMessageDigest(), "helloworld");
		assertThat("hash of concatenated strings", hash1, is(hash2));
	}

	/** Tests that hashing empty input produces valid hash. */
	@Test
	void testHashEmpty() throws NoSuchAlgorithmException {
		final Hash emptyHash = hash(SHA_256.newMessageDigest(), new byte[] {});
		assertThat("empty hash has correct length", emptyHash.length(), is(32));
		assertThat("empty hash checksum matches expected", emptyHash.toChecksum(), is("e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"));
	}

	/** Tests for {@link MessageDigests#hash(MessageDigest, Hash...)} - hashing hashes. */
	@Test
	void testHashOfHashes() throws NoSuchAlgorithmException {
		final Hash hash1 = Hash.of(new byte[] {0x01, 0x02});
		final Hash hash2 = Hash.of(new byte[] {0x03, 0x04});
		final Hash combined = hash(SHA_256.newMessageDigest(), hash1, hash2);
		assertThat("combined hash length", combined.length(), is(32));
		// verify it's not just concatenation but actual hashing
		assertThat("combined hash is different from inputs", combined, is(not(hash1)));
		assertThat("combined hash is different from inputs", combined, is(not(hash2)));
	}

	/** Tests that {@link Hash#updateMessageDigest(MessageDigest)} allows hash chaining. */
	@Test
	void testHashChaining() throws NoSuchAlgorithmException {
		final Hash hash1 = Hash.of(new byte[] {0x01, 0x02, 0x03});
		final Hash hash2 = Hash.of(new byte[] {0x04, 0x05, 0x06});
		final MessageDigest md = MessageDigest.getInstance(SHA_256.getName());
		hash1.updateMessageDigest(md);
		hash2.updateMessageDigest(md);
		final Hash chainedHash = Hash.fromDigest(md);
		// should equal hashing both together
		final Hash directHash = hash(SHA_256.newMessageDigest(), hash1, hash2);
		assertThat("chained hash equals direct hash", chainedHash, is(directHash));
	}

	//## Immutability

	/** Tests that modifying source array after {@link Hash#of(byte[])} does not affect hash. */
	@Test
	void testImmutabilityOfCreation() {
		final byte[] original = {0x01, 0x02, 0x03};
		final Hash hash = Hash.of(original);
		original[0] = (byte)0xFF; // modify source
		assertThat("hash unaffected by source modification", hash.byteAt(0), is((byte)0x01));
	}

	/** Tests that modifying returned array from {@link Hash#toByteArray()} does not affect hash. */
	@Test
	void testImmutabilityOfToByteArray() {
		final Hash hash = Hash.of(new byte[] {0x01, 0x02, 0x03});
		final byte[] array = hash.toByteArray();
		array[0] = (byte)0xFF; // modify returned array
		assertThat("hash unaffected by returned array modification", hash.byteAt(0), is((byte)0x01));
	}

	//## ByteSequence contract

	/** Tests for {@link ByteSequence#compare(ByteSequence, ByteSequence)}. */
	@Test
	void testCompare() {
		final Hash hash1 = Hash.of(new byte[] {0x01, 0x02});
		final Hash hash2 = Hash.of(new byte[] {0x01, 0x03});
		final Hash hash3 = Hash.of(new byte[] {0x01, 0x02, 0x03});
		assertThat("compare less than", ByteSequence.compare(hash1, hash2), is(lessThan(0)));
		assertThat("compare greater than", ByteSequence.compare(hash2, hash1), is(greaterThan(0)));
		assertThat("compare equal", ByteSequence.compare(hash1, hash1), is(0));
		assertThat("shorter sequence is less", ByteSequence.compare(hash1, hash3), is(lessThan(0)));
	}

	/** Tests for {@link Hash#endsWith(byte[])}. */
	@Test
	void testEndsWith() {
		final Hash hash = Hash.of(new byte[] {0x01, 0x02, 0x03, 0x04});
		assertThat("hash ends with suffix", hash.endsWith(new byte[] {0x03, 0x04}), is(true));
		assertThat("hash does not end with non-suffix", hash.endsWith(new byte[] {0x02, 0x03}), is(false));
	}

	/** Tests for {@link Hash#isPrefixOf(byte[])}. */
	@Test
	void testIsPrefixOf() {
		final Hash hash = Hash.of(new byte[] {0x01, 0x02});
		assertThat("hash is prefix of longer array", hash.isPrefixOf(new byte[] {0x01, 0x02, 0x03}), is(true));
		assertThat("hash is not prefix of different array", hash.isPrefixOf(new byte[] {0x03, 0x04}), is(false));
	}

	/** Tests for {@link Hash#isSuffixOf(byte[])}. */
	@Test
	void testIsSuffixOf() {
		final Hash hash = Hash.of(new byte[] {0x03, 0x04});
		assertThat("hash is suffix of longer array", hash.isSuffixOf(new byte[] {0x01, 0x02, 0x03, 0x04}), is(true));
		assertThat("hash is not suffix of different array", hash.isSuffixOf(new byte[] {0x01, 0x02}), is(false));
	}

	/** Tests for {@link Hash#bytes()}. */
	@Test
	void testBytes() {
		final Hash hash = Hash.of(new byte[] {(byte)0xFF, 0x00, 0x7F});
		final int[] bytes = hash.bytes().toArray();
		assertThat("bytes stream length", bytes.length, is(3));
		assertThat("unsigned byte conversion", bytes[0], is(255)); // 0xFF as unsigned
		assertThat("unsigned byte conversion", bytes[1], is(0));
		assertThat("unsigned byte conversion", bytes[2], is(127));
	}

}
