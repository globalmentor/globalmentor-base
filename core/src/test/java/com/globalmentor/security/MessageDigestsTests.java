/*
 * Copyright © 2019 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import static com.globalmentor.java.Bytes.*;
import static com.globalmentor.java.Strings.NO_STRINGS;
import static com.globalmentor.security.MessageDigests.*;
import static java.nio.charset.StandardCharsets.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.security.*;

import org.junit.jupiter.api.*;

/**
 * Tests of {@link MessageDigests}.
 * @author Garret Wilson
 */
public class MessageDigestsTests {

	/** SHA-256 checksum string for <code>""</code>. */
	private final static String SHA_256_EMPTY_STRING_CHECKSUM = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855";

	/** A test string containing ISO-88591-1 range characters. */
	private final static String LATIN1_TEST = "touché";

	/** SHA-256 checksum string for {@value #LATIN1_TEST}. */
	private final static String SHA_256_LATIN1_TEST_CHECKSUM = "0b4212e38104d13a920f5196ecb648f1714d8a6d103578d0463932093e968b85";

	/**
	 * @see MessageDigests#checksum(MessageDigest, byte[])
	 * @see MessageDigests.Algorithm#checksum(byte[])
	 * @see MessageDigests#SHA3_256
	 */
	@Test
	void testChecksumBytes() throws NoSuchAlgorithmException {
		assertThat(MessageDigests.checksum(MessageDigest.getInstance(SHA_256.getName()), NO_BYTES), is(SHA_256_EMPTY_STRING_CHECKSUM));
		assertThat(MessageDigests.checksum(MessageDigest.getInstance(SHA_256.getName()), LATIN1_TEST.getBytes(UTF_8)), is(SHA_256_LATIN1_TEST_CHECKSUM));
		assertThat(SHA_256.checksum(NO_BYTES), is(SHA_256_EMPTY_STRING_CHECKSUM));
		assertThat(SHA_256.checksum(LATIN1_TEST.getBytes(UTF_8)), is(SHA_256_LATIN1_TEST_CHECKSUM));
	}

	/**
	 * @see MessageDigests#checksum(MessageDigest, CharSequence)
	 * @see MessageDigests.Algorithm#checksum(CharSequence)
	 * @see MessageDigests.Algorithm#checksum(char[])
	 * @see MessageDigests#SHA3_256
	 */
	@Test
	void testChecksumString() throws NoSuchAlgorithmException {
		assertThat(MessageDigests.checksum(MessageDigest.getInstance(SHA_256.getName()), ""), is(SHA_256_EMPTY_STRING_CHECKSUM));
		assertThat(MessageDigests.checksum(MessageDigest.getInstance(SHA_256.getName()), "".toCharArray()), is(SHA_256_EMPTY_STRING_CHECKSUM));
		assertThat(MessageDigests.checksum(MessageDigest.getInstance(SHA_256.getName()), LATIN1_TEST), is(SHA_256_LATIN1_TEST_CHECKSUM));
		assertThat(MessageDigests.checksum(MessageDigest.getInstance(SHA_256.getName()), LATIN1_TEST.toCharArray()), is(SHA_256_LATIN1_TEST_CHECKSUM));
		assertThat(SHA_256.checksum(""), is(SHA_256_EMPTY_STRING_CHECKSUM));
		assertThat(SHA_256.checksum("".toCharArray()), is(SHA_256_EMPTY_STRING_CHECKSUM));
		assertThat(SHA_256.checksum(LATIN1_TEST), is(SHA_256_LATIN1_TEST_CHECKSUM));
		assertThat(SHA_256.checksum(LATIN1_TEST.toCharArray()), is(SHA_256_LATIN1_TEST_CHECKSUM));
	}

	//## Algorithm

	/** @see MessageDigests.Algorithm#emptyHash() */
	@Test
	void testMessageDigestEmptyHash() {
		assertThat("Empty MD5 hash equivalent to hash of no strings.", MD5.emptyHash(), is(MD5.hash(NO_STRINGS)));
		assertThat("Empty SHA-256 hash equivalent to hash of no strings.", SHA_256.emptyHash(), is(SHA_256.hash(NO_STRINGS)));
		assertThat("Empty hash caching does not prevent second subsequent call.", SHA_256.emptyHash(), is(SHA_256.hash(NO_STRINGS)));
		assertThat("Empty MD5 hash not equivalent to SHA-256 empty hash.", MD5.emptyHash(), is(not(SHA_256.hash(NO_STRINGS))));
		assertThat("Empty hash caching does not prevent third subsequent call after empty hash for another algorithm.", SHA_256.emptyHash(),
				is(SHA_256.hash(NO_STRINGS)));
	}

	/**
	 * @see MessageDigests.Algorithm#isEmpty(Hash)
	 * @see MessageDigests.Algorithm#emptyHash()
	 */
	@Test
	void testMessageDigestIsEmptyHash() {
		assertThat("SHA-256 hash of no strings is considered empty.", SHA_256.isEmpty(SHA_256.hash(NO_STRINGS)), is(true));
		assertThat("Explicit empty SHA-256 hash is considered empty.", SHA_256.isEmpty(SHA_256.emptyHash()), is(true));
		assertThat("Non-empty hash is not considered empty.", SHA_256.isEmpty(SHA_256.hash("test")), is(false));
	}

	/**
	 * Tests [MessageDigests.Algorithm#equals(Object)] and [MessageDigests.Algorithm#hashCode()].
	 * @see MessageDigests.Algorithm#equals(Object)
	 * @see MessageDigests.Algorithm#hashCode()
	 */
	@SuppressWarnings("unlikely-arg-type")
	@Test
	void testAlgorithmEquality() {
		assertThat("Same algorithm instance is equal to itself", SHA_256.equals(SHA_256), is(true));
		assertThat("Different algorithms are not equal", SHA_256.equals(MD5), is(false));
		assertThat("Algorithm is not equal to non-algorithm", SHA_256.equals("SHA-256"), is(false));
		assertThat("Equal algorithms have equal hash codes", SHA_256.hashCode(), is(SHA_256.hashCode()));
	}

	/** Tests that [MessageDigests.Algorithm#toString()] returns the algorithm name. */
	@Test
	void testAlgorithmToString() {
		assertThat("toString returns algorithm name", SHA_256.toString(), is("SHA-256"));
		assertThat("toString returns algorithm name", MD5.toString(), is("MD5"));
	}

	/**
	 * Tests that [MessageDigests.Algorithm#digest(java.nio.charset.Charset, CharSequence...)] uses the specified charset.
	 * @see MessageDigests.Algorithm#digest(java.nio.charset.Charset, CharSequence...)
	 */
	@Test
	void testAlgorithmDigestWithCharset() {
		// "é" encodes differently in UTF-8 vs ISO-8859-1
		final byte[] digestUtf8 = SHA_256.digest(UTF_8, "é");
		final byte[] digestLatin1 = SHA_256.digest(ISO_8859_1, "é");
		assertThat("Charset affects digest result", digestUtf8, is(not(digestLatin1)));
		assertThat("UTF-8 digest matches expected", digestUtf8, is(SHA_256.digest("é".getBytes(UTF_8))));
		assertThat("ISO-8859-1 digest matches expected", digestLatin1, is(SHA_256.digest("é".getBytes(ISO_8859_1))));
	}

	/**
	 * Tests that [MessageDigests.Algorithm#hash(java.nio.charset.Charset, CharSequence...)] uses the specified charset.
	 * @see MessageDigests.Algorithm#hash(java.nio.charset.Charset, CharSequence...)
	 */
	@Test
	void testAlgorithmHashWithCharset() {
		// "é" encodes differently in UTF-8 vs ISO-8859-1
		final Hash hashUtf8 = SHA_256.hash(UTF_8, "é");
		final Hash hashLatin1 = SHA_256.hash(ISO_8859_1, "é");
		assertThat("Charset affects hash result", hashUtf8, is(not(hashLatin1)));
		assertThat("UTF-8 hash matches expected", hashUtf8, is(SHA_256.hash("é".getBytes(UTF_8))));
		assertThat("ISO-8859-1 hash matches expected", hashLatin1, is(SHA_256.hash("é".getBytes(ISO_8859_1))));
	}

	//## `update()` fluent API

	/** Tests that [MessageDigests#update(MessageDigest, byte[])] returns the same message digest for chaining. */
	@Test
	void testUpdateReturnsSameMessageDigest() {
		final MessageDigest messageDigest = SHA_256.newMessageDigest();
		assertThat("update returns same MessageDigest for chaining", update(messageDigest, new byte[] {0x01, 0x02}), is(sameInstance(messageDigest)));
	}

	//## `digest()` with multiple inputs

	/**
	 * Tests that [MessageDigests#digest(MessageDigest, CharSequence...)] concatenates character sequences in order.
	 * @see MessageDigests#digest(MessageDigest, CharSequence...)
	 */
	@Test
	void testDigestMultipleCharSequencesConcatenatesInOrder() {
		final byte[] digestAB = SHA_256.digest("foo", "bar");
		final byte[] digestCombined = SHA_256.digest("foobar");
		assertThat("Digesting multiple CharSequences concatenates them in order", digestAB, is(digestCombined));
	}

	//## `hash()` with hashes

	/**
	 * Tests that [MessageDigests#hash(MessageDigest, Hash...)] can compute a hash of hashes (Merkle tree pattern).
	 * @see MessageDigests#hash(MessageDigest, Hash...)
	 */
	@Test
	void testHashOfHashes() {
		final Hash hash1 = SHA_256.hash("one");
		final Hash hash2 = SHA_256.hash("two");
		final Hash combinedHash = SHA_256.hash(hash1, hash2);
		assertThat("Hash of hashes produces a valid hash", combinedHash, is(notNullValue()));
		assertThat("Hash of hashes differs from individual hashes", combinedHash, is(not(hash1)));
		assertThat("Hash of hashes differs from individual hashes", combinedHash, is(not(hash2)));
		assertThat("Hash of hashes is deterministic", SHA_256.hash(hash1, hash2), is(combinedHash)); // same order produces same result
		assertThat("Hash order matters", SHA_256.hash(hash2, hash1), is(not(combinedHash))); // different order produces different result
	}

	//## `ByteBuffer` support

	/**
	 * Tests [MessageDigests#hash(MessageDigest, ByteBuffer)].
	 * @see MessageDigests#hash(MessageDigest, ByteBuffer)
	 */
	@Test
	void testHashByteBuffer() {
		final byte[] bytes = "test".getBytes(UTF_8);
		final Hash hashFromBytes = SHA_256.hash(bytes);
		final Hash hashFromBuffer = SHA_256.hash(ByteBuffer.wrap(bytes));
		assertThat("Hash from ByteBuffer equals hash from byte array", hashFromBuffer, is(hashFromBytes));
	}

	//## `InputStream` support

	/**
	 * Tests [MessageDigests#hash(MessageDigest, java.io.InputStream)].
	 * @see MessageDigests#hash(MessageDigest, java.io.InputStream)
	 */
	@Test
	void testHashInputStream() throws IOException {
		final byte[] bytes = "test input stream content".getBytes(UTF_8);
		final Hash hashFromBytes = SHA_256.hash(bytes);
		final Hash hashFromStream = SHA_256.hash(new ByteArrayInputStream(bytes));
		assertThat("Hash from InputStream equals hash from byte array", hashFromStream, is(hashFromBytes));
	}

}
