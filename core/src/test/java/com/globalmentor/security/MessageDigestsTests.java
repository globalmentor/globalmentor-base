/*
 * Copyright © 2019 GlobalMentor, Inc. <http://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.security;

import static com.globalmentor.java.Bytes.*;
import static com.globalmentor.security.MessageDigests.*;
import static java.nio.charset.StandardCharsets.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

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
	public void testChecksumBytes() throws NoSuchAlgorithmException {
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
	public void testChecksumString() throws NoSuchAlgorithmException {
		assertThat(MessageDigests.checksum(MessageDigest.getInstance(SHA_256.getName()), ""), is(SHA_256_EMPTY_STRING_CHECKSUM));
		assertThat(MessageDigests.checksum(MessageDigest.getInstance(SHA_256.getName()), "".toCharArray()), is(SHA_256_EMPTY_STRING_CHECKSUM));
		assertThat(MessageDigests.checksum(MessageDigest.getInstance(SHA_256.getName()), LATIN1_TEST), is(SHA_256_LATIN1_TEST_CHECKSUM));
		assertThat(MessageDigests.checksum(MessageDigest.getInstance(SHA_256.getName()), LATIN1_TEST.toCharArray()), is(SHA_256_LATIN1_TEST_CHECKSUM));
		assertThat(SHA_256.checksum(""), is(SHA_256_EMPTY_STRING_CHECKSUM));
		assertThat(SHA_256.checksum("".toCharArray()), is(SHA_256_EMPTY_STRING_CHECKSUM));
		assertThat(SHA_256.checksum(LATIN1_TEST), is(SHA_256_LATIN1_TEST_CHECKSUM));
		assertThat(SHA_256.checksum(LATIN1_TEST.toCharArray()), is(SHA_256_LATIN1_TEST_CHECKSUM));
	}

}
