/*
 * Copyright © 1996-2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import java.nio.charset.Charset;
import java.security.*;

import static com.globalmentor.io.Charsets.*;

import static com.globalmentor.java.Characters.*;

/**
 * Utility methods for working with message digests.
 * @author Garret Wilson
 */
public class MessageDigests {

	/** The MD5 digest algorithm. */
	public static final String MD5_ALGORITHM = "MD5";
	/** The SHA digest algorithm. */
	public static final String SHA_ALGORITHM = "SHA";

	/**
	 * Computes a digest for the given character sequences using the UTF-8 charset.
	 * @param messageDigest The implementation of a message digest algorithm.
	 * @param charSequences The character sequences to digest.
	 * @return The array of bytes for the resulting hash value.
	 */
	public static byte[] digest(final MessageDigest messageDigest, final CharSequence... charSequences) {
		return update(messageDigest, charSequences).digest(); //update the digest with the given character sequences and return the digest
	}

	/**
	 * Computes a digest for the given character sequences, using the given charset.
	 * @param messageDigest The implementation of a message digest algorithm.
	 * @param charset The charset to use when converting characters to bytes.
	 * @param charSequences The character sequences to digest.
	 * @return The array of bytes for the resulting hash value.
	 */
	public static byte[] digest(final MessageDigest messageDigest, final Charset charset, final CharSequence... charSequences) {
		return update(messageDigest, charset, charSequences).digest(); //update the digest from the character sequence's characters using the given charset and return the digest
	}

	/**
	 * Computes a digest for the given characters using the UTF-8 charset.
	 * @param messageDigest The implementation of a message digest algorithm.
	 * @param characters The characters to digest.
	 * @return The array of bytes for the resulting hash value.
	 */
	public static byte[] digest(final MessageDigest messageDigest, final char[] characters) {
		return digest(messageDigest, UTF_8_CHARSET, characters); //digest the characters using UTF-8
	}

	/**
	 * Computes a digest for the given characters, using the given charset.
	 * @param messageDigest The implementation of a message digest algorithm.
	 * @param charset The charset to use when converting characters to bytes.
	 * @param characters The arrays of characters to digest.
	 * @return The array of bytes for the resulting hash value.
	 */
	public static byte[] digest(final MessageDigest messageDigest, final Charset charset, final char[] characters) {
		final byte[] bytes = toByteArray(characters, charset); //convert the characters to bytes
		return messageDigest.digest(bytes); //calculate and return the digest
	}

	/**
	 * Updates a digest from the given character sequences using the UTF-8 charset.
	 * @param messageDigest The implementation of a message digest algorithm.
	 * @param charSequences The character sequences to digest.
	 * @return The message digest.
	 */
	public static MessageDigest update(final MessageDigest messageDigest, final CharSequence... charSequences) {
		return update(messageDigest, UTF_8_CHARSET, charSequences); //update the digest using UTF-8
	}

	/**
	 * Updates a digest from given character sequences, using the given charset.
	 * @param messageDigest The implementation of a message digest algorithm.
	 * @param charset The charset to use when converting characters to bytes.
	 * @param charSequences The character sequences to digest.
	 * @return The message digest.
	 */
	public static MessageDigest update(final MessageDigest messageDigest, final Charset charset, final CharSequence... charSequences) {
		for(final CharSequence charSequence : charSequences) { //for each character sequence
			update(messageDigest, charset, charSequence); //update the digest from the character sequence using the given charset
		}
		return messageDigest; //return the message digest
	}

	/**
	 * Updates a digest with the given characters using the UTF-8 charset.
	 * @param messageDigest The implementation of a message digest algorithm.
	 * @param characters The characters to digest.
	 * @return The message digest.
	 */
	public static MessageDigest update(final MessageDigest messageDigest, final char[] characters) {
		return update(messageDigest, UTF_8_CHARSET, characters); //update the digest using UTF-8
	}

	/**
	 * Updates a digest with the given character sequence, using the given charset.
	 * @param messageDigest The implementation of a message digest algorithm.
	 * @param charset The charset to use when converting characters to bytes.
	 * @param charSequence The character sequence to digest.
	 * @return The message digest.
	 */
	public static MessageDigest update(final MessageDigest messageDigest, final Charset charset, final CharSequence charSequence) {
		final byte[] bytes = charSequence.toString().getBytes(charset); //convert the characters to bytes
		messageDigest.update(bytes); //update the digest
		return messageDigest; //return the message digest
	}

	/**
	 * Updates a digest with the given characters, using the given charset.
	 * @param messageDigest The implementation of a message digest algorithm.
	 * @param charset The charset to use when converting characters to bytes.
	 * @param characters The arrays of characters to digest.
	 * @return The message digest.
	 */
	public static MessageDigest update(final MessageDigest messageDigest, final Charset charset, final char[] characters) {
		final byte[] bytes = toByteArray(characters, charset); //convert the characters to bytes
		messageDigest.update(bytes); //update the digest
		return messageDigest; //return the message digest
	}

}
