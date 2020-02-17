/*
 * Copyright © 1996-2019 GlobalMentor, Inc. <http://www.globalmentor.com/>
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
import java.nio.file.Path;
import java.security.*;

import javax.annotation.*;

import com.globalmentor.java.Bytes;
import com.globalmentor.model.Named;

import static com.globalmentor.java.Bytes.*;
import static com.globalmentor.java.Characters.*;
import static java.nio.charset.StandardCharsets.*;
import static java.nio.file.Files.*;
import static java.util.Objects.*;

import java.io.*;

/**
 * Utility methods for working with message digests.
 * @apiNote The standard algorithms enumerated by this class identify known algorithms with standard names. Not all of them are guaranteed to be implemented on
 *          any particular Java platform.
 * @author Garret Wilson
 * @see <a href="https://docs.oracle.com/en/java/javase/11/docs/specs/security/standard-names.html#messagedigest-algorithms"><code>MessageDigest</code>
 *      Algorithms</a>
 */
public class MessageDigests {

	/** The MD2 message digest algorithm as defined in <a href="https://tools.ietf.org/html/rfc1319">RFC 1319</a>. */
	public static final Algorithm MD2 = new Algorithm("MD2");

	/**
	 * The MD5 message digest algorithm as defined in <a href="https://tools.ietf.org/html/rfc1321">RFC 1321</a>. Guaranteed to be supported by every Java
	 * platform implementation as of Java 7.
	 */
	public static final Algorithm MD5 = new Algorithm("MD5");

	/**
	 * A hash algorithms defined in <a href="https://csrc.nist.gov/publications/fips/fips180-4/fips-180-4.pdf">FIPS PUB 180-4</a>. Guaranteed to be supported by
	 * every Java platform implementation as of Java 7.
	 */
	public static final Algorithm SHA_1 = new Algorithm("SHA-1");

	/** A hash algorithms defined in <a href="https://csrc.nist.gov/publications/fips/fips180-4/fips-180-4.pdf">FIPS PUB 180-4</a>. */
	public static final Algorithm SHA_224 = new Algorithm("SHA-224");

	/**
	 * A hash algorithms defined in <a href="https://csrc.nist.gov/publications/fips/fips180-4/fips-180-4.pdf">FIPS PUB 180-4</a>. Guaranteed to be supported by
	 * every Java platform implementation as of Java 7.
	 */
	public static final Algorithm SHA_256 = new Algorithm("SHA-256");

	/** A hash algorithms defined in <a href="https://csrc.nist.gov/publications/fips/fips180-4/fips-180-4.pdf">FIPS PUB 180-4</a>. */
	public static final Algorithm SHA_384 = new Algorithm("SHA-384");

	/** A hash algorithms defined in <a href="https://csrc.nist.gov/publications/fips/fips180-4/fips-180-4.pdf">FIPS PUB 180-4</a>. */
	public static final Algorithm SHA_512_224 = new Algorithm("SHA-512/224");

	/** A hash algorithms defined in <a href="https://csrc.nist.gov/publications/fips/fips180-4/fips-180-4.pdf">FIPS PUB 180-4</a>. */
	public static final Algorithm SHA_512_256 = new Algorithm("SHA-512/256");

	/**
	 * Permutation-based hash and extendable-output functions as defined in <a href="https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.202.pdf">FIPS PUB 202</a>
	 * producing a 224 bit digest.
	 */
	public static final Algorithm SHA3_224 = new Algorithm("SHA3-224");

	/**
	 * Permutation-based hash and extendable-output functions as defined in <a href="https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.202.pdf">FIPS PUB 202</a>
	 * producing a 256 bit digest.
	 */
	public static final Algorithm SHA3_256 = new Algorithm("SHA3-256");

	/**
	 * Permutation-based hash and extendable-output functions as defined in <a href="https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.202.pdf">FIPS PUB 202</a>
	 * producing a 384 bit digest.
	 */
	public static final Algorithm SHA3_384 = new Algorithm("SHA3-384");

	/**
	 * Permutation-based hash and extendable-output functions as defined in <a href="https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.202.pdf">FIPS PUB 202</a>
	 * producing a 512 bit digest.
	 */
	public static final Algorithm SHA3_512 = new Algorithm("SHA3-512");

	/**
	 * Computes a digest for the given character sequences using the UTF-8 charset.
	 * @param messageDigest The implementation of a message digest algorithm.
	 * @param charSequences The character sequences to digest.
	 * @return The array of bytes for the resulting hash value.
	 */
	public static byte[] digest(@Nonnull final MessageDigest messageDigest, @Nonnull final CharSequence... charSequences) {
		return update(messageDigest, charSequences).digest(); //update the digest with the given character sequences and return the digest
	}

	/**
	 * Computes a digest for the given character sequences, using the given charset.
	 * @param messageDigest The implementation of a message digest algorithm.
	 * @param charset The charset to use when converting characters to bytes.
	 * @param charSequences The character sequences to digest.
	 * @return The array of bytes for the resulting hash value.
	 */
	public static byte[] digest(@Nonnull final MessageDigest messageDigest, @Nonnull final Charset charset, @Nonnull final CharSequence... charSequences) {
		return update(messageDigest, charset, charSequences).digest(); //update the digest from the character sequence's characters using the given charset and return the digest
	}

	/**
	 * Computes a digest for the given characters using the UTF-8 charset.
	 * @param messageDigest The implementation of a message digest algorithm.
	 * @param characters The characters to digest.
	 * @return The array of bytes for the resulting hash value.
	 */
	public static byte[] digest(@Nonnull final MessageDigest messageDigest, @Nonnull final char[] characters) {
		return digest(messageDigest, UTF_8, characters); //digest the characters using UTF-8
	}

	/**
	 * Computes a digest for the given characters, using the given charset.
	 * @param messageDigest The implementation of a message digest algorithm.
	 * @param charset The charset to use when converting characters to bytes.
	 * @param characters The arrays of characters to digest.
	 * @return The array of bytes for the resulting hash value.
	 */
	public static byte[] digest(@Nonnull final MessageDigest messageDigest, @Nonnull final Charset charset, @Nonnull final char[] characters) {
		final byte[] bytes = toByteArray(characters, charset); //convert the characters to bytes
		return messageDigest.digest(bytes); //calculate and return the digest
	}

	/**
	 * Computes a digest from the contents of the given input stream. All the remaining contents of the input stream are consumed.
	 * @param messageDigest The implementation of a message digest algorithm.
	 * @param inputStream The input stream on which to perform a digest.
	 * @return The array of bytes for the resulting hash value.
	 * @throws IOException if there is an I/O exception reading from the input stream.
	 */
	public static byte[] digest(@Nonnull final MessageDigest messageDigest, @Nonnull final InputStream inputStream) throws IOException {
		return update(messageDigest, inputStream).digest();
	}

	/**
	 * Computes a digest from the contents of the given file.
	 * @param messageDigest The implementation of a message digest algorithm.
	 * @param file The path of the file on which to perform a digest.
	 * @return The array of bytes for the resulting hash value.
	 * @throws IOException if there is an I/O exception reading from the file.
	 */
	public static byte[] digest(@Nonnull final MessageDigest messageDigest, @Nonnull final Path file) throws IOException {
		return update(messageDigest, file).digest();
	}

	/**
	 * Updates a digest from the given character sequences using the UTF-8 charset.
	 * @param messageDigest The implementation of a message digest algorithm.
	 * @param charSequences The character sequences to digest.
	 * @return The message digest.
	 */
	public static MessageDigest update(@Nonnull final MessageDigest messageDigest, @Nonnull final CharSequence... charSequences) {
		return update(messageDigest, UTF_8, charSequences); //update the digest using UTF-8
	}

	/**
	 * Updates a digest from given character sequences, using the given charset.
	 * @param messageDigest The implementation of a message digest algorithm.
	 * @param charset The charset to use when converting characters to bytes.
	 * @param charSequences The character sequences to digest.
	 * @return The message digest.
	 */
	public static MessageDigest update(@Nonnull final MessageDigest messageDigest, @Nonnull final Charset charset, @Nonnull final CharSequence... charSequences) {
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
	public static MessageDigest update(@Nonnull final MessageDigest messageDigest, @Nonnull final char[] characters) {
		return update(messageDigest, UTF_8, characters); //update the digest using UTF-8
	}

	/**
	 * Updates a digest with the given character sequence, using the given charset.
	 * @param messageDigest The implementation of a message digest algorithm.
	 * @param charset The charset to use when converting characters to bytes.
	 * @param charSequence The character sequence to digest.
	 * @return The message digest.
	 */
	public static MessageDigest update(@Nonnull final MessageDigest messageDigest, @Nonnull final Charset charset, @Nonnull final CharSequence charSequence) {
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
	public static MessageDigest update(@Nonnull final MessageDigest messageDigest, @Nonnull final Charset charset, @Nonnull final char[] characters) {
		final byte[] bytes = toByteArray(characters, charset); //convert the characters to bytes
		messageDigest.update(bytes); //update the digest
		return messageDigest; //return the message digest
	}

	/**
	 * Updates a digest with the contents of the given input stream. All the remaining contents of the input stream are consumed.
	 * @param messageDigest The implementation of a message digest algorithm.
	 * @param inputStream The input stream on which to perform a digest.
	 * @return The message digest.
	 * @throws IOException if there is an I/O exception reading from the input stream.
	 */
	public static MessageDigest update(@Nonnull final MessageDigest messageDigest, @Nonnull final InputStream inputStream) throws IOException {
		final byte[] buffer = new byte[1 << 13]; //compare with BufferedInputStream, which uses a 8192 byte buffer as of Java 11
		int readCount;
		while((readCount = inputStream.read(buffer)) != -1) {
			messageDigest.update(buffer, 0, readCount);
		}
		return messageDigest;
	}

	/**
	 * Updates a digest with the contents of the given file.
	 * @param messageDigest The implementation of a message digest algorithm.
	 * @param file The path of the file on which to perform a digest.
	 * @return The message digest.
	 * @throws IOException if there is an I/O exception reading from the file.
	 */
	public static MessageDigest update(@Nonnull final MessageDigest messageDigest, @Nonnull final Path file) throws IOException {
		try (final InputStream inputStream = newInputStream(file)) { //our message digest utility will do its own buffering
			return update(messageDigest, inputStream);
		}
	}

	/**
	 * Computes a lowercase hex checksum string for the given input bytes.
	 * @implSpec This implementation calls {@link MessageDigest#digest(byte[])}.
	 * @apiNote This method considers a <dfn>checksum</dfn> to be a string version of a message <dfn>digest</dfn>, as the former is often used in the context of
	 *          file contents verification.
	 * @param messageDigest The implementation of a message digest algorithm.
	 * @param input The sequence of bytes for which a digest and then a checksum string should be created.
	 * @return The lowercase hex checksum string of the resulting hash value.
	 * @see MessageDigest#digest(byte[])
	 */
	public static String checksum(@Nonnull final MessageDigest messageDigest, @Nonnull final byte[] input) {
		return toHexString(messageDigest.digest(input));
	}

	/**
	 * Computes a lowercase hex checksum string for the given character sequence using the UTF-8 charset.
	 * @implSpec This implementation delegates to {@link #digest(MessageDigest, CharSequence...)}.
	 * @apiNote This method considers a <dfn>checksum</dfn> to be a string version of a message <dfn>digest</dfn>, as the former is often used in the context of
	 *          file contents verification.
	 * @param messageDigest The implementation of a message digest algorithm.
	 * @param charSequence The character sequence for which a checksum should be created.
	 * @return The lowercase hex checksum string of the resulting hash value.
	 */
	public static String checksum(@Nonnull final MessageDigest messageDigest, @Nonnull final CharSequence charSequence) {
		return toHexString(digest(messageDigest, charSequence));
	}

	/**
	 * Computes a lowercase hex checksum string for the given characters using the UTF-8 charset.
	 * @implSpec This implementation delegates to {@link #digest(MessageDigest, char[])}.
	 * @apiNote This method considers a <dfn>checksum</dfn> to be a string version of a message <dfn>digest</dfn>, as the former is often used in the context of
	 *          file contents verification.
	 * @param messageDigest The implementation of a message digest algorithm.
	 * @param characters The characters for which a checksum should be created.
	 * @return The lowercase hex checksum string of the resulting hash value.
	 * @see Bytes#toHexString(byte[])
	 */
	public static String checksum(@Nonnull final MessageDigest messageDigest, @Nonnull final char[] characters) {
		return toHexString(digest(messageDigest, characters));
	}

	/**
	 * Computes a lowercase hex checksum string for the contents of the given input stream. All the remaining contents of the input stream are consumed.
	 * @implSpect This implementation delegates to {@link #digest(MessageDigest, InputStream)}
	 * @apiNote This method considers a <dfn>checksum</dfn> to be a string version of a message <dfn>digest</dfn>, as the former is often used in the context of
	 *          file contents verification.
	 * @param messageDigest The implementation of a message digest algorithm.
	 * @param inputStream The input stream on which to perform a digest.
	 * @return The lowercase hex checksum string of the resulting hash value.
	 * @throws IOException if there is an I/O exception reading from the input stream.
	 */
	public static String checksum(@Nonnull final MessageDigest messageDigest, @Nonnull final InputStream inputStream) throws IOException {
		return toHexString(digest(messageDigest, inputStream));
	}

	/**
	 * Computes a lowercase hex checksum string for the contents of the given file.
	 * @implSpect This implementation delegates to {@link #digest(MessageDigest, Path)}
	 * @apiNote This method considers a <dfn>checksum</dfn> to be a string version of a message <dfn>digest</dfn>, as the former is often used in the context of
	 *          file contents verification.
	 * @param messageDigest The implementation of a message digest algorithm.
	 * @param file The path to the file on which to perform a digest.
	 * @return The lowercase hex checksum string of the resulting hash value.
	 * @throws IOException if there is an I/O exception reading from the file.
	 */
	public static String checksum(@Nonnull final MessageDigest messageDigest, @Nonnull final Path file) throws IOException {
		return toHexString(digest(messageDigest, file));
	}

	/**
	 * Encapsulation of a standard {@link MessageDigest} algorithm, which may or may not be implemented by any particular Java platform.
	 * @author Garret Wilson
	 * @see <a href="https://docs.oracle.com/en/java/javase/11/docs/specs/security/standard-names.html#messagedigest-algorithms"><code>MessageDigest</code>
	 *      Algorithms</a>
	 */
	public final static class Algorithm implements Named<String> {

		private final String name;

		/**
		 * Name constructor.
		 * @param name The algorithm name.
		 */
		private Algorithm(@Nonnull final String name) {
			this.name = requireNonNull(name);
		}

		@Override
		public String getName() {
			return name;
		}

		/**
		 * Convenience method to get an instance of this message digest algorithm.
		 * @apiNote This method differs from {@link MessageDigest#getInstance(String)} in that it throws an unchecked exception if the algorithm is not supported.
		 * @return An instance of this message digest algorithm.
		 * @throws RuntimeException if no {@link Provider} supports a {@link MessageDigestSpi} implementation for this algorithm.
		 * @see MessageDigest#getInstance(String)
		 */
		public MessageDigest getInstance() {
			try {
				return MessageDigest.getInstance(getName());
			} catch(final NoSuchAlgorithmException noSuchAlgorithmException) {
				throw new RuntimeException(noSuchAlgorithmException);
			}
		}

		/**
		 * Computes a digest using this algorithm for the given character sequences using the UTF-8 charset.
		 * @implSpec This convenience method delegates to {@link MessageDigests#digest(MessageDigest, CharSequence...)}.
		 * @param charSequences The character sequences to digest.
		 * @return The array of bytes for the resulting hash value.
		 * @throws RuntimeException if no {@link Provider} supports a {@link MessageDigestSpi} implementation for this algorithm.
		 */
		public byte[] digest(@Nonnull final CharSequence... charSequences) {
			return MessageDigests.digest(getInstance(), charSequences);
		}

		/**
		 * Computes a digest using this algorithm for the given character sequences, using the given charset.
		 * @implSpec This convenience method delegates to {@link MessageDigests#digest(MessageDigest, Charset, CharSequence...)}.
		 * @param charset The charset to use when converting characters to bytes.
		 * @param charSequences The character sequences to digest.
		 * @return The array of bytes for the resulting hash value.
		 * @throws RuntimeException if no {@link Provider} supports a {@link MessageDigestSpi} implementation for this algorithm.
		 */
		public byte[] digest(@Nonnull final Charset charset, @Nonnull final CharSequence... charSequences) {
			return MessageDigests.digest(getInstance(), charSequences);
		}

		/**
		 * Computes a digest using this algorithm for the given characters using the UTF-8 charset.
		 * @implSpec This convenience method delegates to {@link MessageDigests#digest(MessageDigest, char[])}.
		 * @param characters The characters to digest.
		 * @return The array of bytes for the resulting hash value.
		 * @throws RuntimeException if no {@link Provider} supports a {@link MessageDigestSpi} implementation for this algorithm.
		 */
		public byte[] digest(@Nonnull final char[] characters) {
			return MessageDigests.digest(getInstance(), characters);
		}

		/**
		 * Computes a digest using this algorithm for the given characters, using the given charset.
		 * @implSpec This convenience method delegates to {@link MessageDigests#digest(MessageDigest, Charset, char[])}.
		 * @param charset The charset to use when converting characters to bytes.
		 * @param characters The arrays of characters to digest.
		 * @return The array of bytes for the resulting hash value.
		 * @throws RuntimeException if no {@link Provider} supports a {@link MessageDigestSpi} implementation for this algorithm.
		 */
		public byte[] digest(@Nonnull final Charset charset, @Nonnull final char[] characters) {
			return MessageDigests.digest(getInstance(), charset, characters);
		}

		/**
		 * Computes a digest using this algorithm for the contents of the given input stream. All the remaining contents of the input stream are consumed.
		 * @implSpec This convenience method delegates to {@link MessageDigests#digest(MessageDigest, InputStream)}.
		 * @param inputStream The input stream on which to perform a digest.
		 * @return The array of bytes for the resulting hash value.
		 * @throws RuntimeException if no {@link Provider} supports a {@link MessageDigestSpi} implementation for this algorithm.
		 * @throws IOException if there is an I/O exception reading from the input stream.
		 */
		public byte[] digest(@Nonnull final InputStream inputStream) throws IOException {
			return MessageDigests.digest(getInstance(), inputStream);
		}

		/**
		 * Computes a digest using this algorithm for the contents of the given file.
		 * @implSpec This convenience method delegates to {@link MessageDigests#digest(MessageDigest, Path)}.
		 * @param file The path to the file on which to perform a digest.
		 * @return The array of bytes for the resulting hash value.
		 * @throws RuntimeException if no {@link Provider} supports a {@link MessageDigestSpi} implementation for this algorithm.
		 * @throws IOException if there is an I/O exception reading from the file.
		 */
		public byte[] digest(@Nonnull final Path file) throws IOException {
			return MessageDigests.digest(getInstance(), file);
		}

		/**
		 * Computes a lowercase hex checksum string for the given input bytes.
		 * @implSpec This implementation delegates to {@link MessageDigests#checksum(MessageDigest, byte[])}.
		 * @apiNote This method considers a <dfn>checksum</dfn> to be a string version of a message <dfn>digest</dfn>, as the former is often used in the context of
		 *          file contents verification.
		 * @param input The sequence of bytes for which a digest and then a checksum string should be created.
		 * @return The lowercase hex checksum string of the resulting hash value.
		 * @throws RuntimeException if no {@link Provider} supports a {@link MessageDigestSpi} implementation for this algorithm.
		 * @see MessageDigest#digest(byte[])
		 */
		public String checksum(@Nonnull final byte[] input) {
			return MessageDigests.checksum(getInstance(), input);
		}

		/**
		 * Computes a lowercase hex checksum string for the given character sequence using the UTF-8 charset.
		 * @implSpec This implementation delegates to {@link MessageDigests#checksum(MessageDigest, CharSequence)}.
		 * @apiNote This method considers a <dfn>checksum</dfn> to be a string version of a message <dfn>digest</dfn>, as the former is often used in the context of
		 *          file contents verification.
		 * @param charSequence The character sequence for which a checksum should be created.
		 * @throws RuntimeException if no {@link Provider} supports a {@link MessageDigestSpi} implementation for this algorithm.
		 * @return The lowercase hex checksum string of the resulting hash value.
		 */
		public String checksum(@Nonnull final CharSequence charSequence) {
			return MessageDigests.checksum(getInstance(), charSequence);
		}

		/**
		 * Computes a lowercase hex checksum string for the given characters using the UTF-8 charset.
		 * @implSpec This implementation delegates to {@link MessageDigests#checksum(MessageDigest, char[])}.
		 * @apiNote This method considers a <dfn>checksum</dfn> to be a string version of a message <dfn>digest</dfn>, as the former is often used in the context of
		 *          file contents verification.
		 * @param characters The characters for which a checksum should be created.
		 * @return The lowercase hex checksum string of the resulting hash value.
		 * @throws RuntimeException if no {@link Provider} supports a {@link MessageDigestSpi} implementation for this algorithm.
		 * @see Bytes#toHexString(byte[])
		 */
		public String checksum(@Nonnull final char[] characters) {
			return MessageDigests.checksum(getInstance(), characters);
		}

		/**
		 * Computes a lowercase hex checksum string for the contents of the given input stream. All the remaining contents of the input stream are consumed.
		 * @implSpec This implementation delegates to {@link MessageDigests#checksum(MessageDigest, InputStream)}.
		 * @apiNote This method considers a <dfn>checksum</dfn> to be a string version of a message <dfn>digest</dfn>, as the former is often used in the context of
		 *          file contents verification.
		 * @param inputStream The input stream for which a checksum should be created.
		 * @return The lowercase hex checksum string of the resulting hash value.
		 * @throws RuntimeException if no {@link Provider} supports a {@link MessageDigestSpi} implementation for this algorithm.
		 * @throws IOException if there is an I/O exception reading from the input stream.
		 * @see Bytes#toHexString(byte[])
		 */
		public String checksum(@Nonnull final InputStream inputStream) throws IOException {
			return MessageDigests.checksum(getInstance(), inputStream);
		}

		/**
		 * Computes a lowercase hex checksum string for the contents of the given file.
		 * @implSpec This implementation delegates to {@link MessageDigests#checksum(MessageDigest, Path)}.
		 * @apiNote This method considers a <dfn>checksum</dfn> to be a string version of a message <dfn>digest</dfn>, as the former is often used in the context of
		 *          file contents verification.
		 * @param file The path to the file for which a checksum should be created.
		 * @return The lowercase hex checksum string of the resulting hash value.
		 * @throws RuntimeException if no {@link Provider} supports a {@link MessageDigestSpi} implementation for this algorithm.
		 * @throws IOException if there is an I/O exception reading from the file.
		 * @see Bytes#toHexString(byte[])
		 */
		public String checksum(@Nonnull final Path file) throws IOException {
			return MessageDigests.checksum(getInstance(), file);
		}

		/**
		 * {@inheritDoc}
		 * @implSpec This implementation returns the hash code of the algorithm name.
		 * @see #getName()
		 */
		@Override
		public int hashCode() {
			return name.hashCode();
		}

		/**
		 * {@inheritDoc}
		 * @implSpec This implementation considers two algorithms equal if they have the same name.
		 * @see #getName()
		 */
		@Override
		public boolean equals(@Nonnull final Object object) {
			if(this == object) {
				return true;
			}
			if(!(object instanceof Algorithm)) {
				return false;
			}
			return getName().equals(((Algorithm)object).getName());
		}

		/**
		 * {@inheritDoc}
		 * @implSpec This version return the algorithm name.
		 * @see #getName()
		 */
		@Override
		public String toString() {
			return getName();
		}

	}

}
