/*
 * Copyright © 1996-2013 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.io;

import static com.globalmentor.io.Charsets.*;

import java.io.IOException;
import java.nio.ByteOrder;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Optional;

import static java.util.Objects.*;

import javax.annotation.Nonnull;

import com.globalmentor.java.Bytes;
import com.globalmentor.model.ObjectHolder;

/**
 * The Byte Order Mark (BOM) designations for different character encodings.
 * <p>
 * This implementation only supports UTF-8, UTF-16, and UTF-32 BOM variants.
 * </p>
 * @author Garret Wilson
 * @see <a href="http://www.unicode.org/faq/utf_bom.html#BOM">Unicode Byte Order Mark (BOM) FAQ</a>
 */
public enum ByteOrderMark {

	/** UTF-8 BOM */
	UTF_8((byte)0xEF, (byte)0xBB, (byte)0xBF),
	/** UTF-16, big-endian BOM */
	UTF_16BE((byte)0xFE, (byte)0xFF),
	/** UTF-16, little-endian BOM */
	UTF_16LE((byte)0xFF, (byte)0xFE),
	/** UTF-32, big-endian (1234 order) BOM */
	UTF_32BE((byte)0x00, (byte)0x00, (byte)0xFE, (byte)0xFF),
	/** UTF-32, little-endian (4321 order) BOM */
	UTF_32LE((byte)0xFF, (byte)0xFE, (byte)0x00, (byte)0x00),
	/** UTF-32, unusual octet order 1 (2143 order) BOM */
	UTF_32_UNUSUAL_ORDER1((byte)0x00, (byte)0x00, (byte)0xFF, (byte)0xFE),
	/** UTF-32, unusual octet order 2 (3412 order) BOM */
	UTF_32_UNUSUAL_ORDER2((byte)0xFE, (byte)0xFF, (byte)0x00, (byte)0x00);

	/** The maximum number of bytes used by any of the byte order marks in this implementation. */
	public static final int MAX_BYTE_COUNT = 4;

	/** The bytes of this byte order mark. */
	private final byte[] bytes;

	/** @return The bytes of this byte order mark. */
	public byte[] getBytes() {
		return bytes.clone(); //clone the bytes so that the authoritative copy cannot be modified
	}

	/** @return The number of bytes in this byte order mark. */
	public int getLength() {
		return bytes.length;
	}

	/**
	 * @return <code>true</code> if the byte order mark is one for which no charset exists.
	 * @see #UTF_32_UNUSUAL_ORDER1
	 * @see #UTF_32_UNUSUAL_ORDER2
	 */
	public boolean isUnusual() {
		return this == UTF_32_UNUSUAL_ORDER1 || this == UTF_32_UNUSUAL_ORDER2;
	}

	/**
	 * Checks to ensure this byte order mark is a usual one (i.e. one for which there exists a valid charset).
	 * @return This byte order mark.
	 * @throws IOException if this is an unusual byte order mark.
	 * @see #isUnusual()
	 */
	public ByteOrderMark checkUsualIO() throws IOException {
		if(isUnusual()) {
			throw new IOException("The byte order mark " + this + " is unsupported.");
		}
		return this;
	}

	/** @return The minimum number of bytes used for each character in the charset represented by this byte order mark. */
	public int getMinimumBytesPerCharacter() {
		switch(this) {
			case UTF_8:
				return 1;
			case UTF_16BE:
			case UTF_16LE:
				return 2;
			case UTF_32BE:
			case UTF_32LE:
			case UTF_32_UNUSUAL_ORDER1:
			case UTF_32_UNUSUAL_ORDER2:
				return 4;
			default:
				throw new AssertionError();
		}
	}

	/** @return The byte order of this byte order mark, or <code>null</code> if there is one byte per character or the byte order is unusual. */
	public ByteOrder getByteOrder() {
		switch(this) {
			case UTF_16BE:
			case UTF_32BE:
				return ByteOrder.BIG_ENDIAN;
			case UTF_16LE:
			case UTF_32LE:
				return ByteOrder.LITTLE_ENDIAN;
			case UTF_8:
			case UTF_32_UNUSUAL_ORDER1:
			case UTF_32_UNUSUAL_ORDER2:
				return null;
			default:
				throw new AssertionError();
		}
	}

	/**
	 * The index, of a group of encoded bytes for this identified byte order, of the least significant byte. For example, this method would return <code>0</code>
	 * and <code>1</code> for UTF-16LE and UTF-16BE, respectively. The index will be less than {@link #getMinimumBytesPerCharacter()}.
	 * @return The index of the least significant byte within an encoded group for this identified byte order.
	 */
	public int getLeastSignificantByteIndex() {
		switch(this) {
			case UTF_8:
			case UTF_16LE:
			case UTF_32LE:
				return 0;
			case UTF_16BE:
			case UTF_32_UNUSUAL_ORDER2:
				return 1;
			case UTF_32_UNUSUAL_ORDER1:
				return 2;
			case UTF_32BE:
				return 3;
			default:
				throw new AssertionError();
		}
	}

	/**
	 * Bytes constructor.
	 * @param bytes The bytes that represent this BOM
	 * @throws NullPointerException if the given bytes is <code>null</code>.
	 */
	private ByteOrderMark(final byte... bytes) {
		this.bytes = requireNonNull(bytes, "Bytes cannot be null");
	}

	/**
	 * Returns the byte order mark with which the given bytes start. If no valid byte order mark is present, <code>null</code> is returned.
	 * @param bytes The array of bytes potentially starting with a byte order mark.
	 * @return The byte order mark detected.
	 */
	public static Optional<ByteOrderMark> detect(@Nonnull final byte[] bytes) {
		//note: this implementation depends on the order of the BOM enum values, from shorter bytes to longer bytes
		final ByteOrderMark[] boms = values(); //check each BOM manually
		for(int i = boms.length - 1; i >= 0; --i) { //work backwards to check longer BOMs first, as UTF-32LE starts with the UTF-16LE BOM
			final ByteOrderMark bom = boms[i];
			if(Bytes.startsWith(bytes, bom.bytes)) { //if the bytes starts with the BOM's bytes (trusting the comparison method not to modify the bytes, as we pass in our private array)
				return Optional.of(bom);
			}
		}
		return Optional.empty(); //no matching BOM was found
	}

	/**
	 * Determines an imputed Byte Order Mark by detecting a BOM in the actual bytes or, if a true BOM is not present, by comparing the bytes to expected
	 * characters. Regardless of the number of expected characters given, only those characters necessary for detecting the byte order will be used.
	 * @param bytes The array of bytes representing the possible Byte Order Mark and possible expected characters.
	 * @param expectedChars The characters expected, regardless of the encoding method used. At least four characters should included.
	 * @param actualBOM Receives the actual byte order mark present, if any.
	 * @return The actual Byte Order Mark encountered; or, if no Byte Order Mark was present, the Byte Order Mark representing the character encoding assumed by
	 *         comparing bytes to the expected characters; or {@link Optional#empty()} if neither approach could determine a character encoding.
	 * @see <a href="http://www.w3.org/TR/2008/REC-xml-20081126/#sec-guessing-no-ext-info">XML 1.0 (Fifth Edition): F.1 Detection Without External Encoding
	 *      Information)</a>
	 */
	public static Optional<ByteOrderMark> impute(final byte[] bytes, final CharSequence expectedChars, final ObjectHolder<ByteOrderMark> actualBOM) { //TODO maybe allow a default eight-bit encoding to be specified
		Optional<ByteOrderMark> bom = ByteOrderMark.detect(bytes); //check the byte order mark by itself; if there is no BOM, this will return null
		if(bom.isPresent()) { //if we found a byte order mark TODO improve with Java 9: https://bugs.openjdk.java.net/browse/JDK-8071670
			actualBOM.setObject(bom.get()); //show that we actually found a byte order mark
		} else { //if no byte order mark was found, try to compare characters with what is expected
			if(bytes.length >= 4 && expectedChars.length() >= 1) { //if there are at least four bytes in the array, and we have at least one character to compare them with
				final char expected0 = expectedChars.charAt(0); //find the first character they were expecting
				if(bytes[0] == 0x00 && bytes[1] == 0x00 && bytes[2] == 0x00 && bytes[3] == (byte)expected0) { //00 00 00 X1: UCS-4, big-endian (1234 order)
					bom = Optional.of(UTF_32BE);
				} else if(bytes[0] == (byte)expected0 && bytes[1] == 0x00 && bytes[2] == 0x00 && bytes[3] == 0x00) { //X1 00 00 00: UCS-4, little-endian (4321 order)
					bom = Optional.of(UTF_32LE);
				} else if(bytes[0] == 0x00 && bytes[1] == 0x00 && bytes[2] == (byte)expected0 && bytes[3] == 0x00) { //00 00 X1 00: UCS-4, 2143 order
					bom = Optional.of(UTF_32_UNUSUAL_ORDER1);
				} else if(bytes[0] == 0x00 && bytes[1] == (byte)expected0 && bytes[2] == 0x00 && bytes[3] == 0x00) { //00 X1 00 00 UCS-4, 3412 order
					bom = Optional.of(UTF_32_UNUSUAL_ORDER2);
				}
				if(expectedChars.length() >= 2) { //if we have at least two character with which to compare the bytes in the array
					final char expected1 = expectedChars.charAt(1); //find the second character they were expecting
					if(bytes[0] == 0x00 && bytes[1] == (byte)expected0 && bytes[2] == 0x00 && bytes[3] == (byte)expected1) { //00 X1 00 X2: UTF-16, big-endian, no Byte Order Mark
						bom = Optional.of(UTF_16BE);
					} else if(bytes[0] == (byte)expected0 && bytes[1] == 0x00 && bytes[2] == (byte)expected1 && bytes[3] == 0x00) { //X1 00 X2 00: UTF-16, little-endian, no Byte Order Mark
						bom = Optional.of(UTF_16LE);
					}
					if(expectedChars.length() >= 4) { //if we have at least four character with which to compare the bytes in the array
						final char expected3 = expectedChars.charAt(2); //find the third character they were expecting
						final char expected4 = expectedChars.charAt(3); //find the fourth character they were expecting
						if(bytes[0] == (byte)expected0 && bytes[1] == (byte)expected1 && bytes[2] == (byte)expected3 && bytes[3] == (byte)expected4) { //X1 X2 X3 X4: UTF-8 (or similar), no Byte Order Mark
							bom = Optional.of(UTF_8);
						}
					}
				}
			}
		}
		return bom; //return whatever BOM was found (which may be empty)
	}

	/**
	 * Determines the byte order mark (BOM) needed to represent the given charset. For charsets that do not specify endianness, big-endian is assumed as per the
	 * {@link Charset} documentation.
	 * @param charset The charset for which a byte order mark should be returned.
	 * @return The byte order mark for the given character set, or <code>null</code> if there is no byte order mark to represent the given character set.
	 * @throws NullPointerException if the given charset is <code>null</code>.
	 */
	public static ByteOrderMark forCharset(final Charset charset) {
		final String name = charset.name();
		if(UTF_8_NAME.equals(name)) { //UTF-8
			return ByteOrderMark.UTF_8;
		} else if(UTF_16_NAME.equals(name)) { //UTF-16
			return ByteOrderMark.UTF_16BE; //default to UTF-16BE
		} else if(UTF_16BE_NAME.equals(name)) { //UTF-16BE
			return ByteOrderMark.UTF_16BE;
		} else if(UTF_16LE_NAME.equals(name)) { //UTF-16LE
			return ByteOrderMark.UTF_16LE;
		} else if(UTF_32_NAME.equals(name)) { //UTF-32
			return ByteOrderMark.UTF_32BE; //default to UTF-32BE
		} else if(UTF_32BE_NAME.equals(name)) { //UTF-32BE
			return ByteOrderMark.UTF_32BE;
		} else if(UTF_32LE_NAME.equals(name)) { //UTF-32LE
			return ByteOrderMark.UTF_32LE;
		} else { //if we don't recognize the charset
			return null; //we don't know of a byte order mark for this charset
		}
	}

	/**
	 * Returns a charset corresponding to this byte order mark.
	 * <p>
	 * The byte order marks {@link #UTF_32_UNUSUAL_ORDER1} and {@link #UTF_32_UNUSUAL_ORDER2} are defined in XML but have no corresponding charset, so calling
	 * this method for them (i.e. for byte order marks for which {@link #isUnusual()} returns <code>true</code>) will result in a
	 * {@link UnsupportedOperationException} being thrown.
	 * </p>
	 * @return a charset corresponding to this byte order mark.
	 * @throws UnsupportedOperationException if this byte order mark has no corresponding charset.
	 * @see #isUnusual()
	 */
	public Charset toCharset() {
		final String charsetName; //we'll skip the name and short-circuit the process for charsets we already have constructed
		switch(this) {
			case UTF_8:
				return StandardCharsets.UTF_8;
			case UTF_16BE:
				return StandardCharsets.UTF_16BE;
			case UTF_16LE:
				return StandardCharsets.UTF_16LE;
			case UTF_32BE:
				charsetName = UTF_32BE_NAME;
				break;
			case UTF_32LE:
				charsetName = UTF_32LE_NAME;
				break;
			case UTF_32_UNUSUAL_ORDER1:
			case UTF_32_UNUSUAL_ORDER2:
				throw new UnsupportedOperationException("The byte order mark " + this + " has no corresponding charset.");
			default:
				throw new AssertionError("Unhandled byte order mark: " + this);
		}
		return Charset.forName(charsetName);
	}
}
