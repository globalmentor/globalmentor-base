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
import static com.globalmentor.java.Objects.*;

import java.io.IOException;
import java.nio.charset.Charset;

import com.globalmentor.java.Bytes;

/**
 * The Byte Order Mark (BOM) designations for different character encodings.
 * @author Garret Wilson
 * @see <a href="http://www.w3.org/TR/REC-xml/#sec-guessing">XML 1.0 Fourth Edition: Autodetection of Character Encodings (Non-Normative)</a>
 */
public enum ByteOrderMark {

	/** UTF-8 BOM */
	UTF_8((byte)0xEF, (byte)0xBB, (byte)0xBF),
	/** UTF-16, big-endian BOM */
	UTF_16BE((byte)0xFE, (byte)0xFF),
	/** UTF-16, little-endian BOM */
	UTF_16LE((byte)0xFF, (byte)0xFE),
	/** UTF-32, big-endian BOM */
	UTF_32BE((byte)0x00, (byte)0x00, (byte)0xFE, (byte)0xFF),
	/** UTF-32, little-endian BOM */
	UTF_32LE((byte)0xFF, (byte)0xFE, (byte)0x00, (byte)0x00),
	/** UTF-32, unusual octet order 1 BOM */
	UTF_32_UNUSUAL_ORDER1((byte)0x00, (byte)0x00, (byte)0xFF, (byte)0xFE),
	/** UTF-32, unusual octet order 2 BOM */
	UTF_32_UNUSUAL_ORDER2((byte)0xFE, (byte)0xFF, (byte)0x00, (byte)0x00);

	/** The bytes of this byte order mark. */
	private final byte[] bytes;

	/** The bytes of this byte order mark. */
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

	/**
	 * Bytes constructor.
	 * @param bytes The bytes that represent this BOM
	 * @throws NullPointerException if the given bytes is <code>null</code>.
	 */
	private ByteOrderMark(final byte... bytes) {
		this.bytes = checkInstance(bytes, "Bytes cannot be null");
	}

	/**
	 * Returns the byte order mark with which the given bytes start. If no valid byte order mark is present, <code>null</code> is returned.
	 * @param bytes The array of bytes potentially starting with a byte order mark.
	 * @return The byte order mark detected, or <code>null</code> if no byte order mark is present.
	 */
	public static ByteOrderMark detectByteOrderMark(final byte[] bytes) {
		for(final ByteOrderMark bom : values()) { //check each BOM manually
			if(Bytes.startsWith(bytes, bom.bytes)) { //if the bytes starts with the BOM's bytes (trusting the comparison method not to modify the bytes, as we pass in our private array)
				return bom;
			}
		}
		return null; //no matching BOM was found
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
		final String charsetName;
		switch(this) {
			case UTF_8:
				return UTF_8_CHARSET; //we already have the UTF-8 charset; short circuit and return it
			case UTF_16BE:
				charsetName = UTF_16BE_NAME;
				break;
			case UTF_16LE:
				charsetName = UTF_16LE_NAME;
				break;
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
