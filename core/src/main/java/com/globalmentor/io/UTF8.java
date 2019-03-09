/*
 * Copyright © 2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

/**
 * Constants and methods for working with the UTF-8 encoding.
 * 
 * @author Garret Wilson
 * 
 * @see <a href="http://www.ietf.org/rfc/rfc3629.txt">RFC 3629: UTF-8, a transformation format of ISO 10646</a>
 * @see <a href="http://en.wikipedia.org/wiki/UTF-8">Wikipedia: UTF-8</a>
 * @see <a href="http://developers.sun.com/dev/gadc/technicalpublications/articles/utf8.html">Sun: What Is UTF-8 And Why Is It Important?</a>
 * @see <a href="http://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt">UTF-8 Test</a>
 */
public class UTF8 {

	/** The largest code point value that can be encoded in one byte. */
	public static final int MAX_ENCODED_BYTE_COUNT1 = 0x7F;
	/** The largest code point value that can be encoded in two bytes. */
	public static final int MAX_ENCODED_BYTE_COUNT2 = 0x07FF;
	/** The largest code point value that can be encoded in three bytes. */
	public static final int MAX_ENCODED_BYTE_COUNT3 = 0xFFFF;

	/** The maximum number of octets used to encoded a character in UTF-8. */
	public static final int MAX_ENCODED_BYTE_COUNT_LENGTH = 4;

	/**
	 * Determines how many bytes are needed to encode a single character in UTF-8.
	 * @param c The character to encode.
	 * @return The minimum number of bytes needed to encode a single character.
	 */
	public static int getEncodedByteCountForCodePoint(final int c) {
		if(c <= MAX_ENCODED_BYTE_COUNT1) {
			return 1;
		} else if(c <= MAX_ENCODED_BYTE_COUNT2) {
			return 2;
		} else if(c <= MAX_ENCODED_BYTE_COUNT3) {
			return 3;
		} else {
			return 4;
		}
	}

	/**
	 * Determines how many bytes are used to encoded a sequence based on its first encoded byte.
	 * @param initialByte The value of the first byte (which in Java may be a negative number, as bytes are signed) in a UTF-8 sequence.
	 * @return The number of octets to expect in the sequence beginning with the given byte.
	 * @throws IllegalArgumentException if the given value is not a valid initial octet of UTF-8.
	 * @see #getEncodedByteCountFromInitialOctet(int)
	 */
	public static int getEncodedByteCountFromInitialByte(final byte initialByte) {
		return getEncodedByteCountFromInitialOctet(Byte.toUnsignedInt(initialByte));
	}

	/**
	 * Determines how many bytes are used to encoded a sequence based on its first encoded octet.
	 * @param initialOctet The value of the first octet in a UTF-8 sequence.
	 * @return The number of octets to expect in the sequence beginning with the given octet.
	 * @throws IllegalArgumentException if the given value is not a valid initial octet of UTF-8.
	 */
	public static int getEncodedByteCountFromInitialOctet(final int initialOctet) {
		if(initialOctet <= MAX_ENCODED_BYTE_COUNT1) { //0xxxxxxx
			return 1;
		}
		if(initialOctet >= 0b11000000) { //10xxxxxx is illegal UTF-8
			//check each bit from zero-based bit 5 to bit 3 (counting 2 to 4, bit = 7 - count)
			for(int count = 2; count <= MAX_ENCODED_BYTE_COUNT_LENGTH; count++) {
				if((initialOctet & (1 << (7 - count))) == 0) {
					return count; //the first 0 bit we find from the left indicates the count
				}
			}
		}
		throw new IllegalArgumentException("Invalid UTF-8 initial octet: 0x" + Integer.toHexString(initialOctet).toUpperCase());
	}

}
