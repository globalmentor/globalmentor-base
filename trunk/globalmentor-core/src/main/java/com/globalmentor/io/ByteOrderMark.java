/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static com.globalmentor.java.Objects.*;

/**The Byte Order Mark (BOM) designations for different character encodings.
@author Garret Wilson
@see <a href="http://www.w3.org/TR/REC-xml/#sec-guessing">XML 1.0 Fourth Edition: Autodetection of Character Encodings (Non-Normative)</a>
*/
public enum ByteOrderMark
{

	/**UTF-8 BOM*/
	UTF_8((byte)0xEF, (byte)0xBB, (byte)0xBF),
	/**UTF-16, big-endian BOM*/
	UTF_16BE((byte)0xFE, (byte)0xFF),
	/**UTF-16, little-endian BOM*/
	UTF_16LE((byte)0xFF, (byte)0xFE),
	/**UTF-32, big-endian BOM*/
	UTF_32BE((byte)0x00, (byte)0x00, (byte)0xFE, (byte)0xFF),
	/**UTF-32, little-endian BOM*/
	UTF_32LE((byte)0xFF, (byte)0xFE, (byte)0x00, (byte)0x00),
	/**UTF-32, unusual octet order 1 BOM*/
	UTF_32_UNUSUAL_ORDER1((byte)0x00, (byte)0x00, (byte)0xFF, (byte)0xFE),
	/**UTF-32, unusual octet order 2 BOM*/
	BOM_UTF_32_UNUSUAL_ORDER2((byte)0xFE, (byte)0xFF, (byte)0x00, (byte)0x00);

	/**The bytes of this byte order mark.*/
	private final byte[] bytes;

		/**The bytes of this byte order mark.*/
		public byte[] getBytes() {return bytes.clone();}	//clone the bytes so that the authoritative copy cannot be modified

	/**Bytes constructor.
	@param bytes The bytes that represent this BOM
	@throws NullPointerException if the given bytes is <code>null</code>.
	*/
	private ByteOrderMark(final byte... bytes)
	{
		this.bytes=checkInstance(bytes, "Bytes cannot be null");;
	}
}
