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

import java.nio.charset.Charset;

/**Utilities for working with charsets.
@author Garret Wilson
@see <a href="http://www.iana.org/assignments/character-sets">IANA Charset Registry</a>
@see <a href="http://www.w3.org/TR/REC-xml/#sec-guessing">XML 1.0 Fourth Edition: Autodetection of Character Encodings (Non-Normative)</a>
*/
public class Charsets
{
		//canonical name
	/**The canonical name of the eight-bit UTF-8 charset (to which the big-endian/little-endian byte order does not apply).*/
	public final static String UTF_8_NAME="UTF-8";
	/**The canonical name of the general 16-bit UTF-16 charset (which requires an initial Byte Order Mark).*/
	public final static String UTF_16_NAME="UTF-16";
	/**The canonical name of the 16-bit UTF-16 big-endian charset.*/
	public final static String UTF_16BE_NAME="UTF-16BE";
	/**The canonical name of the 16-bit UTF-16 little-endian charset.*/
	public final static String UTF_16LE_NAME="UTF-16LE";
	/**The canonical name of the 32-bit UTF-32 charset.*/
	public final static String UTF_32_NAME="UTF-32";
	/**The canonical name of the 32-bit UTF-32 big-endian charset.*/
	public final static String UTF_32BE_NAME="UTF-32BE";
	/**The canonical name of the 32-bit UTF-32 little-endian charset.*/
	public final static String UTF_32LE_NAME="UTF-32LE";
	/**The canonical name of the ISO-8859-1 charset.*/
	public final static String ISO_8859_1_NAME="ISO-8859-1";
	/**The canonical name of the Cp1252 charset.*/
	public final static String WINDOWS_1252_NAME="windows-1252";

	/**The UTF-8 charset.*/
	public final static Charset UTF_8_CHARSET=Charset.forName(UTF_8_NAME);

	/**Determines the byte order mark (BOM) needed to represent the given charset.
	For charsets that do not specify endianness, big-endian is assumed.
	@param charset The charset for which a byte order mark should be returned.
	@return The byte order mark for the given character set, or <code>null</code> if there is no byte order mark to represent the given character set.
	@throws NullPointerException if the given charset is <code>null</code>.
	*/
	public static ByteOrderMark getByteOrderMark(final Charset charset)
	{
		final String name=charset.name();	//get the charset name
		if(UTF_8_NAME.equals(name))	//UTF-8
		{
			return ByteOrderMark.UTF_8;
		}
		else if(UTF_16_NAME.equals(name))	//UTF-16
		{
			return ByteOrderMark.UTF_16BE;	//default to UTF-16BE
		}
		else if(UTF_16BE_NAME.equals(name))	//UTF-16BE
		{
			return ByteOrderMark.UTF_16BE;
		}
		else if(UTF_16LE_NAME.equals(name))	//UTF-16LE
		{
			return ByteOrderMark.UTF_16LE;
		}
		else if(UTF_32_NAME.equals(name))	//UTF-32
		{
			return ByteOrderMark.UTF_32BE;	//default to UTF-32BE
		}
		else if(UTF_32BE_NAME.equals(name))	//UTF-32BE
		{
			return ByteOrderMark.UTF_32BE;
		}
		else if(UTF_32LE_NAME.equals(name))	//UTF-32LE
		{
			return ByteOrderMark.UTF_32LE;
		}
		else	//if we don't recognize the charset
		{
			return null;	//we don't know of a byte order mark for this charset
		}
	}
}
