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

import java.nio.charset.*;

import javax.annotation.*;

/**
 * Utilities for working with charsets.
 * <p>
 * In most cases, rather that using the names provided here, consider using {@link Charset#name()} for one of the predefined charsets in
 * {@link StandardCharsets}.
 * </p>
 * @author Garret Wilson
 * @see <a href="http://www.iana.org/assignments/character-sets">IANA Charset Registry</a>
 * @see <a href="http://www.w3.org/TR/REC-xml/#sec-guessing">XML 1.0 Fourth Edition: Autodetection of Character Encodings (Non-Normative)</a>
 * @see StandardCharsets
 */
public class Charsets {

	/** The canonical name of the eight-bit UTF-8 charset (to which the big-endian/little-endian byte order does not apply). */
	public static final String UTF_8_NAME = "UTF-8";
	/** The canonical name of the general 16-bit UTF-16 charset (which requires an initial Byte Order Mark). */
	public static final String UTF_16_NAME = "UTF-16";
	/** The canonical name of the 16-bit UTF-16 big-endian charset. */
	public static final String UTF_16BE_NAME = "UTF-16BE";
	/** The canonical name of the 16-bit UTF-16 little-endian charset. */
	public static final String UTF_16LE_NAME = "UTF-16LE";
	/** The canonical name of the 32-bit UTF-32 charset. */
	public static final String UTF_32_NAME = "UTF-32";
	/** The canonical name of the 32-bit UTF-32 big-endian charset. */
	public static final String UTF_32BE_NAME = "UTF-32BE";
	/** The canonical name of the 32-bit UTF-32 little-endian charset. */
	public static final String UTF_32LE_NAME = "UTF-32LE";
	/** The canonical name of the US-ASCII charset. */
	public static final String US_ASCII_NAME = "US-ASCII";
	/** The canonical name of the ISO-8859-1 charset. */
	public static final String ISO_8859_1_NAME = "ISO-8859-1";
	/** The canonical name of the Cp1252 charset. */
	public static final String WINDOWS_1252_NAME = "windows-1252";

	/**
	 * Convenience method for creating a new decoder from a charset and setting its error handling approach for malformed input and unmappable characters.
	 * @param charset The charset from which to create the decoder.
	 * @param codingErrorAction The action to take if the data being decoded is invalid.
	 * @return The new configured decoder.
	 * @see Charset#newDecoder()
	 * @see CharsetDecoder#onMalformedInput(CodingErrorAction)
	 * @see CharsetDecoder#onUnmappableCharacter(CodingErrorAction)
	 */
	public static CharsetDecoder newDecoder(@Nonnull final Charset charset, @Nonnull final CodingErrorAction codingErrorAction) {
		final CharsetDecoder decoder = charset.newDecoder();
		decoder.onMalformedInput(codingErrorAction);
		decoder.onUnmappableCharacter(codingErrorAction);
		return decoder;
	}
}
