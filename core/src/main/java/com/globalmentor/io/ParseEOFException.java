/*
 * Copyright © 1996-2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import java.io.LineNumberReader;
import java.io.Reader;

/**
 * Unexpected end of file error when parsing an input stream. Used by {@link ParseReader}.
 * @see ParseIOException
 */
public class ParseEOFException extends ParseIOException { //TODO rename to ParseEndException

	private static final long serialVersionUID = 1L;

	/**
	 * Reader constructor with a default message and no cause. This constructor will attempt to determine the location from the reader.
	 * @param reader The reader from which the error occurred.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 * @see LineNumberReader
	 */
	public ParseEOFException(final Reader reader) {
		this(reader, "Unexpected end of data.");
	}

	/**
	 * Reader and message constructor with no cause. This constructor will attempt to determine the location from the reader.
	 * @param reader The reader from which the error occurred.
	 * @param message The error message, or <code>null</code> if there is no error message.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 */
	public ParseEOFException(final Reader reader, final String message) {
		this(message, getLineIndex(reader), getCharacterIndex(reader));
	}

	/**
	 * Reader and message constructor with no cause. This constructor will attempt to determine the location from the reader.
	 * @param message The error message, or <code>null</code> if there is no error message.
	 * @param lineIndex The index of the line in which the error occurred, or -1 if not known.
	 * @param charIndex The index of the character at which the error occurred on the current line, or -1 if not known.
	 * @throws NullPointerException if the given reader is <code>null</code>.
	 */
	public ParseEOFException(final String message, final long lineIndex, final long charIndex) {
		super(message, lineIndex, charIndex);
	}

}
