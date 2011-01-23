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

/**Unexpected end of file error when parsing an input stream.
Used by {@link ParseReader}.
@see ParseIOException
@see ParseReader
@deprecated
*/
public class ParseEOFException extends ParseIOException
{

	/**Constructs an EOF exception from a parse reader.
	@param parseReader The parse reader the data of which is the source of the error.
	*/
	public ParseEOFException(final ParseReader parseReader)
	{
		this(parseReader.getLineIndex(), parseReader.getCharIndex(), parseReader.getName());	//construct the class with values from the parse reader
	}

	/**Constructs an EOF exception with a line index, a character index, and the
		name of the source.
	@param lineIndex The index of the line in which the error occurred.
	@param charIndex The index of the character at which the error occurred on the current line.
	@param sourceName The name of the source of the data (perhaps a filename).
	*/
	public ParseEOFException(final long lineIndex, final long charIndex, final String sourceName)
	{
		super("Unexpected end of file.", sourceName, lineIndex, charIndex);
	}

	/**Constructs an EOF exception with an error message, a line index, a character
		index, and the name of the source.
	@param s The error message.
	@param lineIndex The index of the line in which the error occurred.
	@param charIndex The index of the character at which the error occurred on the current line.
	@param sourceName The name of the source of the data (perhaps a filename).
	*/
	public ParseEOFException(final String s, final long lineIndex, final long charIndex, final String sourceName)
	{
		super(s, sourceName, lineIndex, charIndex);
	}
}

