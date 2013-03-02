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

import java.io.*;
import java.nio.charset.Charset;

import static com.globalmentor.io.Charsets.*;
import static com.globalmentor.io.InputStreams.*;

/**
 * A reader that attempts to auto-detect the charset of an input stream from the beginning byte order mark (BOM).
 * <p>
 * The input stream must be at its beginning and must support marking and resetting.
 * </p>
 * @author Garret Wilson
 * @see ByteOrderMark
 */
public class BOMInputStreamReader extends InputStreamReader //TODO create a version of this class that does not depend on input stream marking and resetting.
{

	/**
	 * Constructs an input stream reader that uses UTF-8 as the default charset if the charset cannot be determined by the BOM.
	 * @param inputStream An input stream; must be at its beginning and must support marking and resetting.
	 * @throws IOException if there is an error attempting to read the byte order mark from the input stream.
	 * @throws UnsupportedEncodingException if the named charset is not supported.
	 */
	public BOMInputStreamReader(final InputStream inputStream) throws IOException, UnsupportedEncodingException
	{
		this(inputStream, UTF_8_CHARSET); //default to UTF-8 if we can't determine the charset by the BOM
	}

	/**
	 * Constructs an input stream reader that uses the given named charset as a default if the charset cannot be determined by the BOM.
	 * @param inputStream An input stream; must be at its beginning and must support marking and resetting.
	 * @param defaultCharset The default charset to use if one cannot be determined by the BOM.
	 * @throws IOException if there is an error attempting to read the byte order mark from the input stream.
	 * @throws UnsupportedEncodingException if the named charset is not supported.
	 */
	public BOMInputStreamReader(final InputStream inputStream, final Charset defaultCharset) throws IOException, UnsupportedEncodingException
	{
		super(inputStream, detectCharset(inputStream, defaultCharset)); //construct an input stream reader, guessing the encoding from the BOM if we can
	}

}
