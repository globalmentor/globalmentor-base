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

import java.io.*;

import com.garretwilson.text.CharacterEncoding;
import static com.garretwilson.text.CharacterEncoding.*;
import static com.globalmentor.io.InputStreams.*;

/**A reader that attempts to autodetect the character encoding of an input stream from the beginning byte order mark (BOM).
<p>The input stream must be at its beginning and must support marking and resetting.</p>
@author Garret Wilson
*/
public class BOMInputStreamReader extends InputStreamReader	//TODO create a version of this class that does not depend on input stream marking and resetting.
{

	/**Constructs an input stream reader that uses UTF-8 as the default character set
	 	if the character set cannot be determined by the BOM.
	@param inputStream An input stream; must be at its beginning and must support marking and resetting.
	@exception IOException if there is an error attempting to read the byte order mark from the input stream.
	@exception UnsupportedEncodingException if the named charset is not supported.
	*/
	public BOMInputStreamReader(final InputStream inputStream) throws IOException, UnsupportedEncodingException
	{
		this(inputStream, UTF_8);	//default to UTF-8 if we can't determine the character encoding by the BOM
	}
	
	/**Constructs an input stream reader that uses the given named character set as a default
	 	if the character set cannot be determined by the BOM.
	@param inputStream An input stream; must be at its beginning and must support marking and resetting.
	@param defaultCharsetName The name of the default character set to use if one cannot be determined by the BOM.
	@exception IOException if there is an error attempting to read the byte order mark from the input stream.
	@exception UnsupportedEncodingException if the named charset is not supported.
	*/
	public BOMInputStreamReader(final InputStream inputStream, final String defaultCharsetName) throws IOException, UnsupportedEncodingException
	{
		super(inputStream, getBOMEncoding(inputStream, new CharacterEncoding(defaultCharsetName)).toString());	//construct an input stream reader, guessing the encoding from the BOM if we can
	}

}
