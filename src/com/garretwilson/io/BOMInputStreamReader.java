package com.garretwilson.io;

import java.io.*;

import static com.garretwilson.io.InputStreamUtilities.*;
import static com.garretwilson.text.CharacterEncodingConstants.*;
import com.garretwilson.text.CharacterEncoding;

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
		super(inputStream, getBOMEncoding(inputStream, new CharacterEncoding(defaultCharsetName)).getEncoding());	//construct an input stream reader, guessing the encoding from the BOM if we can
	}

}
