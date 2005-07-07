package com.garretwilson.io;

import java.io.*;

/**Utility methods for writers.
@author Garret Wilson
*/
public class WriterUtilities
{

	/**Copies the contents of a reader to a writer. Neither reader nor writer is closed after the operation.
	@param reader The content to copy.
	@param writer The destination of the reader contents.
	@exception IOException Thrown if there is an error copying the content.
	*/
	public static void write(final Reader reader, final Writer writer) throws IOException
	{
		final char[] buffer=new char[64*1024];  //create a new 64k character buffer
		int charactersRead;  //we'll keep track of the characters we read each time
		while((charactersRead=reader.read(buffer))>=0)  //read as much as we can; while there's more information coming in
		{
			writer.write(buffer, 0, charactersRead); //write the characters to the output stream
		}
	}

}
