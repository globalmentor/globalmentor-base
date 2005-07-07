package com.garretwilson.io;

import java.io.*;
import com.garretwilson.util.Debug;

/**Class to manipulate output streams.
@author Garret Wilson
*/
public class OutputStreamUtilities
{

	/**Copies the contents of an input stream to an output stream. Neither stream
		is closed after the operation.
	@param inputStream The content to copy.
	@param outputStream The destination of the input stream contents.
	@exception IOException Thrown if there is an error copying the content.
	*/
	public static void write(final InputStream inputStream, final OutputStream outputStream) throws IOException
	{
		final byte[] buffer=new byte[64*1024];  //create a new 64k buffer
		int bytesRead;  //we'll keep track of the bytes we read each time
		while((bytesRead=inputStream.read(buffer))>=0)  //read as much as we can; while there's more information coming in
		{
			outputStream.write(buffer, 0, bytesRead); //write the bytes to the output stream
		}
	}

	/**Writes the given byte to the output stream the specified number of times.
	@param outputStream The stream the bytes should be written to.
	@param b The byte to write; the 24 high-order bits are ignored.
	@param count The number of bytes to write.
	@exception IOException Thrown if there is an error writing to the output stream.
	*/
	public static void write(final OutputStream outputStream, final int b, int count) throws IOException
	{
		for(; count>0; --count) //decrement the count each time we write a byte
			outputStream.write(b);  //write the byte
	}

	/**Writes the given number of bytes of the given value, low-ordered bytes first.
	@param outputStream The stream the bytes should be written to.
	@param value The value to write.
	@param byteCount The number of bytes to write (&lt;=4).
	@exception IllegalArgumentException Thrown if the byte count is over four.
	@exception IOException Thrown if there is an error writing to the output stream.
	*/
	public static void writeLowOrderFirst(final OutputStream outputStream, long value, int byteCount) throws IllegalArgumentException, IOException
	{
//G***del Debug.trace("writing low order value: ", Integer.toHexString(value));  //G***del
		if(byteCount>4) //if an invalid byte count was given
			throw new IllegalArgumentException("Invalid byte count: "+byteCount);
		for(; byteCount>0; --byteCount) //write each byte
		{
//G***del Debug.trace("writing value: ", Integer.toHexString(value&0xff));  //G***del
			outputStream.write((int)value);  //write the LSB of the value
			value=value>>>8; //shift the value down a byte
		}
	}

}