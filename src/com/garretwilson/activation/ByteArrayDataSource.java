package com.garretwilson.activation;

import java.io.*;
import javax.activation.DataSource;

/**A JAF data source class that serves as a source of data from an array of
	bytes.
@author Garret Wilson
*/
public class ByteArrayDataSource implements DataSource
{

	/**The data of which this class is a source.*/
	private final byte[] bytes;

	/**Constructor that saves an array of bytes.
	@param byteArray The bytes from which data will be retrieved.
	*/
	public ByteArrayDataSource(final byte[] byteArray)
	{
		bytes=byteArray;  //save the bytes
	}

	/**@return An input stream to the bytes.*/
  public InputStream getInputStream() throws IOException
	{
		return new ByteArrayInputStream(bytes); //return an input stream to the bytes
	}

  /**@return An output stream to the bytes.*/
	public OutputStream getOutputStream() throws IOException
	{
		throw new IOException("ByteArrayDataSource.getOutputStream() not yet implemented."); //G***fix
	}

	/**@return The content type of a byte array, "application/octet-stream".*/
  public String getContentType()
	{
		return "application/octet-stream";  //G***use a constant; comment
	}

  /**@return The name of this object.*/
	public String getName()
	{
		return getClass().getName();  //G***fix
	}
}