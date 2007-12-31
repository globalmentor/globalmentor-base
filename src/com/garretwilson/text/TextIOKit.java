package com.garretwilson.text;

import java.io.*;
import java.net.*;
import com.garretwilson.io.*;

/**Class for loading and saving text. Text is saved in the UTF-8 encoding.
@author Garret Wilson
@see LocaleText
*/
public class TextIOKit extends AbstractIOKit<StringBuilder>
{

	/**Default constructor.*/
	public TextIOKit()
	{
		this(null, null);
	}

	/**URI input stream locator constructor.
	@param uriInputStreamable The implementation to use for accessing a URI for
		input, or <code>null</code> if the default implementation should be used.
	*/
	public TextIOKit(final URIInputStreamable uriInputStreamable)
	{
		this(uriInputStreamable, null);
	}

	/**URI output stream locator constructor.
	@param uriOutputStreamable The implementation to use for accessing a URI for
		output, or <code>null</code> if the default implementation should be used.
	*/
	public TextIOKit(final URIOutputStreamable uriOutputStreamable)
	{
		this(null, uriOutputStreamable);
	}

	/**Full constructor.
	@param uriInputStreamable The implementation to use for accessing a URI for
		input, or <code>null</code> if the default implementation should be used.
	@param uriOutputStreamable The implementation to use for accessing a URI for
		output, or <code>null</code> if the default implementation should be used.
	*/
	public TextIOKit(final URIInputStreamable uriInputStreamable, final URIOutputStreamable uriOutputStreamable)
	{
		super(uriInputStreamable, uriOutputStreamable);	//construct the parent class
	}

	/**Loads a model from an input stream.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the content, or <code>null</code> if no base URI is available.
	@return The text from the input stream.
	@throws IOException Thrown if there is an error reading the data.
	*/ 
	public StringBuilder load(final InputStream inputStream, final URI baseURI) throws IOException
	{
		final CharacterEncoding encoding=InputStreamUtilities.getBOMEncoding(inputStream);	//try to sense from the byte order mark the encoding of the text
		final byte[] bytes=InputStreamUtilities.getBytes(inputStream);	//get the bytes from the input stream
		//use the character encoding we sensed to create a string, using a default encoding if we couldn't sense one from the byte order mark
		final String string=encoding!=null ? new String(bytes, encoding.toString()) : new String(bytes);
		return new StringBuilder(string);	//return a text model from the text we read
	}
	
	/**Saves a model to an output stream.
	@param model The model the data of which will be written to the given output stream.
	@param outputStream The output stream to which to write the model content.
	@throws IOException Thrown if there is an error writing the model.
	*/
	public void save(final StringBuilder model, final OutputStream outputStream) throws IOException
	{
		//TODO create a BOMWriter that does both steps
		outputStream.write(CharacterEncoding.BOM_UTF_8);	//write the UTF-8 byte order mark
		final Writer writer=new OutputStreamWriter(outputStream, CharacterEncoding.UTF_8);	//create a UTF-8 writer
		writer.write(model.toString());	//write the text to the writer
		writer.flush();	//flush the data to the output stream
	}
}
