package com.globalmentor.urf;

import java.io.*;
import java.net.*;

/**Class for saving and loading an URF instance as TURF.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public class DefaultURFTURFIO extends AbstractTURFIO<URF>
{

	/**Default constructor.*/
	public DefaultURFTURFIO()
	{
		super(URF.class);	//construct the parent class
	}

	/**Reads URF from an input stream using an existing URF instance.
	@param urf The URF instance to use in creating new resources.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The URF instance read from the input stream.
	@exception NullPointerException if the given URF instance and/or input stream is <code>null</code>.
	@exception IOException if there is an error reading the data.
	@exception ClassCastException if no appropriate resource factory was installed, and the loaded resource is not of the correct Java class.
	*/ 
	public URF read(final URF urf, final InputStream inputStream, final URI baseURI) throws IOException
	{
		return readURF(urf, inputStream, baseURI);	//read URF from the input stream and return it
	}

	/**Writes URF to an output stream.
	@param outputStream The output stream to which to write the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@param urf The URF instance to write to the given output stream.
	@throws IOException Thrown if there is an error writing the data.
	*/
	public void write(final OutputStream outputStream, final URI baseURI, final URF urf) throws IOException
	{
		writeURF(outputStream, baseURI, urf);	//write the URF resources
	}

}
