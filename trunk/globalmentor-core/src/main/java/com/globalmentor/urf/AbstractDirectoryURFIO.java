/*
 * Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.urf;

import java.io.*;
import java.net.*;

import com.globalmentor.io.BOMInputStreamReader;

import static com.globalmentor.text.CharacterEncoding.*;

/**Base functionality for loading and saving URF information stored in <code>text/directory</code format as defined in
	<a href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>, "A MIME Content-Type for Directory Information".
<p>This class is not yet finished.</p>
@param <T> The type to read and write.
@author Garret Wilson
*/
public abstract class AbstractDirectoryURFIO<T> extends AbstractURFIO<T>	//TODO finish
{

	/**Whether a byte order mark (BOM) is written.*/
	private boolean bomWritten=true;

		/**@return Whether a byte order mark (BOM) is written.*/
		public boolean isBOMWritten() {return bomWritten;}

		/**Whether a byte order mark (BOM) is written.
		@param bomWritten Whether a byte order mark (BOM) is written.
		*/
		public void setBOMWritten(final boolean bomWritten) {this.bomWritten=bomWritten;}

	/**Class constructor.
	@param objectClass The class representing the type of object being loaded and saved.
	@exception NullPointerException if the given class is <code>null</code>.
	*/
	public AbstractDirectoryURFIO(final Class<T> objectClass)
	{
		super(objectClass);	//construct the parent class
	}

	/**Reads URF data from an input stream.
	This implementation reads <code>text/directory</code> information.
	@param urf The URF instance to use in creating new resources.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The URF instance representing the data read.
	@exception IOException if there is an error reading the data.
	@see #readTURF(URF, InputStream, URI)
	*/
	protected URF readURF(final URF urf, InputStream inputStream, final URI baseURI) throws IOException
	{
		return readDirectory(urf, inputStream, baseURI);	//read the turf
	}

	/**Reads URF data from a <code>text/directory</code> input stream using a default URF data model.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The URF instance representing the data read.
	@exception IOException if there is an error reading the data.
	*/
	public static URF readTURF(final InputStream inputStream, final URI baseURI) throws IOException
	{
		return readDirectory(new URF(), inputStream, baseURI);	//read a directory using a default URF data model
	}

	/**Reads URF data from a <code>text/directory</code> input stream.
	@param urf The URF instance to use in creating new resources.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The URF instance representing the data read.
	@exception IOException if there is an error reading the data.
	*/
	public static URF readDirectory(final URF urf, InputStream inputStream, final URI baseURI) throws IOException
	{
		return readDirectory(new URFDirectoryProcessor(urf), inputStream, baseURI);	//create a new URF processor and process the data
	}

	/**Reads URF data from a <code>text/directory</code> input stream using an existing URF processor.
	@param urfProcessor The URF processor for processing the data.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The URF instance representing the data read.
	@exception IOException if there is an error reading the data.
	*/
	public static URF readDirectory(final URFDirectoryProcessor urfProcessor, InputStream inputStream, final URI baseURI) throws IOException
	{
		if(!inputStream.markSupported())	//if the input stream doesn't support marking
		{
			inputStream=new BufferedInputStream(inputStream);	//buffer the input stream to allow marking
		}
		final Reader reader=new LineNumberReader(new BOMInputStreamReader(inputStream, UTF_8));	//created a reader from the input stream, defaulting to UTF-8 if not specified
throw new UnsupportedOperationException("not yet implemented");	//TODO fix		return readTURF(urfProcessor, reader, baseURI);	//read the TURF from the reader
	}

	/**Reads URF data from a TURF reader using a default URF data model.
	The given reader must support marking.
	@param reader The reader from which to read the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The URF instance representing the data read.
	@exception IOException if the reader does not suppport marking, or there is an error reading the data.
	*/
	public static URF readTURF(final Reader reader, final URI baseURI) throws IOException
	{
		return readTURF(new URF(), reader, baseURI);	//read TURF using a default URF data model
	}

	/**Reads URF data from a TURF reader.
	The given reader must support marking.
	@param urf The URF instance to use in creating new resources.
	@param reader The reader from which to read the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The URF instance representing the data read.
	@exception IOException if the reader does not suppport marking, or there is an error reading the data.
	*/
	public static URF readTURF(final URF urf, final Reader reader, final URI baseURI) throws IOException
	{
		return readTURF(new URFTURFProcessor(urf), reader, baseURI);	//create a new URF processor and process the data
	}

	/**Reads URF data from a TURF reader using an existing URF processor.
	The given reader must support marking.
	@param urfProcessor The URF processor for processing the data.
	@param reader The reader from which to read the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The URF instance representing the data read.
	@exception IOException if the reader does not suppport marking, or there is an error reading the data.
	*/
	public static URF readTURF(final URFTURFProcessor urfProcessor, final Reader reader, final URI baseURI) throws IOException
	{
		urfProcessor.process(reader, baseURI);	//process the TURF
		return urfProcessor.getURF();	//return the URF
	}

	/**Writes an URF instance to a TURF output stream.
	@param outputStream The output stream to which to write the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@param urf The URF instance to write to the given output stream.
	@excepion NullPointerException if the given output stream, and/or URF instance is <code>null</code>.
	@throws IOException Thrown if there is an error writing the data.
	*/
	protected void writeURF(final OutputStream outputStream, final URI baseURI, final URF urf) throws IOException
	{
		writeTURF(outputStream, baseURI, urf, isBOMWritten());	//write TURF, using this object as the namespace prefix manager
	}

	/**Writes a resource to an output stream.
	This implementation writes TURF.
	@param outputStream The output stream to which to write the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@param resource The resource to write to the given output stream.
	@throws IOException Thrown if there is an error writing the data.
	@see #writeTURFResource(OutputStream, URI, URFResource, TURFNamespaceLabelManager)
	*/
	protected void writeURFResource(final OutputStream outputStream, final URI baseURI, final URFResource resource) throws IOException
	{
		writeTURFResource(outputStream, baseURI, resource, isBOMWritten());	//write TURF, using this object as the namespace prefix manager
	}

	/**Writes an URF resource to a TURF output stream with a beginning BOM.
	@param outputStream The output stream to which to write the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@param resource The resource to write to the given output stream.
	@excepion NullPointerException if the given output stream, resource, and/or namespace prefix manager is <code>null</code>.
	@throws IOException Thrown if there is an error writing the data.
	*/
	public static void writeTURFResource(final OutputStream outputStream, final URI baseURI, final URFResource resource) throws IOException
	{
		writeTURFResource(outputStream, baseURI, resource, true);	//write the resource with a BOM
	}

	/**Writes an URF resource to a TURF output stream.
	@param outputStream The output stream to which to write the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@param resource The resource to write to the given output stream.
	@param bomWritten Whether a byte order mark (BOM) is written.
	@excepion NullPointerException if the given output stream, resource, and/or namespace prefix manager is <code>null</code>.
	@throws IOException Thrown if there is an error writing the data.
	*/
	public static void writeTURFResource(final OutputStream outputStream, final URI baseURI, final URFResource resource, final boolean bomWritten) throws IOException
	{
/*TODO fix
		if(bomWritten)	//if we should write a BOM
		{
			outputStream.write(BOM_UTF_8);	//write the UTF-8 byte order mark
		}
		final Writer writer=new OutputStreamWriter(outputStream, UTF_8);	//create a writer for writing in UTF-8
		final URFTURFGenerator turfGenerator=new URFTURFGenerator(baseURI, formatted, namespacePrefixManager);	//create a new TURF generator, using the given namespace prefix manager
		turfGenerator.generateResources(writer, resource);	//generate the resource to the writer
		writer.flush();	//flush the accumulated bytes, since we're using the writer locally only
*/
	}

	/**Writes an URF instance to a TURF output stream, formatted with a byte order mark (BOM).
	@param outputStream The output stream to which to write the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@param urf The URF instance to write to the given output stream.
	@excepion NullPointerException if the given output stream, URF instance, and/or namespace prefix manager is <code>null</code>.
	@throws IOException Thrown if there is an error writing the data.
	*/
	public static void writeTURF(final OutputStream outputStream, final URI baseURI, final URF urf) throws IOException
	{
		writeTURF(outputStream, baseURI, urf, true);	//write the URF with a BOM
	}

	/**Writes an URF instance to a TURF output stream.
	@param outputStream The output stream to which to write the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@param urf The URF instance to write to the given output stream.
	@param bomWritten Whether a byte order mark (BOM) is written.
	@excepion NullPointerException if the given output stream, URF instance, and/or namespace prefix manager is <code>null</code>.
	@throws IOException Thrown if there is an error writing the data.
	*/
	public static void writeTURF(final OutputStream outputStream, final URI baseURI, final URF urf, final boolean bomWritten) throws IOException
	{
/*TODO fix
		if(bomWritten)	//if we should write a BOM
		{
			outputStream.write(BOM_UTF_8);	//write the UTF-8 byte order mark
		}
		final Writer writer=new OutputStreamWriter(outputStream, UTF_8);	//create a writer for writing in UTF-8
		final URFTURFGenerator turfGenerator=new URFTURFGenerator(baseURI, formatted, namespacePrefixManager);	//create a new TURF generator, using the given namespace prefix manager
		turfGenerator.generateResources(writer, urf);	//generate the URF resources to the writer
		writer.flush();	//flush the accumulated bytes, since we're using the writer locally only
*/
	}

}