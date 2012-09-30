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

package org.urframework;

import java.io.*;
import java.net.*;

/**Class for saving and loading an URF instance as TURF.
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
