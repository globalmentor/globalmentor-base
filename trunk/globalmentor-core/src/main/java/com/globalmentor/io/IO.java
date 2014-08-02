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
import java.net.URI;

/**Support for reading or writing a particular type.
@param <T> The type to read and write.
@author Garret Wilson
*/
public interface IO<T>
{

	/**Reads an object from an input stream.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The object read from the input stream.
	@throws NullPointerException if the given input stream is <code>null</code>.
	@throws IOException Thrown if there is an error reading the data.
	*/ 
	public T read(final InputStream inputStream, final URI baseURI) throws IOException;
	
	/**Writes an object to an output stream.
	@param outputStream The output stream to which to write the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@param object The object to write to the given output stream.
	@throws NullPointerException if the given output stream and/or object is <code>null</code>.
	@throws IOException Thrown if there is an error writing the data.
	*/
	public void write(final OutputStream outputStream, final URI baseURI, final T object) throws IOException;
}
