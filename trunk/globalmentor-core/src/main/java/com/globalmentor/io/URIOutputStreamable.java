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

/**Class that knows how to get an output stream for the given URI.
@author Garret Wilson
@see OutputStream
@deprecated
*/
public interface URIOutputStreamable
{

	/**Returns an output stream for the given URI.
	The calling class has the responsibility for closing the output stream.
	@param uri A URI to a resource.
	@return An output stream to the contents of the resource represented by the given URI.
	@throws IOException Thrown if an I/O error occurred.
	*/
	public OutputStream getOutputStream(final URI uri) throws IOException;

}
