/*
 * Copyright © 2013 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

/**
 * An object that can read information from an input stream.
 * @author Garret Wilson
 */
public interface InputStreamReadable {

	/**
	 * Reads the contents of the given input stream.
	 * <p>
	 * Any buffering will likely be done at a higher level and need not be implemented within this method. The given input stream is not closed at the end of this
	 * operation.
	 * </p>
	 * @param inputStream The input stream from which the information will be read.
	 * @param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	 * @throws IOException if there is an error reading the contents of the input stream.
	 */
	public void read(final InputStream inputStream, final URI baseURI) throws IOException;

}
