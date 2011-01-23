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

/**Utility methods for writers.
@author Garret Wilson
*/
public class Writers
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
