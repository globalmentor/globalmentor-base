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

package com.globalmentor.util;

import java.io.*;
import java.util.*;

/**A properties object with file storage.
@author Garret Wilson
@deprecated
*/
public class FileProperties extends Properties
{

	/**The file in which the properties should be stored.*/
	private File file;

		/**@return The file in which the properties should be stored.*/
		public File getFile() {return file;}

	/**Creates properties from the given file. If the given file exists, the
		the properties will be automatically loaded.
	@param file The file in which the properties should be stored.
	@exception IOException Thrown if there is an error loading the properties.
	@see #load
	*/
	public FileProperties(final File file) throws IOException
	{
		this.file=file; //save the file
		if(file.exists()) //if the file exists
		{
			load(); //load the properties
		}
	}

	/**Loads the properties from the file.
	@exception IOException Thrown if there is an error loading the properties.
	@see Properties#load
	*/
	public void load() throws IOException
	{
		final InputStream inputStream=new BufferedInputStream(new FileInputStream(getFile()));  //create an input stream to the file
		try
		{
		  load(inputStream);  //load the properties from the input stream
		}
		finally
		{
			inputStream.close();  //always close the input stream
		}
	}

	/**Saves the properties to the file.
	@exception IOException Thrown if there is an error storing the properties.
	@see Properties#store
	*/
	public void store() throws IOException
	{
		final OutputStream outputStream=new BufferedOutputStream(new FileOutputStream(getFile()));  //create an output stream to the file
		try
		{
		  store(outputStream, null);  //store the properties in the output stream
			outputStream.flush(); //flush the contents of the output stream
		}
		finally
		{
			outputStream.close();  //always close the output stream
		}
	}
}