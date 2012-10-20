/*
 * Copyright © 1996-2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package org.urframework.io;

import static com.globalmentor.io.Files.*;

import java.io.*;
import java.net.*;

import org.urframework.*;
import org.urframework.io.URFIO;

import com.globalmentor.io.IO;

/**
 * Various constants and utilities for examining files containing serialized URF.
 * @author Garret Wilson
 */
public class URFFiles
{

	/**
	 * Reads an object from a file using the given URF I/O support, with the URI of the file as the base URI.
	 * @param file The file from which to read.
	 * @param urf The URF instance to use in creating new resources.
	 * @param io The I/O support for reading the object.
	 * @return The object read from the file.
	 * @throws IOException if there is an error reading the data.
	 */
	public static <T> T read(final File file, final URF urf, final URFIO<T> io) throws IOException
	{
		return read(file, urf, toURI(file), io); //read from the file, using the file URI as the base URI
	}

	/**
	 * Reads an object from a file using the given URF I/O support.
	 * @param file The file from which to read.
	 * @param urf The URF instance to use in creating new resources.
	 * @param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	 * @param io The I/O support for reading the object.
	 * @return The object read from the file.
	 * @throws IOException if there is an error reading the data.
	 */
	public static <T> T read(final File file, final URF urf, final URI baseURI, final URFIO<T> io) throws IOException
	{
		final InputStream bufferedInputStream = new BufferedInputStream(new FileInputStream(file)); //create a buffered input stream to the file
		try
		{
			return io.read(urf, bufferedInputStream, baseURI); //read the object, using the given URF instance
		}
		finally
		{
			bufferedInputStream.close(); //always close the input stream
		}
	}

	/**
	 * Writes an object to a file using the given I/O support, with the URI of the file as the base URI.
	 * @param file The file to which to write.
	 * @param object The object to write to the given file.
	 * @param io The I/O support for writing the object.
	 * @throws IOException if there is an error writing the data.
	 */
	public static <T> void write(final File file, final T object, final IO<T> io) throws IOException
	{
		write(file, toURI(file), object, io); //write to the file, using the file URI as the base URI
	}

	/**
	 * Writes an object to a file using the given I/O support.
	 * @param file The file to which to write.
	 * @param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	 * @param object The object to write to the given file.
	 * @param io The I/O support for writing the object.
	 * @throws IOException if there is an error writing the data.
	 */
	public static <T> void write(final File file, final URI baseURI, final T object, final IO<T> io) throws IOException
	{
		final OutputStream bufferedOutputStream = new BufferedOutputStream(new FileOutputStream(file));//create a buffered output stream to the file
		try
		{
			io.write(bufferedOutputStream, baseURI, object); //write the object
		}
		finally
		{
			bufferedOutputStream.close(); //always close the output stream
		}
	}

	/**
	 * Stores an array of bytes in a file. The file is closed after the operation.
	 * @param file The file in which the bytes should be stored.
	 * @param bytes The bytes to store in the file.
	 * @throws IOException Thrown if there is an error loading the bytes.
	 * @see #readBytes(File)
	 */
	public static void write(final File file, final byte[] bytes) throws IOException
	{
		final OutputStream fileOutputStream = new BufferedOutputStream(new FileOutputStream(file)); //create a buffered output stream to the file
		try
		{
			fileOutputStream.write(bytes); //write the bytes to the file
			fileOutputStream.flush(); //flush all our data to the file
		}
		finally
		{
			fileOutputStream.close(); //always close the file output stream
		}
	}

}