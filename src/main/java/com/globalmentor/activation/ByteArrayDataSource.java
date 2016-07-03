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

package com.globalmentor.activation;

import java.io.*;
import javax.activation.DataSource;

import static java.util.Objects.*;

import com.globalmentor.net.ContentType;

/**
 * A JavaBeans Activation Framework data source class that serves as a source of data from an array of bytes.
 * @author Garret Wilson
 */
public class ByteArrayDataSource implements DataSource {

	/** The data of which this class is a source. */
	private final byte[] bytes;

	/** The name of the data. */
	private final String name;

	/**
	 * Constructor that saves an array of bytes. The name of the class {@link ByteArrayDataSource} will be used as the name of the data source object.
	 * @param bytes The bytes from which data will be retrieved.
	 * @throws NullPointerException if the given byte array and/or name is <code>null</code>.
	 */
	public ByteArrayDataSource(final byte[] bytes) {
		this(bytes, ByteArrayDataSource.class.getName()); //construct the class with the class name
	}

	/**
	 * Byte array and name constructor.
	 * @param bytes The bytes from which data will be retrieved.
	 * @param name The name of the data source.
	 * @throws NullPointerException if the given byte array and/or name is <code>null</code>.
	 */
	public ByteArrayDataSource(final byte[] bytes, final String name) {
		this.bytes = requireNonNull(bytes, "Bytes cannot be null."); //save the bytes
		this.name = requireNonNull(name, "Name cannot be null."); //save the name
	}

	/** @return An input stream to the bytes. */
	public InputStream getInputStream() throws IOException {
		return new ByteArrayInputStream(bytes); //return an input stream to the bytes
	}

	/** @return An output stream to the bytes. */
	public OutputStream getOutputStream() throws IOException {
		throw new IOException("ByteArrayDataSource.getOutputStream() not yet implemented."); //TODO fix
	}

	/** @return The content type of a byte array, <code>application/octet-stream</code>. */
	public String getContentType() {
		return ContentType.APPLICATION_OCTET_STREAM_CONTENT_TYPE.toString();
	}

	/** @return The name of this object. */
	public String getName() {
		return name;
	}
}
