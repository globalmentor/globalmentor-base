/*
 * Copyright © 1996-2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static com.globalmentor.io.Files.*;
import static com.globalmentor.java.Conditions.*;

import java.io.*;

import com.globalmentor.log.Log;

/**
 * An output stream used for the temporary collection of bytes. The bytes are collected in memory using a {@link ByteArrayOutputStream}. If the collected bytes
 * rise above a configured threshold, bytes are collected instead in a temporary file that is removed when the output stream is closed. No additional buffering
 * is performed, allowing buffered to be controlled at a higher level.
 * 
 * <p>
 * An input stream to the collected bytes can be requested at any time using {@link #getInputStream()}, but if any bytes are written to the output stream after
 * the input stream is retrieved, the input stream is closed and released.
 * </p>
 * 
 * <p>
 * Once the stream is closed, the underlying output stream is released and the temporary file, if any, is deleted. Thus if it is not known exactly when the
 * stream will be closed, a subclass should be created that overrides {@link #beforeClose()}, at which point {@link #getInputStream()} can be called to retrieve
 * the written bytes.
 * </p>
 * 
 * <p>
 * It is very important to properly close this output stream when finished using it; otherwise, orphaned temporary files may remain.
 * </p>
 * 
 * @author Garret Wilson
 */
public class TempOutputStream extends OutputStreamDecorator<OutputStream>
{

	/** The temporary file in use, or <code>null</code> if no temporary file is in use (which means the output stream will be a {@link ByteArrayOutputStream}). */
	private File tempFile = null;

	/** The current input stream requested by the client, or <code>null</code> if there is no input stream in effect. */
	private InputStream inputStream = null;

	/**
	 * The threshold number of bytes for switching from memory to a temporary file. If set to zero, this file will always use a temporary file.
	 */
	private final int threshold;

	/**
	 * The threshold number of bytes for switching from memory to a temporary file. If set to zero, this output stream will always use a temporary file.
	 * @return The threshold number of bytes for switching from memory to a temporary file.
	 */
	public int getThreshold()
	{
		return threshold;
	}

	/** The default threshold number of bytes for switching from memory to a temporary file. */
	public final static int DEFAULT_THRESHOLD = 1 << 16;

	/**
	 * Default constructor with default threshold.
	 * @see #DEFAULT_THRESHOLD
	 */
	public TempOutputStream()
	{
		this(DEFAULT_THRESHOLD);
	}

	/**
	 * Threshold constructor. If the threshold is set to zero, this output stream will always use a temporary file.
	 * @param threshold The threshold number of bytes for switching from memory to a temporary file.
	 * @throws IllegalArgumentException if the given threshold is negative.
	 */
	public TempOutputStream(final int threshold)
	{
		super(new ByteArrayOutputStream(Math.min(threshold, 32))); //no need to have the byte array output stream larger than the threshold 
		this.threshold = checkArgumentNotNegative(threshold);
	}

	/**
	 * Retrieves an input stream to the current data. If there is already an input stream to the data, it is returned. If no input stream has been requested (or
	 * data has been written since the last request), the output stream is first flushed and then a new input stream to the data is created and returned. The next
	 * time data is written, the returned input stream will be closed and released.
	 * @return An input stream to the current data.
	 * @throws IOException if the output stream has already been closed.
	 * @throws IOException if there is an error retrieving an input stream to the data.
	 * @see #flush()
	 */
	public synchronized InputStream getInputStream() throws IOException
	{
		if(inputStream == null) //if we don't currently have an input stream
		{
			flush(); //flush our current data; this will also throw an exception if our output stream is closed
			if(tempFile != null) //if we are writing to a file
			{
				inputStream = new FileInputStream(tempFile); //get an input stream to the temp file
			}
			else
			//if we are not writing to a file, we must be writing to an underlying byte array output stream
			{
				inputStream = new ByteArrayInputStream(((ByteArrayOutputStream)getOutputStream()).toByteArray()); //get the bytes from the byte array output stream and get an input stream to them
			}
		}
		return inputStream;
	}

	/**
	 * Called before any writes occur, passing the number of bytes that will be written.
	 * <p>
	 * If any bytes are to be written and any input stream has been opened, it is closed and released.
	 * </p>
	 * <p>
	 * Examines the number of bytes prepared to write and, if writing them would surpass the threshold, switches immediately to a temporary file and writes the
	 * so-far accumulated bytes to that file.
	 * </p>
	 * @param len The number of bytes ready to be written.
	 * @throws IOException if there is an error updating the streams.
	 * @see #getThreshold()
	 * @see #getInputStream()
	 */
	protected synchronized void beforeWrite(final int len) throws IOException
	{
		if(inputStream != null && len > 0) //if we have an input stream in effect and there are bytes to be written
		{
			inputStream.close(); //close the input stream
			inputStream = null; //release the input stream
		}
		if(tempFile == null) //if we haven't switched to a file, yet
		{
			final ByteArrayOutputStream byteArrayOutputStream = (ByteArrayOutputStream)getOutputStream(); //we're still using a byte array
			if(byteArrayOutputStream.size() + len >= getThreshold()) //if writing these bytes would put us over the threshold
			{
				final byte[] bytes = byteArrayOutputStream.toByteArray(); //get the accumulated bytes
				final File file = createTempFile(); //create a new temporary file, but don't change our class state until we know we succeed in writing our current data
				final FileOutputStream fileOutputStream = new FileOutputStream(file); //get an output stream to the temp file
				try
				{
					fileOutputStream.write(bytes); //write the accumulated bytes to the temporary file
				}
				catch(final IOException ioException) //if we have any problem transferring the bytes over
				{
					file.delete(); //delete the temporary file so it won't be orphaned; the state hasn't been modified
					throw ioException;
				}
				setOutputStream(fileOutputStream); //now that we've successfully transferred over our bytes, switch over to the new output stream
				tempFile = file; //note the temporary file, so it will be deleted when we close
			}
		}
	}

	/**
	 * {@inheritDoc} This version performs pre-write checks and updates.
	 * @see #beforeWrite(int)
	 */
	@Override
	public void write(int b) throws IOException
	{
		beforeWrite(1);
		super.write(b);
	}

	/**
	 * {@inheritDoc} This version performs pre-write checks and updates.
	 * @see #beforeWrite(int)
	 */
	@Override
	public void write(byte b[]) throws IOException
	{
		beforeWrite(b.length);
		super.write(b);
	}

	/**
	 * {@inheritDoc} This version performs pre-write checks and updates.
	 * @see #beforeWrite(int)
	 */
	@Override
	public void write(byte b[], int off, int len) throws IOException
	{
		beforeWrite(1);
		super.write(b, off, len);
	}

	/**
	 * {@inheritDoc} This version does not allow closing without closing the decorated stream.
	 * @throws IllegalArgumentException if the close decorated stream flag is <code>false</code>.
	 */
	@Override
	public synchronized void close(final boolean closeDecoratedStream) throws IOException
	{
		checkArgument(closeDecoratedStream == true, "This decorated output stream does not allow closing with closing the underlying stream.");
		super.close(closeDecoratedStream);
	}

	/**
	 * Creates a temporary file. The file will not be marked for automatic deletion.
	 * @return A new file for temporarily storing data.
	 * @throw IOException if there is an error creating a temporary file.
	 */
	protected static File createTempFile() throws IOException
	{
		return Files.createTempFile(TempOutputStream.class.getSimpleName(), false); //create a temp file that won't automatically be deleted 
	}

	/** {@inheritDoc} This version closes and releases the input stream, if any, and deletes the temporary file, if any. */
	@Override
	public synchronized void dispose()
	{
		try
		{
			super.dispose();
		}
		finally
		{
			if(inputStream != null) //if we still have an input stream in effect
			{
				try
				{
					inputStream.close(); //try to close the input stream
				}
				catch(final IOException ioException)
				{
					Log.error(ioException);
				}
				inputStream = null; //release the input stream
			}
			if(tempFile != null) //if we still have a temporary file
			{
				try
				{
					delete(tempFile); //try to delete the temporary file
				}
				catch(final IOException ioException)
				{
					Log.error(ioException);
				}
				tempFile = null;
			}
		}
	}

}
