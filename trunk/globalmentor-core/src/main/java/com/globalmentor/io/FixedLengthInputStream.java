/*
 * Copyright © 2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static com.globalmentor.java.Conditions.*;

/**Wraps an existing input stream and only returns a fixed number of bytes.
<p>This stream should always be closed when access is finished; otherwise the underlying stream could be corrupted.</p>
<p>This class is not thread safe.</p>
@author Garret Wilson
*/
public class FixedLengthInputStream extends InputStreamDecorator<InputStream>
{

	/**The number of bytes left to be read.*/
	private long length;

		/**@return The number of bytes left to be read.*/
		protected long getLength() {return length;}

	/**Whether the decorated stream should be closed when this stream is closed.*/
	private final boolean closeDecoratedStream;

	/**Decorates the given input stream.
	The underlying stream will be closed when this stream is closed.
	@param inputStream The input stream to decorate.
	@param length The number of bytes to read.
	@exception NullPointerException if the given stream is <code>null</code>.
	@throws IllegalArgumentException if the given length is less than zero.
	*/
	public FixedLengthInputStream(final InputStream inputStream, final long length)
	{
		this(inputStream, length, true);
	}

	/**Decorates the given input stream.
	@param inputStream The input stream to decorate.
	@param length The number of bytes to read.
	@param closeDecoratedStream Whether the decorated stream should be closed when this stream is closed.
	@exception NullPointerException if the given stream is <code>null</code>.
	@throws IllegalArgumentException if the given length is less than zero.
	*/
	public FixedLengthInputStream(final InputStream inputStream, final long length, final boolean closeDecoratedStream)
	{
		super(inputStream);
		this.length=checkArgumentMinimum(length, 0);
		this.closeDecoratedStream=closeDecoratedStream;
	}

  /**
   * Reads the next byte of data from the input stream. The value byte is
   * returned as an <code>int</code> in the range <code>0</code> to
   * <code>255</code>. If no byte is available because the end of the stream
   * has been reached, the value <code>-1</code> is returned. This method
   * blocks until input data is available, the end of the stream is detected,
   * or an exception is thrown.
   *
   * <p> A subclass must provide an implementation of this method.
   *
   * @return     the next byte of data, or <code>-1</code> if the end of the
   *             stream is reached.
   * @exception  IOException  if an I/O error occurs.
   */
  public int read() throws IOException
	{
  	if(length==0)	//if we've reached the end of our stream
  	{
  		return -1;
  	}
  	final int b=super.read();
  	if(b>=0)	//if we haven't reached the end of the stream
  	{
  		--length;	//indicate that we've read another byte
  	}
  	return b;
	}

  /**
   * Reads some number of bytes from the input stream and stores them into
   * the buffer array <code>b</code>. The number of bytes actually read is
   * returned as an integer.  This method blocks until input data is
   * available, end of file is detected, or an exception is thrown.
   *
   * <p> If <code>b</code> is <code>null</code>, a
   * <code>NullPointerException</code> is thrown.  If the length of
   * <code>b</code> is zero, then no bytes are read and <code>0</code> is
   * returned; otherwise, there is an attempt to read at least one byte. If
   * no byte is available because the stream is at end of file, the value
   * <code>-1</code> is returned; otherwise, at least one byte is read and
   * stored into <code>b</code>.
   *
   * <p> The first byte read is stored into element <code>b[0]</code>, the
   * next one into <code>b[1]</code>, and so on. The number of bytes read is,
   * at most, equal to the length of <code>b</code>. Let <i>k</i> be the
   * number of bytes actually read; these bytes will be stored in elements
   * <code>b[0]</code> through <code>b[</code><i>k</i><code>-1]</code>,
   * leaving elements <code>b[</code><i>k</i><code>]</code> through
   * <code>b[b.length-1]</code> unaffected.
   *
   * <p> If the first byte cannot be read for any reason other than end of
   * file, then an <code>IOException</code> is thrown. In particular, an
   * <code>IOException</code> is thrown if the input stream has been closed.
   *
   * <p> The <code>read(b)</code> method for class <code>InputStream</code>
   * has the same effect as: <pre><code> read(b, 0, b.length) </code></pre>
   *
   * @param      b   the buffer into which the data is read.
   * @return     the total number of bytes read into the buffer, or
   *             <code>-1</code> is there is no more data because the end of
   *             the stream has been reached.
   * @exception  IOException  if an I/O error occurs.
   * @exception  NullPointerException  if <code>b</code> is <code>null</code>.
   * @see        java.io.InputStream#read(byte[], int, int)
   */
  public final int read(byte b[]) throws IOException
	{
  	return read(b, 0, b.length);	//let the other method take care of the fixed length
	}

  /**
   * Reads up to <code>len</code> bytes of data from the input stream into
   * an array of bytes.  An attempt is made to read as many as
   * <code>len</code> bytes, but a smaller number may be read.
   * The number of bytes actually read is returned as an integer.
   *
   * <p> This method blocks until input data is available, end of file is
   * detected, or an exception is thrown.
   *
   * <p> If <code>b</code> is <code>null</code>, a
   * <code>NullPointerException</code> is thrown.
   *
   * <p> If <code>off</code> is negative, or <code>len</code> is negative, or
   * <code>off+len</code> is greater than the length of the array
   * <code>b</code>, then an <code>IndexOutOfBoundsException</code> is
   * thrown.
   *
   * <p> If <code>len</code> is zero, then no bytes are read and
   * <code>0</code> is returned; otherwise, there is an attempt to read at
   * least one byte. If no byte is available because the stream is at end of
   * file, the value <code>-1</code> is returned; otherwise, at least one
   * byte is read and stored into <code>b</code>.
   *
   * <p> The first byte read is stored into element <code>b[off]</code>, the
   * next one into <code>b[off+1]</code>, and so on. The number of bytes read
   * is, at most, equal to <code>len</code>. Let <i>k</i> be the number of
   * bytes actually read; these bytes will be stored in elements
   * <code>b[off]</code> through <code>b[off+</code><i>k</i><code>-1]</code>,
   * leaving elements <code>b[off+</code><i>k</i><code>]</code> through
   * <code>b[off+len-1]</code> unaffected.
   *
   * <p> In every case, elements <code>b[0]</code> through
   * <code>b[off]</code> and elements <code>b[off+len]</code> through
   * <code>b[b.length-1]</code> are unaffected.
   *
   * <p> If the first byte cannot be read for any reason other than end of
   * file, then an <code>IOException</code> is thrown. In particular, an
   * <code>IOException</code> is thrown if the input stream has been closed.
   *
   * <p> The <code>read(b,</code> <code>off,</code> <code>len)</code> method
   * for class <code>InputStream</code> simply calls the method
   * <code>read()</code> repeatedly. If the first such call results in an
   * <code>IOException</code>, that exception is returned from the call to
   * the <code>read(b,</code> <code>off,</code> <code>len)</code> method.  If
   * any subsequent call to <code>read()</code> results in a
   * <code>IOException</code>, the exception is caught and treated as if it
   * were end of file; the bytes read up to that point are stored into
   * <code>b</code> and the number of bytes read before the exception
   * occurred is returned.  Subclasses are encouraged to provide a more
   * efficient implementation of this method.
   *
   * @param      b     the buffer into which the data is read.
   * @param      off   the start offset in array <code>b</code>
   *                   at which the data is written.
   * @param      len   the maximum number of bytes to read.
   * @return     the total number of bytes read into the buffer, or
   *             <code>-1</code> if there is no more data because the end of
   *             the stream has been reached.
   * @exception  IOException  if an I/O error occurs.
   * @exception  NullPointerException  if <code>b</code> is <code>null</code>.
   * @see        java.io.InputStream#read()
   */
  public final int read(byte b[], int off, int len) throws IOException
	{
  	if(length==0)	//if we've reached the end of our stream
  	{
  		return -1;
  	}
  	if(len>length)	//if they want to read more than we have
  	{
  		len=(int)length;	//only read what we have
  	}
  	int count=super.read(b, off, len);	//read the data
  	if(count>=0)	//if we haven't reached the end of the stream
  	{
  		length-=count;	//note that we've read however many bytes 
  	}
  	return count;
	}

  /**
   * Skips over and discards <code>n</code> bytes of data from this input
   * stream. The <code>skip</code> method may, for a variety of reasons, end
   * up skipping over some smaller number of bytes, possibly <code>0</code>.
   * This may result from any of a number of conditions; reaching end of file
   * before <code>n</code> bytes have been skipped is only one possibility.
   * The actual number of bytes skipped is returned.  If <code>n</code> is
   * negative, no bytes are skipped.
   *
   * <p> The <code>skip</code> method of <code>InputStream</code> creates a
   * byte array and then repeatedly reads into it until <code>n</code> bytes
   * have been read or the end of the stream has been reached. Subclasses are
   * encouraged to provide a more efficient implementation of this method.
   *
   * @param      n   the number of bytes to be skipped.
   * @return     the actual number of bytes skipped.
   * @exception  IOException  if an I/O error occurs.
   */
  public long skip(long n) throws IOException
	{
  	if(length==0)	//if we've reached the end of our stream
  	{
  		return 0;
  	}
  	if(n>length)	//if they want to skip more than we have
  	{
  		n=length;	//only skip what we have
  	}
  	long count=super.skip(n);	//skip the data
  	if(count>=0)	//if we haven't reached the end of the stream
  	{
  		length-=count;	//note that we've skipped however many bytes 
  	}
  	return count;
	}

  /**
   * Returns the number of bytes that can be read (or skipped over) from
   * this input stream without blocking by the next caller of a method for
   * this input stream.  The next caller might be the same thread or
   * another thread.
   *
   * <p> The <code>available</code> method for class <code>InputStream</code>
   * always returns <code>0</code>.
   *
   * <p> This method should be overridden by subclasses.
   *
   * @return     the number of bytes that can be read from this input stream
   *             without blocking.
   * @exception  IOException  if an I/O error occurs.
   */
  public int available() throws IOException
	{
  	return (int)Math.min(super.available(), length);	//don't return that more is available that our length 
	}

	/**Closes this input stream and releases any system resources associated with the stream.
	A closed stream cannot perform output operations and cannot be reopened.
	@exception IOException if an I/O error occurs.
	@see #beforeClose()
	@see #afterClose()
	@see #close(boolean)
	*/
	public void close() throws IOException
	{
		if(getInputStream()!=null && length>0)	//if we aren't already closed and we have more bytes left
		{
			skip(length);	//skip the remaining bytes
		}
		close(closeDecoratedStream);	//close this stream and optionally the underlying string, as configured
	}
}
