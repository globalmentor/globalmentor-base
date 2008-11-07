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

import static com.globalmentor.java.Objects.*;

/**Wraps an existing input stream.
The decorated input stream is released when this stream is closed.
This decorator provides convenience methods {@link #beforeClose()} and {@link #afterClose()} called before and after the stream is closed, respectively.
@param <I> The type of input stream being decorated.
@author Garret Wilson
*/
public class InputStreamDecorator<I extends InputStream> extends InputStream
{
	
	/**The input stream being decorated.*/
	private I inputStream;

		/**@return The input stream being decorated, or <code>null</code> if it has been released after this stream was closed.*/
		protected I getInputStream() {return inputStream;}

	/**Decorates the given input stream.
	@param inputStream The input stream to decorate.
	@exception NullPointerException if the given stream is <code>null</code>.
	*/
	public InputStreamDecorator(final I inputStream)
	{
		this.inputStream=checkInstance(inputStream, "Input stream cannot be null.");	//save the decorated input stream
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
  	final InputStream inputStream=getInputStream();	//get the decorated input stream
 		return inputStream!=null ? inputStream.read() : -1;	//if there is no decorated input stream, indicate that the stream is closed by returning -1
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
  public int read(byte b[]) throws IOException
	{
  	final InputStream inputStream=getInputStream();	//get the decorated input stream
 		return inputStream!=null ? inputStream.read(b) : -1;	//if there is no decorated input stream, indicate that the stream is closed by returning -1
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
  public int read(byte b[], int off, int len) throws IOException
	{
  	final InputStream inputStream=getInputStream();	//get the decorated input stream
 		return inputStream!=null ? inputStream.read(b, off, len) : -1;	//if there is no decorated input stream, indicate that the stream is closed by returning -1
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
  	final InputStream inputStream=getInputStream();	//get the decorated input stream
  	return inputStream!=null ? inputStream.skip(n) : 0;
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
  	final InputStream inputStream=getInputStream();	//get the decorated input stream
 		return inputStream!=null ? inputStream.available() : 0;
	}

  /**
   * Marks the current position in this input stream. A subsequent call to
   * the <code>reset</code> method repositions this stream at the last marked
   * position so that subsequent reads re-read the same bytes.
   *
   * <p> The <code>readlimit</code> arguments tells this input stream to
   * allow that many bytes to be read before the mark position gets
   * invalidated.
   *
   * <p> The general contract of <code>mark</code> is that, if the method
   * <code>markSupported</code> returns <code>true</code>, the stream somehow
   * remembers all the bytes read after the call to <code>mark</code> and
   * stands ready to supply those same bytes again if and whenever the method
   * <code>reset</code> is called.  However, the stream is not required to
   * remember any data at all if more than <code>readlimit</code> bytes are
   * read from the stream before <code>reset</code> is called.
   *
   * <p> The <code>mark</code> method of <code>InputStream</code> does
   * nothing.
   *
   * @param   readlimit   the maximum limit of bytes that can be read before
   *                      the mark position becomes invalid.
   * @see     java.io.InputStream#reset()
   */
  public synchronized void mark(int readlimit)
	{
  	final InputStream inputStream=getInputStream();	//get the decorated input stream
  	if(inputStream!=null)	//if we still have an input stream to decorate
  	{
  		inputStream.mark(readlimit);
  	}
	}

  /**
   * Repositions this stream to the position at the time the
   * <code>mark</code> method was last called on this input stream.
   *
   * <p> The general contract of <code>reset</code> is:
   *
   * <p><ul>
   *
   * <li> If the method <code>markSupported</code> returns
   * <code>true</code>, then:
   *
   *     <ul><li> If the method <code>mark</code> has not been called since
   *     the stream was created, or the number of bytes read from the stream
   *     since <code>mark</code> was last called is larger than the argument
   *     to <code>mark</code> at that last call, then an
   *     <code>IOException</code> might be thrown.
   *
   *     <li> If such an <code>IOException</code> is not thrown, then the
   *     stream is reset to a state such that all the bytes read since the
   *     most recent call to <code>mark</code> (or since the start of the
   *     file, if <code>mark</code> has not been called) will be resupplied
   *     to subsequent callers of the <code>read</code> method, followed by
   *     any bytes that otherwise would have been the next input data as of
   *     the time of the call to <code>reset</code>. </ul>
   *
   * <li> If the method <code>markSupported</code> returns
   * <code>false</code>, then:
   *
   *     <ul><li> The call to <code>reset</code> may throw an
   *     <code>IOException</code>.
   *
   *     <li> If an <code>IOException</code> is not thrown, then the stream
   *     is reset to a fixed state that depends on the particular type of the
   *     input stream and how it was created. The bytes that will be supplied
   *     to subsequent callers of the <code>read</code> method depend on the
   *     particular type of the input stream. </ul></ul>
   *
   * <p>The method <code>reset</code> for class <code>InputStream</code>
   * does nothing except throw an <code>IOException</code>.
   *
   * @exception  IOException  if this stream has not been marked or if the
   *               mark has been invalidated.
   * @see     java.io.InputStream#mark(int)
   * @see     java.io.IOException
   */
  public synchronized void reset() throws IOException
	{
  	final InputStream inputStream=getInputStream();	//get the decorated input stream
  	if(inputStream!=null)	//if we still have an input stream to decorate
  	{
  		inputStream.reset();
  	}
	}

  /**
   * Tests if this input stream supports the <code>mark</code> and
   * <code>reset</code> methods. Whether or not <code>mark</code> and
   * <code>reset</code> are supported is an invariant property of a
   * particular input stream instance. The <code>markSupported</code> method
   * of <code>InputStream</code> returns <code>false</code>.
   *
   * @return  <code>true</code> if this stream instance supports the mark
   *          and reset methods; <code>false</code> otherwise.
   * @see     java.io.InputStream#mark(int)
   * @see     java.io.InputStream#reset()
   */
  public boolean markSupported()
	{
  	final InputStream inputStream=getInputStream();	//get the decorated input stream
 		return inputStream!=null ? inputStream.markSupported() : false;
	}
	
  /**Called before the stream is closed.
	@exception IOException if an I/O error occurs.
	*/
  protected void beforeClose() throws IOException 
  {
  }

  /**Called after the stream is successfully closed.
	@exception IOException if an I/O error occurs.
	*/
  protected void afterClose() throws IOException
  {
  }

	/**Closes this input stream and releases any system resources associated with the stream.
	A closed stream cannot perform output operations and cannot be reopened.
	@param closeDecoratedStream Whether the decorated stream should also be closed.
	@exception IOException if an I/O error occurs.
	@see #beforeClose()
	@see #afterClose()
	*/
	public synchronized void close(final boolean closeDecoratedStream) throws IOException	//this method is synchronized so that the closing operation can complete without being bothered by other threads
	{
  	final InputStream inputStream=getInputStream();	//get the decorated input stream
  	if(inputStream!=null)	//if we still have an input stream to decorate
  	{
  		beforeClose();	//perform actions before closing
  		if(closeDecoratedStream)
  		{
  			inputStream.close();	//close the decorated input stream
  		}
  		this.inputStream=null;	//release the decorated input stream if closing was successful
  		afterClose();	//perform actions after closing
  	}
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
		close(true);	//close this stream and the underlying stream
	}
}
