package com.garretwilson.io;

import java.io.*;
import static com.garretwilson.lang.ObjectUtilities.*;

/**Wraps an existing output stream.
The decorated output stream is released when this stream is closed.
This decorator provides convenience methods {@link #beforeClose()} and {@link #afterClose()} called before and after the stream is closed, respectively.
@author Garret Wilson
*/
public abstract class OutputStreamDecorator<O extends OutputStream> extends OutputStream
{
	
	/**The output stream being decorated.*/
	private O outputStream;

		/**@return The output stream being decorated, or <code>null</code> if it has been released after this stream was closed.*/
		protected O getOutputStream() {return outputStream;}

	/**Decorates the given output stream.
	@param outputStream The output stream to decorate.
	@exception NullPointerException if the given stream is <code>null</code>.
	*/
	public OutputStreamDecorator(final O outputStream)
	{
		this.outputStream=checkInstance(outputStream, "Output stream cannot be null.");	//save the decorated output stream
	}

  /**
   * Writes the specified byte to this output stream. The general 
   * contract for <code>write</code> is that one byte is written 
   * to the output stream. The byte to be written is the eight 
   * low-order bits of the argument <code>b</code>. The 24 
   * high-order bits of <code>b</code> are ignored.
   * <p>
   * Subclasses of <code>OutputStream</code> must provide an 
   * implementation for this method. 
   *
   * @param      b   the <code>byte</code>.
   * @exception  IOException  if an I/O error occurs. In particular, 
   *             an <code>IOException</code> may be thrown if the 
   *             output stream has been closed.
   */
	public void write(int b) throws IOException
	{
  	final OutputStream outputStream=getOutputStream();	//get the decorated output stream
  	if(outputStream!=null)	//if we still have an output stream to decorate
  	{
  		outputStream.write(b);
  	}
	}

  /**
   * Writes <code>b.length</code> bytes from the specified byte array 
   * to this output stream. The general contract for <code>write(b)</code> 
   * is that it should have exactly the same effect as the call 
   * <code>write(b, 0, b.length)</code>.
   *
   * @param      b   the data.
   * @exception  IOException  if an I/O error occurs.
   * @see        java.io.OutputStream#write(byte[], int, int)
   */
  public void write(byte b[]) throws IOException
	{
  	final OutputStream outputStream=getOutputStream();	//get the decorated output stream
  	if(outputStream!=null)	//if we still have an output stream to decorate
  	{
  		outputStream.write(b);
  	}
  }

  /**
   * Writes <code>len</code> bytes from the specified byte array 
   * starting at offset <code>off</code> to this output stream. 
   * The general contract for <code>write(b, off, len)</code> is that 
   * some of the bytes in the array <code>b</code> are written to the 
   * output stream in order; element <code>b[off]</code> is the first 
   * byte written and <code>b[off+len-1]</code> is the last byte written 
   * by this operation.
   * <p>
   * The <code>write</code> method of <code>OutputStream</code> calls 
   * the write method of one argument on each of the bytes to be 
   * written out. Subclasses are encouraged to override this method and 
   * provide a more efficient implementation. 
   * <p>
   * If <code>b</code> is <code>null</code>, a 
   * <code>NullPointerException</code> is thrown.
   * <p>
   * If <code>off</code> is negative, or <code>len</code> is negative, or 
   * <code>off+len</code> is greater than the length of the array 
   * <code>b</code>, then an <tt>IndexOutOfBoundsException</tt> is thrown.
   *
   * @param      b     the data.
   * @param      off   the start offset in the data.
   * @param      len   the number of bytes to write.
   * @exception  IOException  if an I/O error occurs. In particular, 
   *             an <code>IOException</code> is thrown if the output 
   *             stream is closed.
   */
  public void write(byte b[], int off, int len) throws IOException
	{
  	final OutputStream outputStream=getOutputStream();	//get the decorated output stream
  	if(outputStream!=null)	//if we still have an output stream to decorate
  	{
  		outputStream.write(b, off, len);
  	}
  }

  /**
   * Flushes this output stream and forces any buffered output bytes 
   * to be written out. The general contract of <code>flush</code> is 
   * that calling it is an indication that, if any bytes previously 
   * written have been buffered by the implementation of the output 
   * stream, such bytes should immediately be written to their 
   * intended destination.
   * <p>
   * If the intended destination of this stream is an abstraction provided by
   * the underlying operating system, for example a file, then flushing the
   * stream guarantees only that bytes previously written to the stream are
   * passed to the operating system for writing; it does not guarantee that
   * they are actually written to a physical device such as a disk drive.
   * <p>
   * The <code>flush</code> method of <code>OutputStream</code> does nothing.
   *
   * @exception  IOException  if an I/O error occurs.
   */
  public void flush() throws IOException
	{
  	final OutputStream outputStream=getOutputStream();	//get the decorated output stream
  	if(outputStream!=null)	//if we still have an output stream to decorate
  	{
  		outputStream.flush();
  	}
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

	/**Closes this output stream and releases any system resources associated with this stream. 
	A closed stream cannot perform output operations and cannot be reopened.
	@exception IOException if an I/O error occurs.
	@see #beforeClose()
	@see #afterClose()
	*/
  public synchronized void close() throws IOException	//this method is synchronized so that the colsing operation can complete without being bothered by other threads 
	{
  	final OutputStream outputStream=getOutputStream();	//get the decorated output stream
  	if(outputStream!=null)	//if we still have an output stream to decorate
  	{
  		beforeClose();	//perform actions before closing
  		outputStream.close();	//close the decorated output stream
  		this.outputStream=null;	//release the decorated output stream if closing was successful
  		afterClose();	//perform actions after closing
  	}
	}
}
