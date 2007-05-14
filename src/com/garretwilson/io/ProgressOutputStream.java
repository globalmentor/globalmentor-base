package com.garretwilson.io;

import java.io.*;

import com.garretwilson.event.*;

/**An output stream that provides progres events for bytes written.
@author Garret Wilson
*/
public class ProgressOutputStream extends OutputStreamDecorator<OutputStream>
{

	/**The object managing event listeners.*/
	private final EventListenerManager eventListenerManager=new EventListenerManager();
	
	/**The number of bytes successfully written.*/
	private long progress=0;

	/**Decorates the given output stream.
	@param outputStream The output stream to decorate.
	@exception NullPointerException if the given stream is <code>null</code>.
	*/
	public ProgressOutputStream(final OutputStream outputStream)
	{
		super(outputStream);	//construct the parent class
	}

	/**Writes the specified byte to the decorated output stream.
	This version delegates to {@link #write(int)} to ensure that no bytes written are noted more than once
	@param b The byte to write.
	@exception IOException if an I/O error occurs.
	*/
	public void write(final int b) throws IOException
	{
		write(new byte[]{(byte)b});	//create a new byte array and write the single byte
	}

	/**Writes all the bytes from the specified byte array to the decorated output stream.
	This version delegates to {@link #write(byte[], int, int)} to ensure that no bytes written are noted more than once
	@param byes The bytes to write.
	@exception NullPointerException if the given byte array is <code>null</code>.
	@exception IOException if an I/O error occurs.
	*/
  public void write(final byte bytes[]) throws IOException
	{
  	write(bytes, 0, bytes.length);	//write the entire array
  }

	/**Writes specified number of > bytes from the specified byte array to the decorated doutput stream starting at the given offset. 
	@param byes The bytes to write.
	@param offset The start offset in the data.
	@param length The number of bytes to write.
	@exception NullPointerException if the given byte array is <code>null</code>.
	@exception IndexOutOfBoundsException if the given offset is negative, or the given offset plus the given langth is greater than the length of the given array.  
	@exception IOException if an I/O error occurs.
	*/
  public synchronized void write(final byte bytes[], final int offset, final int length) throws IOException	//this method is synchronized so that the progress won't be updated and/or events sent out of order
	{
  	super.write(bytes, offset, length);	//do the default writing
  	progress+=length;	//increase the total progress
  	fireProgressed(length, progress, -1);	//indicate that bytes have been written
	}

  /**Called before the stream is closed.
	This version fires a final progress event indicating that the maximum value is now known and has been reached
	(i.e. the progress reflects the maximum value), indicating no delta bytes written.
	@exception IOException if an I/O error occurs.
	*/
/*TODO del if not needed or wanted
  protected synchronized void beforeClose() throws IOException 
  {
  	fireProgressed(0, progress, progress);	//indicate that bytes have been written
  }
*/

	/**Adds a progress listener.
	@param progressListener The progress listener to add.
	*/
	public void addProgressListener(final ProgressListener progressListener)
	{
		eventListenerManager.add(ProgressListener.class, progressListener);	//add the listener
	}

	/**Removes an progress listener.
	@param progressListener The progress listener to remove.
	*/
	public void removeProgressListener(final ProgressListener progressListener)
	{
		eventListenerManager.remove(ProgressListener.class, progressListener);	//remove the listener
	}

	/**Fires a progress event to all registered progress listeners.
	This method delegates to {@link #fireProgessed(ProgressEvent)}.
	@param delta The amount of recent progress, or <code>-1</code> if not known.
	@param value The total progress to this point, or <code>-1</code> if not known.
	@param maximum The goal, or <code>-1</code> if not known.
	@see ProgressListener
	@see ProgressEvent
	*/
	public void fireProgressed(final long delta, final long value, final long maximum)
	{
		if(eventListenerManager.hasListeners(ProgressListener.class))	//if there are progress listeners registered
		{
			fireProgressed(new ProgressEvent(this, delta, value, maximum));	//create and fire a new progress event
		}
	}

	/**Fires a given progress event to all registered progress listeners.
	@param progressEvent The progress event to fire.
	*/
	protected void fireProgressed(final ProgressEvent progressEvent)
	{
		for(final ProgressListener progressListener:eventListenerManager.getListeners(ProgressListener.class))	//for each progress listener
		{
			progressListener.progressed(progressEvent);	//dispatch the progress event to the listener
		}
	}

}
