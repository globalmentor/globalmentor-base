package com.garretwilson.io;

import java.io.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.locks.*;

import static com.garretwilson.lang.ObjectUtilities.*;
import static com.garretwilson.util.ArrayUtilities.*;

/**Writer that queues information and writes the information to the underlying writer asynchronously.

<p>Information is written in the order in which character arrays are committed to the underlying queue.
Characters within a particular buffer are guaranteed to be written in the order received.
This writer uses a producer/consumer paradigm with multiple threads and a blocking queue.
This writer is thread-safe. In addition, a copy is made of all data written so that any data structure, such as a character buffer, can be reused after production has occurred.</p>

<p>Because this writer does not synchronize production, the writer is not guaranteed to throw an {@link IOException} if the writer is closed by another thread immediately before a write operation returns.
This implies that that information written to the writer at the same time another thread closes this writer has a slight possibility of being discarded with no error.
If synchronized production is desired, access to this writer's production methods should be synchronized externally.
If access to production methods are synchronized, they should be synchronized on the lock used to construct the writer or, if no custom lock was used to construct the writer, on the writer itself.</p>

<p>This {@link #flush()} and {@link #close()} operations do not block until finished.</p>

<p>Whether or not this writer is synchronized, it will block when producing information if the underlying blocking queue has a fixed length and is full, as is the standard contract for {@link BlockingQueue}.</p>

@author	Garret Wilson
*/
public class AsynchronousWriter extends Writer
{

	/**The predefined, shared character buffer that, when produced, indicates that the underlying writer should flush.*/
	protected final static char[] FLUSH_INDICATOR=new char[0];

	/**The predefined, shared character buffer that, when produced, indicates that the underlying writer should close.*/
	protected final static char[] CLOSE_INDICATOR=new char[0];

	/**The underlying writer to which information will be asynchronously written.*/
	private final Writer writer;

		/**@return The underlying writer to which information will be asynchronously written.*/
		protected Writer getWriter() {return writer;}

	/**The underlying blocking queue used for producing and consuming information, or <code>null</code> if the underlying stream has been closed.*/
	private BlockingQueue<char[]> blockingQueue;

		/**@return The underlying blocking queue used for producing and consuming information, or <code>null</code> if the underlying stream has been closed.*/
		protected BlockingQueue<char[]> getBlockingQueue() {return blockingQueue;}		

	/**The thread-safe queue of any I/O errors encountered by the consumer thread, to be returned by the production methods.*/
	private final Queue<IOException> ioExceptionQueue=new ConcurrentLinkedQueue<IOException>();
		
	/**Writer constructor with a default {@link LinkedBlockingQueue} used for data production and consumption, with critical sections synchronized on the writer itself.
	@param writer The writer being decorated.
	@exception NullPointerException if the given writer is <code>null</code>.
	*/
	public AsynchronousWriter(final Writer writer)
	{
		this(writer, new LinkedBlockingQueue<char[]>());	//construct the class with a default blocking queue
	}

	/**Writer and blocking queue constructor, with critical sections synchronized on the writer itself.
	@param writer The writer being decorated.
	@param blockingQueue The implementation of blocking queue to use for producing and consuming data.
	@exception NullPointerException if the given writer and/or blocking queue is <code>null</code>.
	*/
	public AsynchronousWriter(final Writer writer, final BlockingQueue<char[]> blockingQueue)
	{
		super();	//construct the parent class using this class as a lock
		this.writer=checkInstance(writer, "Writer cannot be null.");
		this.blockingQueue=checkInstance(blockingQueue, "Blocking queue cannot be null.");
		final Thread consumerThread=new Thread(new Consumer(), "asynchthread");	//create a new consumer thread TODO fix name
		consumerThread.setDaemon(true);	//make the consumer thread a daemon so that it won't hold up the application when the system shuts down
		consumerThread.start();	//start the consumer thread
	}

	/**Writer and lock constructor with a default {@link LinkedBlockingQueue} used for data production and consumption.
	@param writer The writer being decorated.
	@param lock The object to synchronize on for operations such as {@link #flush()} and {@link #close()}.
	@exception NullPointerException if the given writer and/or lock is <code>null</code>.
	*/
	public AsynchronousWriter(final Writer writer, final Object lock)
	{
		this(writer, new LinkedBlockingQueue<char[]>(), lock);	//construct the class with a default blocking queue
	}

	/**Writer, blocking queue, and lock constructor.
	@param writer The writer being decorated.
	@param blockingQueue The implementation of blocking queue to use for producing and consuming data.
	@param lock The object to synchronize on for operations such as {@link #flush()} and {@link #close()}.
	@exception NullPointerException if the given writer, blocking queue, and/or lock is <code>null</code>.
	*/
	public AsynchronousWriter(final Writer writer, final BlockingQueue<char[]> blockingQueue, final Object lock)
	{
		super(lock);	//construct the parent class with the given lock
		this.writer=checkInstance(writer, "Writer cannot be null.");
		this.blockingQueue=checkInstance(blockingQueue, "Blocking queue cannot be null.");
	}

	/**Write a single character.
	The character to be written is contained in the 16 low-order bits of the given integer value; the 16 high-order bits are ignored.
	@param c int specifying a character to be written.
	@exception IOException if an I/O error occurs.
	*/
	public void write(int c) throws IOException
	{
		produce(new char[]{(char)c});	//always create a new character array and produce the buffer, so that we won't hold up the production process from other threads; go ahead and produce the array because we don't need to make a copy
	}

	/**Write a portion of a string.
	@param string The string to write.
	@param offset The offset from which to start writing characters.
	@param length The number of characters to write.
	@exception IOException if an I/O error occurs.
	*/
	public void write(final String string, final int offset, final int length) throws IOException
	{
		final char[] charBuffer=new char[length];	//create a new character buffer so that we won't hold up other threads producing information
		string.getChars(offset, offset+length, charBuffer, 0);	//get the relevant characters of the string to write
		produce(charBuffer);	//immediately produce the characters, because we don't need to make a copy---we're using a local copy anyway
	}

	/**Write a portion of an array of characters.
	A copy of the character buffer
	@param charBuffer The array of characters to write.
	@param offset The offset from which to start writing characters.
	@param length The number of characters to write.
	@exception IOException if an I/O error occurs.
	*/
	public void write(final char[] charBuffer, final int offset, final int length) throws IOException
	{
		produce(createCopy(charBuffer, offset, length));	//create and produce a copy of the relevant characters
	}

	/**The lock used for draining the writer.*/
	private final Lock drainLock=new ReentrantLock();

	/**The condition to indicate that the writer has been drained.*/
	private final Condition drainCondition=drainLock.newCondition();
	
	/**Flush the stream by writing all data to the underlying writer and then flushing the underlying writer.
	This implementation does not block waiting for data to be flush; flushing occurs asynchronously.
	@exception IOException if an I/O error occurs
	*/
	public void flush() throws IOException
	{
		produce(FLUSH_INDICATOR);	//produce a flush indicator		
	}

	/**Close the stream, flushing it first.
	This implementation does not block waiting for the writer to close; closing occurs asynchronously.
	Once a stream has been closed, further write() or {@link #flush()} invocations will cause an {@link IOException} to be thrown.
	Closing a previously-closed stream, however, has no effect.
	This implementation does not call this class' {@link #flush()} method, but because this operation involves flushing the underlying writer this method's data production is synchronized.
	@exception IOException if an I/O error occurs.
	*/
	public void close() throws IOException
	{
		produce(CLOSE_INDICATOR, false);	//produce a close indicator, but don't indicate an error if the writer is already closed
/*TODO fix
		drainLock.lock();	//get a hold on the drain lock
		try
		{
			produce(DRAIN_INDICATOR);	//indicate that we want to drain the writer to this point
			drainCondition.await();	//wait until the consumer reaches the drain point			
		}
		finally
		{
			drainLock.unlock();	//always release the drain lock
		}
*/
	}

  /**Queues an array of characters for asynchronous writing, throwing an exception if this writer has been closed.
	The calling method should not subsequently modify the contents of the buffer.
	No synchronization occurs in this method; any desired synchronization should occur in the caller.
	This implementation delegates to {@link #produce(char[], boolean)}.
	@param charBuffer The buffer of characters to write.
	@exception IOException if this writer has been closed before production begins, or if this thread was interrupted while waiting for production to complete.
	*/
	protected void produce(final char[] charBuffer) throws IOException
  {
		produce(charBuffer, true);	//produce the buffer, indicating an error if the writer is closed
  }

  /**Queues an array of characters for asynchronous writing.
	The calling method should not subsequently modifying the contents of the buffer.
	No synchronization occurs in this method; any desired synchronization should occur in the caller.
	@param charBuffer The buffer of characters to write.
	@param errorOnClosed <code>true</code> if a closed condition should cause an error
	@exception IOException if an error has occurred earlier in the asynchronous writing, if this writer has been closed before production begins and production on close should be an error condition, or if this thread was interrupted while waiting for production to complete.
	*/
	protected void produce(final char[] charBuffer, final boolean errorOnClose) throws IOException
  {
		final IOException ioException=ioExceptionQueue.poll();	//get any ioException waiting to be reported
		if(ioException!=null)	//if there is a waiting I/O exception
		{
			throw (IOException)ioException.fillInStackTrace();	//add information about the program location and then throw the I/O exception
		}
  	final BlockingQueue<char[]> blockingQueue=getBlockingQueue();	//get the blocking queue to use
  	if(blockingQueue!=null)	//if the writer is still open
  	{
	  	try
			{
				blockingQueue.put(charBuffer);	//queue the character buffer for writing later
			}
	  	catch(final InterruptedException interruptedException)	//if we were interrupted while adding the buffer to the queue
			{
	  		throw (IOException)new IOException(interruptedException.getMessage()).initCause(interruptedException);	//convert the interrupted exception to an IOException
			}
  	}
  	else if(errorOnClose)	//if the writer has already been closed and we should report an error
  	{
  		throw new IOException("Information written to already closed writer.");
  	}
  }

	/**Cleans up the asynchronous writer by ensuring that the writer is closed and thereby the consumer thread is ended.
	@see #close()
	*/
	protected void finalize() throws Throwable
	{
		try
		{
			close();	//flush and close the writer, if it isn't closed already
		}
		finally
		{
			super.finalize();	//always call the parent version
		}			
	}

	/**The consumer runnable that writes data to the underlying writer.
	This runnable's interruption policy is to close the underlying writer and end execution.
	@author Garret Wilson
	*/
	protected class Consumer implements Runnable
	{

		/**The main functionality of the consumer, which consumes data from the blocking queue and writes it to the underlying writer.
		If {@link AsynchronousWriter#FLUSH_INDICATOR} is consumed, the underlying writer is flushed.
		If {@link AsynchronousWriter#CLOSE_INDICATOR} is consumed, the underlying writer is closed and the blocking queue is removed.
		*/
		public void run()
		{
			while(true)	//keep consuming until we break manually
			{
				final BlockingQueue<char[]> blockingQueue=getBlockingQueue();	//get the blocking queue to use
				assert blockingQueue!=null : "Blocking queue unexpectedly null before asynchronous consumption has started.";	//only the consumer is supposed to set the blocking queue to null
				try
				{
					final char[] charBuffer=blockingQueue.take();	//get the next character buffer to write
					try
					{
						if(charBuffer==CLOSE_INDICATOR)	//if this is the close indicator
						{
//TODO del if not needed							synchronized(lock)	//synchronize on the lock, in case this writer is synchronized externally, so that if another thread is attempting to write we'll remove the queue before they can grab it, and then they'll know the writer is closed
							{
								AsynchronousWriter.this.blockingQueue=null;	//remove the blocking queue to indicate that this writer is closed; we'll ignore remaining information in the queue
								getWriter().close();	//close the decorated writer
								break;	//stop processing and exit the thread
							}
						}
						else if(charBuffer==FLUSH_INDICATOR)	//if this is the flush indicator
						{
							getWriter().flush();	//flush the decorated writer
						}
/*TODO fix
						else if(charBuffer==DRAIN_INDICATOR)	//if the producer thread is waiting until we reach this point, and now wishes to be informed
						{
							drainLock.lock();	//get a hold on the drain lock; this will ensure that the producer thread has reached the point where it's waiting on the drain signal
							try
							{
								drainCondition.signal();	//signal the producer thread that we've drained to the point it wishes
							}
							finally
							{
								drainLock.unlock();	//always release the drain lock
							}					
						}
*/
						else	//if this is normal data
						{
							getWriter().write(charBuffer);	//write the data to the underlying writer
						}
					}
					catch(final IOException ioException)	//if an I/O error occurs writing the the underlying writer
					{
						ioExceptionQueue.add(ioException);	//save the I/O exception for the producer to report it back
					}
				}
				catch(final InterruptedException interruptedException)	//if we're interrupted while waiting 
				{
					try
					{
						getWriter().close();	//close the writer
					}
					catch(final IOException ioException)	//if an I/O error occurs writing the the underlying writer
					{
						ioExceptionQueue.add(ioException);	//save the I/O exception for the producer to report it back
					}						
					break;	//stop processing and exit the thread
				}
			}
		}
	}
}
