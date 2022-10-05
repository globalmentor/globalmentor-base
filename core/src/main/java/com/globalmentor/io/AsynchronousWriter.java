/*
 * Copyright © 1996-2012 GlobalMentor, Inc. <https://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.io;

import java.io.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.locks.*;

import static java.util.Objects.*;

import static com.globalmentor.java.Arrays.*;

/**
 * Writer that queues information and writes the information to the underlying writer asynchronously.
 * 
 * <p>
 * Information is written in the order in which character arrays are committed to the underlying queue. Characters within a particular buffer are guaranteed to
 * be written in the order received. This writer uses a producer/consumer paradigm with multiple threads and a blocking queue. This writer is thread-safe. In
 * addition, a copy is made of all data written so that any data structure, such as a character buffer, can be reused after production has occurred.
 * </p>
 * 
 * <p>
 * Because this writer does not synchronize production, the writer is not guaranteed to throw an {@link IOException} if the writer is closed by another thread
 * immediately before a write operation returns. This implies that that information written to the writer at the same time another thread closes this writer has
 * a slight possibility of being discarded with no error. If synchronized production is desired, access to this writer's production methods should be
 * synchronized externally. If access to production methods are synchronized, they should be synchronized on the lock used to construct the writer or, if no
 * custom lock was used to construct the writer, on the writer itself.
 * </p>
 * 
 * <p>
 * The {@link #close()} and {@link #drain()} operations block until finished.
 * </p>
 * 
 * <p>
 * The {@link #flush()} operation does not block until finished.
 * </p>
 * 
 * <p>
 * Whether or not this writer is synchronized, it will block when producing information if the underlying blocking queue has a fixed length and is full, as is
 * the standard contract for {@link BlockingQueue}.
 * </p>
 * 
 * @author Garret Wilson
 */
public class AsynchronousWriter extends Writer {

	/** The predefined, shared character buffer that, when produced, indicates that the underlying writer should flush. */
	protected static final char[] FLUSH_INDICATOR = new char[0];

	/** The predefined, shared character buffer that, when produced, indicates that the underlying writer should close. */
	protected static final char[] CLOSE_INDICATOR = new char[0];

	/**
	 * The predefined, shared character buffer that, when produced, indicates that all queued writes should be drained. As the appearance of this indicator
	 * indicates that the queue has already been drained, it functions as sort of a signaling no-op.
	 */
	protected static final char[] DRAIN_INDICATOR = new char[0];

	/** The underlying writer to which information will be asynchronously written. */
	private final Writer writer;

	/** @return The underlying writer to which information will be asynchronously written. */
	protected Writer getWriter() {
		return writer;
	}

	/**
	 * The underlying blocking queue used for producing and consuming information, or <code>null</code> if the underlying stream has been closed.
	 */
	private BlockingQueue<char[]> blockingQueue;

	/**
	 * @return The underlying blocking queue used for producing and consuming information, or <code>null</code> if the underlying stream has been closed.
	 */
	protected BlockingQueue<char[]> getBlockingQueue() {
		return blockingQueue;
	}

	/**
	 * The thread-safe queue of any I/O errors encountered by the consumer thread, to be returned by the production methods.
	 */
	private final Queue<IOException> ioExceptionQueue = new ConcurrentLinkedQueue<IOException>();

	/** @return Whether this writer is still open. */
	public boolean isOpen() {
		return blockingQueue != null;
	}

	/**
	 * Writer constructor with a default {@link LinkedBlockingQueue} used for data production and consumption, with critical sections synchronized on the writer
	 * itself.
	 * @param writer The writer being decorated.
	 * @throws NullPointerException if the given writer is <code>null</code>.
	 */
	public AsynchronousWriter(final Writer writer) {
		this(writer, new LinkedBlockingQueue<char[]>()); //construct the class with a default blocking queue
	}

	/**
	 * Writer and blocking queue constructor, with critical sections synchronized on the writer itself.
	 * @param writer The writer being decorated.
	 * @param blockingQueue The implementation of blocking queue to use for producing and consuming data.
	 * @throws NullPointerException if the given writer and/or blocking queue is <code>null</code>.
	 */
	public AsynchronousWriter(final Writer writer, final BlockingQueue<char[]> blockingQueue) {
		super(); //construct the parent class using this class as a lock
		this.writer = requireNonNull(writer, "Writer cannot be null.");
		this.blockingQueue = requireNonNull(blockingQueue, "Blocking queue cannot be null.");
		final Thread consumerThread = new Thread(new Consumer(), getClass().getSimpleName()); //create a new consumer thread
		consumerThread.setDaemon(true); //make the consumer thread a daemon so that it won't hold up the application when the system shuts down
		consumerThread.start(); //start the consumer thread
	}

	/**
	 * Writer and lock constructor with a default {@link LinkedBlockingQueue} used for data production and consumption.
	 * @param writer The writer being decorated.
	 * @param lock The object to synchronize on for operations such as {@link #flush()} and {@link #close()}.
	 * @throws NullPointerException if the given writer and/or lock is <code>null</code>.
	 */
	public AsynchronousWriter(final Writer writer, final Object lock) {
		this(writer, new LinkedBlockingQueue<char[]>(), lock); //construct the class with a default blocking queue
	}

	/**
	 * Writer, blocking queue, and lock constructor.
	 * @param writer The writer being decorated.
	 * @param blockingQueue The implementation of blocking queue to use for producing and consuming data.
	 * @param lock The object to synchronize on for operations such as {@link #flush()} and {@link #close()}.
	 * @throws NullPointerException if the given writer, blocking queue, and/or lock is <code>null</code>.
	 */
	public AsynchronousWriter(final Writer writer, final BlockingQueue<char[]> blockingQueue, final Object lock) {
		super(lock); //construct the parent class with the given lock
		this.writer = requireNonNull(writer, "Writer cannot be null.");
		this.blockingQueue = requireNonNull(blockingQueue, "Blocking queue cannot be null.");
	}

	/**
	 * Write a single character. The character to be written is contained in the 16 low-order bits of the given integer value; the 16 high-order bits are ignored.
	 * @param c int specifying a character to be written.
	 * @throws IOException if an I/O error occurs.
	 */
	public void write(int c) throws IOException {
		produce(new char[] { (char)c }); //always create a new character array and produce the buffer, so that we won't hold up the production process from other threads; go ahead and produce the array because we don't need to make a copy
	}

	/**
	 * Write a portion of a string.
	 * @param string The string to write.
	 * @param offset The offset from which to start writing characters.
	 * @param length The number of characters to write.
	 * @throws IOException if an I/O error occurs.
	 */
	public void write(final String string, final int offset, final int length) throws IOException {
		final char[] charBuffer = new char[length]; //create a new character buffer so that we won't hold up other threads producing information
		string.getChars(offset, offset + length, charBuffer, 0); //get the relevant characters of the string to write
		produce(charBuffer); //immediately produce the characters, because we don't need to make a copy---we're using a local copy anyway
	}

	/**
	 * Write a portion of an array of characters. A copy of the character buffer
	 * @param charBuffer The array of characters to write.
	 * @param offset The offset from which to start writing characters.
	 * @param length The number of characters to write.
	 * @throws IOException if an I/O error occurs.
	 */
	public void write(final char[] charBuffer, final int offset, final int length) throws IOException {
		produce(createCopy(charBuffer, offset, offset + length)); //create and produce a copy of the relevant characters
	}

	/** The lock used for sending synchronous indicators to the consumer. */
	private final Lock lock = new ReentrantLock();

	/** The condition to indicate that the writer has been closed. */
	private final Condition closeCondition = lock.newCondition();

	/** The condition to indicate that the writer has been drained. */
	private final Condition drainCondition = lock.newCondition();

	/**
	 * Flush the stream by writing all data to the underlying writer and then flushing the underlying writer. This implementation does not block waiting for data
	 * to be flush; flushing occurs asynchronously.
	 * @throws IOException if an I/O error occurs
	 */
	public void flush() throws IOException {
		produce(FLUSH_INDICATOR); //produce a flush indicator		
	}

	/**
	 * Drains the writer by ensuring all enqueued information has been written to the underlying writer.
	 * 
	 * @throws IOException If in I/O error occurs.
	 */
	public void drain() throws IOException {
		lock.lock(); //get a hold on the lock; this will prevent the drain status from changing without signaling the drain indicator
		try {
			produce(DRAIN_INDICATOR); //indicate that we want to drain the writer to this point
			try {
				drainCondition.await(); //wait until the consumer reaches the drain point
			} catch(final InterruptedException interruptedException) { //if we are interrupted while waiting
				throw (IOException)new IOException(interruptedException.getMessage()).initCause(interruptedException); //convert the interrupted exception to an IOException
			}
		} finally {
			lock.unlock(); //always release the lock
		}
	}

	/**
	 * Close the stream, flushing it first. This implementation does not block waiting for the writer to close; closing occurs asynchronously. Once a stream has
	 * been closed, further write() or {@link #flush()} invocations will cause an {@link IOException} to be thrown. Closing a previously-closed stream, however,
	 * has no effect. This implementation does not call this class' {@link #flush()} method, but because this operation involves flushing the underlying writer
	 * this method's data production is synchronized.
	 * @throws IOException if an I/O error occurs.
	 */
	public void close() throws IOException {
		lock.lock(); //get a hold on the lock; this will prevent the close status from changing without signaling the close indicator
		try {
			if(isOpen()) { //only close the writer if it's open; the lock prevents a race condition
				produce(CLOSE_INDICATOR); //produce a close indicator
				try {
					closeCondition.await(); //wait until the consumer reaches the close point
				} catch(final InterruptedException interruptedException) { //if we are interrupted while waiting
					throw (IOException)new IOException(interruptedException.getMessage()).initCause(interruptedException); //convert the interrupted exception to an IOException
				}
			}
		} finally {
			lock.unlock(); //always release the lock
		}
	}

	/**
	 * Queues an array of characters for asynchronous writing, throwing an exception if this writer has been closed. The calling method should not subsequently
	 * modify the contents of the buffer. No synchronization occurs in this method; any desired synchronization should occur in the caller. This implementation
	 * delegates to {@link #produce(char[])}.
	 * @param charBuffer The buffer of characters to write.
	 * @throws IOException if this writer has been closed before production begins, or if this thread was interrupted while waiting for production to complete.
	 */
	protected void produce(final char[] charBuffer) throws IOException {
		final IOException ioException = ioExceptionQueue.poll(); //get any ioException waiting to be reported
		if(ioException != null) { //if there is a waiting I/O exception
			throw (IOException)ioException.fillInStackTrace(); //add information about the program location and then throw the I/O exception
		}
		final BlockingQueue<char[]> blockingQueue = getBlockingQueue(); //get the blocking queue to use
		if(blockingQueue != null) { //if the writer is still open
			try {
				blockingQueue.put(charBuffer); //queue the character buffer for writing later
			} catch(final InterruptedException interruptedException) { //if we were interrupted while adding the buffer to the queue
				throw (IOException)new IOException(interruptedException.getMessage()).initCause(interruptedException); //convert the interrupted exception to an IOException
			}
		} else { //if the writer has already been closed and we should report an error
			throw new IOException("Information written to already closed writer.");
		}
	}

	/**
	 * The consumer runnable that writes data to the underlying writer. This runnable's interruption policy is to close the underlying writer and end execution.
	 * @author Garret Wilson
	 */
	protected class Consumer implements Runnable {

		/**
		 * The main functionality of the consumer, which consumes data from the blocking queue and writes it to the underlying writer. If
		 * {@link AsynchronousWriter#FLUSH_INDICATOR} is consumed, the underlying writer is flushed. If {@link AsynchronousWriter#CLOSE_INDICATOR} is consumed, the
		 * underlying writer is closed and the blocking queue is removed.
		 */
		public void run() {
			final BlockingQueue<char[]> blockingQueue = getBlockingQueue(); //get the blocking queue to use
			try {
				while(true) { //keep consuming until we break when interrupted
					assert blockingQueue != null : "Blocking queue unexpectedly null before asynchronous consumption has started."; //only the consumer is supposed to set the blocking queue to null
					final char[] charBuffer = blockingQueue.take(); //get the next character buffer to write
					try {
						if(charBuffer == CLOSE_INDICATOR) { //if this is the close indicator
							lock.lock(); //get a hold on the lock; this will ensure that the producer thread has reached the point where it's waiting on the close signal
							try {
								AsynchronousWriter.this.blockingQueue = null; //remove the blocking queue to indicate that this writer is closed; we'll ignore remaining information in the queue
								getWriter().close(); //close the decorated writer
								closeCondition.signal(); //signal the producer thread that we've closed
								break; //stop processing and exit the thread
							} finally {
								lock.unlock(); //always release the lock
							}
						} else if(charBuffer == FLUSH_INDICATOR) { //if this is the flush indicator
							getWriter().flush(); //flush the decorated writer
						} else if(charBuffer == DRAIN_INDICATOR) { //if the producer thread is waiting until we reach this point, and now wishes to be informed
							lock.lock(); //get a hold on the lock; this will ensure that the producer thread has reached the point where it's waiting on the drain signal
							try {
								drainCondition.signal(); //signal the producer thread that we've drained to the point it wishes
							} finally {
								lock.unlock(); //always release the lock
							}
						} else { //if this is normal data
							getWriter().write(charBuffer); //write the data to the underlying writer
						}
					} catch(final IOException ioException) { //if an I/O error occurs writing the the underlying writer
						ioExceptionQueue.add(ioException); //save the I/O exception for the producer to report it back
					}
				}
			} catch(final InterruptedException interruptedException) { //if we're interrupted while waiting 
				try {
					getWriter().close(); //close the writer
				} catch(final IOException ioException) { //if an I/O error occurs writing the the underlying writer
					ioExceptionQueue.add(ioException); //save the I/O exception for the producer to report it back
				}
			} finally {
				lock.lock(); //get a hold on the lock; this will ensure that the producer thread has reached the point where it's waiting on the close signal
				try {
					AsynchronousWriter.this.blockingQueue = null; //whatever happens, indicate that we're closed when we're finished
					closeCondition.signal(); //signal the producer thread that we've closed
				} finally {
					lock.unlock(); //always release the lock
				}
			}
		}
	}
}
