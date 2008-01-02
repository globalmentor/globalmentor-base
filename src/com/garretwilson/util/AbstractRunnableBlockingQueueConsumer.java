package com.garretwilson.util;

import java.util.concurrent.BlockingQueue;

import static com.globalmentor.java.Objects.*;

/**A runnable consumer that takes elements from a blocking queue and consume them.
Normally this consumer is run in a separate thread so that consuming can occur asynchronously.
Each element is taken using {@link BlockingQueue#take()} and processed using {@link #consume(Object)}.
This runnable's interruption policy is to end execution.
@param <E> The type of elements held in the queue.
@see BlockingQueue#take()
@author Garret Wilson
*/
public abstract class AbstractRunnableBlockingQueueConsumer<E> implements Consumer<E>, Runnable
{

	/**The blocking queue from which elements are being consumed.*/
	private final BlockingQueue<E> blockingQueue;

		/**@return The blocking queue from which elements are being consumed.*/
		public BlockingQueue<E> getBlockingQueue() {return blockingQueue;}

	/**Blocking queue constructor.
	@param blockingQueue The blocking queue from which elements will be consumed.
	@exception NullPointerException if the given blocking queue is <code>null</code>.
	*/
	public AbstractRunnableBlockingQueueConsumer(final BlockingQueue<E> blockingQueue)
	{
		this.blockingQueue=checkInstance(blockingQueue, "Blocking queue cannot be null.");
	}

	/**The main functionality of the consumer, which consumes data from the blocking queue and calls {@link #consume(Object)}.*/
	public void run()
	{
		started();	//indicate that consumption has started
		try
		{
			while(true)	//keep consuming until we break when interrupted
			{
				final E element=getBlockingQueue().take();	//take the next element from the queue
				consume(element);	//consume this element
			}
		}
		catch(final InterruptedException interruptedException)	//if we're interrupted while waiting
		{
			stopped();	//indicate that consuming is finished
			Thread.currentThread().interrupt();	//interrupt the current thread, stop processing, and exit
		}
	}

	/**Called when the consumer is started before processing ends.*/
	protected void started()
	{
	}

	/**Consumes an element from the queue.
	@param element The element to consume.
	*/
	public abstract void consume(final E element);

	/**Called when the consumer is stopped after processing ends.*/
	protected void stopped()
	{
	}
}
