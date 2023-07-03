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

package com.globalmentor.collections;

import java.util.concurrent.BlockingQueue;
import java.util.function.Consumer;

import static java.util.Objects.*;

/**
 * A runnable consumer that takes elements from a blocking queue and consume them. Normally this consumer is run in a separate thread so that consuming can
 * occur asynchronously. Each element is taken using {@link BlockingQueue#take()} and processed using {@link #accept(Object)}. This runnable's interruption
 * policy is to end execution.
 * @param <E> The type of elements held in the queue.
 * @see BlockingQueue#take()
 * @author Garret Wilson
 */
public abstract class AbstractRunnableBlockingQueueConsumer<E> implements Consumer<E>, Runnable {

	/** The blocking queue from which elements are being consumed. */
	private final BlockingQueue<E> blockingQueue;

	/** @return The blocking queue from which elements are being consumed. */
	protected BlockingQueue<E> getBlockingQueue() {
		return blockingQueue;
	}

	/**
	 * Blocking queue constructor.
	 * @param blockingQueue The blocking queue from which elements will be consumed.
	 * @throws NullPointerException if the given blocking queue is <code>null</code>.
	 */
	public AbstractRunnableBlockingQueueConsumer(final BlockingQueue<E> blockingQueue) {
		this.blockingQueue = requireNonNull(blockingQueue, "Blocking queue cannot be null.");
	}

	/** The main functionality of the consumer, which consumes data from the blocking queue and calls {@link #accept(Object)}. */
	public void run() {
		started(); //indicate that consumption has started
		while(!Thread.interrupted()) { //keep consuming until interrupted
			try {
				final E element = getBlockingQueue().take(); //take the next element from the queue
				accept(element); //consume this element
			} catch(final InterruptedException interruptedException) { //if we're interrupted while waiting
				break; //break out of the loop
			} catch(final Throwable throwable) { //if any other exception occurs
				//TODO fix log: Log.error(throwable); //log the error and continue
			}
		}
		stopped(); //indicate that consuming is finished
	}

	/** Called when the consumer is started before processing ends. */
	protected void started() {
	}

	/**
	 * Consumes an element from the queue.
	 * @param element The element to consume.
	 */
	@Override
	public abstract void accept(final E element);

	/** Called when the consumer is stopped after processing ends. */
	protected void stopped() {
	}
}
