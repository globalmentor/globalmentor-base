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

import com.globalmentor.event.*;

/**
 * An output stream that provides progress events for bytes written.
 * @author Garret Wilson
 */
public class ProgressOutputStream extends OutputStreamDecorator<OutputStream> {

	/** The object managing event listeners. */
	private final EventListenerManager eventListenerManager = new EventListenerManager();

	/** The number of bytes successfully written. */
	private long progress = 0;

	/**
	 * Decorates the given output stream.
	 * @param outputStream The output stream to decorate.
	 * @throws NullPointerException if the given stream is <code>null</code>.
	 */
	public ProgressOutputStream(final OutputStream outputStream) {
		super(outputStream); //construct the parent class
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This version delegates to {@link #write(int)} to ensure that no bytes written are noted more than once.
	 */
	@Override
	public void write(final int b) throws IOException {
		write(new byte[] {(byte)b}); //create a new byte array and write the single byte
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This version delegates to {@link #write(byte[], int, int)} to ensure that no bytes written are noted more than once.
	 */
	@Override
	public void write(final byte bytes[]) throws IOException {
		write(bytes, 0, bytes.length); //write the entire array
	}

	/**
	 * {@inheritDoc}
	 * @implNote This method is synchronized so that the progress won't be updated and/or events sent out of order.
	 */
	@Override
	public synchronized void write(final byte bytes[], final int offset, final int length) throws IOException {
		super.write(bytes, offset, length); //do the default writing
		progress += length; //increase the total progress
		fireProgressed(length, progress, -1); //indicate that bytes have been written
	}

	/**
	 * Adds a progress listener.
	 * @param progressListener The progress listener to add.
	 */
	public void addProgressListener(final ProgressListener progressListener) {
		eventListenerManager.add(ProgressListener.class, progressListener); //add the listener
	}

	/**
	 * Removes an progress listener.
	 * @param progressListener The progress listener to remove.
	 */
	public void removeProgressListener(final ProgressListener progressListener) {
		eventListenerManager.remove(ProgressListener.class, progressListener); //remove the listener
	}

	/**
	 * Fires a progress event to all registered progress listeners.
	 * @implSpec This method delegates to {@link #fireProgressed(ProgressEvent)}.
	 * @param delta The amount of recent progress, or <code>-1</code> if not known.
	 * @param value The total progress to this point, or <code>-1</code> if not known.
	 * @param maximum The goal, or <code>-1</code> if not known.
	 * @see ProgressListener
	 * @see ProgressEvent
	 */
	public void fireProgressed(final long delta, final long value, final long maximum) {
		if(eventListenerManager.hasListeners(ProgressListener.class)) { //if there are progress listeners registered
			fireProgressed(new ProgressEvent(this, delta, value, maximum)); //create and fire a new progress event
		}
	}

	/**
	 * Fires a given progress event to all registered progress listeners.
	 * @param progressEvent The progress event to fire.
	 */
	protected void fireProgressed(final ProgressEvent progressEvent) {
		for(final ProgressListener progressListener : eventListenerManager.getListeners(ProgressListener.class)) { //for each progress listener
			progressListener.progressed(progressEvent); //dispatch the progress event to the listener
		}
	}

}
