/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.event;

import java.util.*;

import static java.util.Objects.*;

import com.globalmentor.event.PostponedEvent;

/**
 * Abstract implementation of an event and the means to fire it at a later time.
 * @param <L> The type of listener that listens for the events.
 * @param <E> The type of postponed event.
 * @author Garret Wilson
 */
public abstract class AbstractPostponedEvent<L extends EventListener, E extends EventObject> implements PostponedEvent<E> {

	/** Whether the postponed event has been fired. */
	private boolean fired = false;

	/** @return Whether the postponed event has been fired. */
	public boolean isFired() {
		return fired;
	}

	/** The manager that keeps track of event listeners, or <code>null</code> if there is no manager and therefore no listeners. */
	private final EventListenerManager eventListenerManager;

	/** @return The manager that keeps track of event listeners, or <code>null</code> if there is no manager and therefore no listeners. */
	protected EventListenerManager getEventListenerManager() {
		return eventListenerManager;
	}

	/** The key under which listeners have been stored in the listener manager. */
	private final Class<L> listenerKey;

	/** @return The key under which listeners have been stored in the listener manager. */
	protected Class<L> getListenerKey() {
		return listenerKey;
	}

	/** The event that has been postponed. */
	private final E event;

	/** @return The event that has been postponed. */
	public E getEvent() {
		return event;
	}

	/**
	 * Creates a postponed event.
	 * @param eventListenerManager The manager that keeps track of event listeners, or <code>null</code> if there is no manager and therefore no listeners.
	 * @param listenerKey The key under which listeners have been stored in the listener manager.
	 * @param event The event that has been postponed.
	 * @throws NullPointerException if the listener key and/or event is <code>null</code>.
	 */
	public AbstractPostponedEvent(final EventListenerManager eventListenerManager, final Class<L> listenerKey, final E event) {
		this.eventListenerManager = eventListenerManager;
		this.listenerKey = requireNonNull(listenerKey, "Listener key cannot be null.");
		this.event = requireNonNull(event, "Event cannot be null.");
	}

	/**
	 * Fires the postponed event. This method delegates to {@link #fireEvent(EventListener, EventObject)}, and concrete classes should usually override that method instead of this one.
	 * @throws IllegalStateException if the postponed event has already been fired.
	 */
	public void fireEvent() {
		synchronized(this) { //prevent race conditions around checking the fired status
			if(isFired()) { //if we've already fired the event
				throw new IllegalStateException("Postponed event " + getEvent() + " has already been fired.");
			}
			fired = true; //show that we've started to fire the event
		}
		final EventListenerManager eventListenerManager = getEventListenerManager(); //get the event listener manager
		if(eventListenerManager != null) { //if we have support for listeners
			final E event = getEvent(); //get the postponed event
			for(final L listener : eventListenerManager.getListeners(getListenerKey())) { //for each registered event listeners
				fireEvent(listener, event); //dispatch the event
			}
		}
	}

	/**
	 * Fires the postponed event to the provided listener.
	 * @param listener The listener to which the event should be fired.
	 * @param event The event to be dispatched.
	 */
	protected abstract void fireEvent(final L listener, final E event);
}
