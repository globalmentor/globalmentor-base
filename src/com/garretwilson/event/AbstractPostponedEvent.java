package com.garretwilson.event;

import java.util.*;
import com.garretwilson.event.PostponedEvent;
import com.garretwilson.util.Debug;

import static com.garretwilson.lang.ObjectUtilities.*;

/**Abstract implementation of an event and the means to fire it at a later time.
@param <L> The type of listener that listens for the events.
@param <E> The type of postponed event.
@author Garret Wilson
*/
public abstract class AbstractPostponedEvent<L extends EventListener, E extends EventObject> implements PostponedEvent<E>
{

	/**Whether the postponed event has been fired.*/
	private boolean fired=false;

		/**@return Whether the postponed event has been fired.*/
		public boolean isFired() {return fired;}

	/**The manager that keeps track of event listeners, or <code>null</code> if there is no manager and therefore no listeners.*/
	private final EventListenerManager eventListenerManager;

		/**@return The manager that keeps track of event listeners, or <code>null</code> if there is no manager and therefore no listeners.*/
		protected EventListenerManager getEventListenerManager() {return eventListenerManager;}

	/**The key under which listeners have been stored in the listener manager.*/
	private final Class<L> listenerKey;

		/**@return The key under which listeners have been stored in the listener manager.*/
		protected Class<L> getListenerKey() {return listenerKey;}

	/**The event that has been postponed.*/
	private final E event;

		/**@return The event that has been postponed.*/
		public E getEvent() {return event;}

	/**Creates a postponed event.
	@param eventListenerManager The manager that keeps track of event listeners, or <code>null</code> if there is no manager and therefore no listeners.
	@param listenerKey The key under which listeners have been stored in the listener manager.
	@param event The event that has been postponed.
	@exception NullPointerException if the listener key and/or event is <code>null</code>.
	*/
	public AbstractPostponedEvent(final EventListenerManager eventListenerManager, final Class<L> listenerKey, final E event)
	{
		this.eventListenerManager=eventListenerManager;
		this.listenerKey=checkInstance(listenerKey, "Listener key cannot be null.");
		this.event=checkInstance(event, "Event cannot be null.");
	}

	/**Fires the postponed event.
	This method delegates to {@link #fireEvent(L, E)}, and concrete classes should usually override that method instead of this one.
	@exception IllegalStateException if the postponed event has already been fired.
	*/
	public void fireEvent()
	{
		synchronized(this)	//prevent race conditions around checking the fired status
		{
			if(isFired())	//if we've already fired the event
			{
				throw new IllegalStateException("Postponed event "+getEvent()+" has already been fired.");
			}
			fired=true;	//show that we've started to fire the event
		}
		final EventListenerManager eventListenerManager=getEventListenerManager();	//get the event listener manager
		if(eventListenerManager!=null)	//if we have support for listeners
		{
			final E event=getEvent();	//get the postponed event
			final Iterator<L> listeners=getEventListenerManager().getListeners(getListenerKey());	//get an iterator to the listeners
			while(listeners.hasNext())	//for each listener
			{
				fireEvent(listeners.next(), event);	//dispatch the event
			}
			
		}
	}

	/**Fires the postponed event to the provided listener.
	@param listener The listener to which the event should be fired.
	@param event The event to be dispatched.
	*/
	protected abstract void fireEvent(final L listener, final E event);
}
