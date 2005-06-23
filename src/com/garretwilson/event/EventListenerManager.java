package com.garretwilson.event;

import java.util.*;
import static java.util.Collections.*;

/**Class that stores and retrieves event listeners, facilitating the creation
	of another class that allows event listeners to be registered with it.
<p>This class maintains weak references to event listeners so that they may be
	collected by the garbage collector when they are no longer in ordinary use. TODO remove comment</p>
<p>A class is used as a key to a set of event listeners, all of which must be instances of that class or a subclass.
	Generics are used to ensure that only instances of the class or subclasses are keyed to a particular class.
	If the event listener type itself is generic, using classes should ensure that all listeners keyed to a particular
	class are indeed generic subclasses of the class, as Java 5 <code>Class&lt;<var>T</var>&gt;</code> objects do not keep track
	of the generic type of <var>T</var>.</p> 
<p>This class uses thread-safe access methods. Returned listener sets are
	 "snapshots" of currently registered listeners, so may be accessed even
	 though other threads (or even the event listener itself) may add and/or
	 remove listeners.
	Example:
	<blockquote><pre><code>
	final Set&lt;MyListener&gt; myListeners=getListeners(MyListener.class);
	if(!myListeners.isEmpty())
	{
		final MyEvent myEvent=new MyEvent();
		for(final MyListener myListener:myListeners)
		{
			myListener.fireEvent(myEvent);
		}
	}
	</pre></code></blockquote>
</p>
<p>This class uses little memory if there are no registered event listeners.</p>
<p>This class was inspired by <code>javax.swing.EventListenerList</code> 1.33 12/03/01 by Georges Saab, Hans Muller, and James Gosling.</p>
@author Garret Wilson
@see javax.swing.EventListenerList
*/
public class EventListenerManager	//TODO fix to not use WeakHashSet, which isn't a good idea, as anonymous listeners may not be referenced after being added to this class
{

	//Any access to a set is syncrhonized.

	//Rather than synchronizing on the map, each method that accesses the map is
	//	synchronized because the map can be created and destroyed.

	/**The map containing weak sets of event listeners; only allocated when needed.*/
	private Map<Class<? extends EventListener>, Set<? extends EventListener>> listenerSetMap=null;

	/**Retrieves the set of listeners associated with the given key.
	If no listener set map or no listener set exists, it will be created.
	@param key The key with which the listeners would be associated.
	@return The set in which listeners associated with the given key are stored.
	*/
	@SuppressWarnings("unchecked")
	protected synchronized <T extends EventListener> Set<T> getListenerSet(final Class<T> key)
	{
		if(listenerSetMap==null)	//if there is no map of listener sets
		{
			listenerSetMap=new HashMap<Class<? extends EventListener>, Set<? extends EventListener>>();	//create a map of listener sets
		}
		Set<T> listenerSet=(Set<T>)listenerSetMap.get(key);	//get the set of listeners associated with the key; we will have only stored subclasses of the class keyed to the given key
		if(listenerSet==null)	//if there is no set of listeners associated with the key
		{
//TODO fix weak hash set, which will not work for anonymous classes with no other references			listenerSet=Collections.synchronizedSet(new WeakHashSet());	//create a new synchronized weak set in which to store the listeners
			listenerSet=Collections.synchronizedSet(new HashSet<T>());	//create a new synchronized weak set in which to store the listeners
			listenerSetMap.put(key, listenerSet);	//store the set in the map keyed to the key
		}
		return listenerSet;	//return the set of listeners
	}

	/**Checks to see if a given listener set is empty and if so removes it from
		from the map. If the map is consequently empty, the map is removed.
	@param key The key with which the listener set is associated.
	@param listenerSet The set of listeners to check
	*/
	protected synchronized <T extends EventListener, L extends T> void checkListenerSet(final Class<T> key, final Set<L> listenerSet)
	{
		synchronized(listenerSet)	//don't allow other threads to access the set while we access it
		{
			if(listenerSet.size()==0)	//if the listener set has no elements
			{
				listenerSetMap.remove(key);	//remove the now empty set of listeners
				if(listenerSetMap.size()==0)	//if there are no more sets of listeners
				{
					listenerSetMap=null;	//remove the entire map of listener sets
				}
			}
		}
	}

	/**Adds a listener to the manager, associated with the given key.
	<p>Example: <code>add(MyListener.class, listener);</code>.</p>
	@param key The key with which the listener should be associated.
	@param listener The event listener with which to associated the listener.
	*/
	public synchronized <T extends EventListener, L extends T> void add(final Class<T> key, final L listener)	//use generics to make sure that the listener is a subclass of the given type
	{
		final Set<T> listenerSet=getListenerSet(key);	//get the set of listeners associated with the given key, creating a map and/or set if needed
		synchronized(listenerSet)	//don't allow other threads to access the set while we access it
		{
			listenerSet.add(listener);	//add the listener to the set of listeners
		}		 
	}

	/**Removes a listener from the manager as associated with the given key.
	If the listener is not associated with the key, no action is taken.
	<p>Example: <code>remove(MyListener.class, listener);</code>
	@param key The key with which the listener was associated.
	@param listener The listener to remove from the manager.
	@return <code>true</code> if the manager contained the specified listener.
	*/
	@SuppressWarnings("unchecked")
	public synchronized <T extends EventListener, L extends T> boolean remove(final Class<T> key, final L listener)
	{
		boolean containedListener=false;	//start out assuming we don't have the listener
		if(listenerSetMap!=null)	//if we have a map of listener sets
		{
			final Set<T> listenerSet=(Set<T>)listenerSetMap.get(key);	//get the set of listeners associated with this key; we will have only stored subclasses of the class keyed to the given key
			if(listenerSet!=null)	//if there is a set of listeners associated with this key
			{
				synchronized(listenerSet)	//don't allow other threads to access the set while we access it
				{
					containedListener=listenerSet.remove(listener);	//remove the given listener
					checkListenerSet(key, listenerSet);	//remove the listener set and the map if they are no longer needed
				}
			}
		}
		return containedListener;	//return whether the manager contained the listener before it was removed
	}

	/**Returns the number of listeners associated with the given key.
	<p>Example: <code>getListenerCount(MyListener.class);</code>
	@param key The key with which the listeners are associated.
	@return Thte number of listeners associated with the given key.
	*/
	@SuppressWarnings("unchecked")
	public synchronized <T extends EventListener> int getListenerCount(final Class<T> key)
	{
		if(listenerSetMap!=null)	//if we have a map of listener sets
		{
			final Set<T> listenerSet=(Set<T>)listenerSetMap.get(key);	//get the set of listeners associated with this key; we will have only stored subclasses of the class keyed to the given key
			if(listenerSet!=null)	//if there is a set of listeners associated with this key
			{
				synchronized(listenerSet)	//don't allow other threads to access the set while we access it
				{
					return listenerSet.size();	//return the size of the set of listeners
				}
			}
		}
		return 0;	//show that we have no listeners registered with the given key
	}

	/**Retrieves a read-only copied set of listeners associated with the given key. 
	<p>Example: <code>getListeners(MyListener.class);</code></p>
	@param key The key with which listeners have been associated.
	@return A set of all currently registered listeners.
	*/
	@SuppressWarnings("unchecked")
	public synchronized <T extends EventListener> Set<T> getListeners(final Class<T> key)
	{
		if(listenerSetMap!=null)	//if we have a map of listener sets
		{
			final Set<T> listenerSet=(Set<T>)listenerSetMap.get(key);	//get the set of listeners associated with this key; we will have only stored subclasses of the class keyed to the given key
			if(listenerSet!=null)	//if there is a set of listeners associated with this key
			{
				synchronized(listenerSet)	//don't allow other threads to access the set while we access it
				{
					if(listenerSet.size()>0)	//if there are elements in the listener set
					{
						return unmodifiableSet(new HashSet<T>(listenerSet));	//return a read-only copy of the listener set
					}
				}
			}
		}
		return EMPTY_SET;	//return an empty set of event listeners
	}

	/**Retrieves a lock to allow thread-safe access to a set of iterators
		associated with the given key.
	<p>Example: <code>synchronized(getLock(MyListener.class))</code>.</p>
	@param key The key with which the listeners are associated.
	@return A lock that allows thread synchronization before accessing listeners.
	*/
	public synchronized <T extends EventListener> Object getLock(final Class<T> key)
	{
		return getListenerSet(key);	//return the listener set, which should be used as the lock
	}

}
