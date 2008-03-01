package com.garretwilson.event;

import java.util.*;

import static com.globalmentor.util.Iterators.*;

import java.util.concurrent.CopyOnWriteArraySet;

/**Class that stores and retrieves event listeners, facilitating the creation
	of another class that allows event listeners to be registered with it.
<p>This class maintains weak references to event listeners so that they may be
	collected by the garbage collector when they are no longer in ordinary use. TODO remove comment</p>
<p>A class is used as a key to a set of event listeners, all of which must be instances of that class or a subclass.
	Generics are used to ensure that only instances of the class or subclasses are keyed to a particular class.
	If the event listener type itself is generic, using classes should ensure that all listeners keyed to a particular
	class are indeed generic subclasses of the class, as Java 5 <code>Class&lt;<var>T</var>&gt;</code> objects do not keep track
	of the generic type of <var>T</var>.</p> 
<p>This class uses thread-safe access methods. Returned listener iterators are "snapshots" of currently registered listeners, so may be accessed even
	 though other threads (or even the event listener itself) may add and/or remove listeners.</p>
<p>Example:</p>
	<blockquote><pre><code>
	if(haslisteners(MyListener.class)
	{
		final MyEvent myEvent=new MyEvent();
		for(final myListener:getListeners(MyListener.class))
		{
			myListener.fireEvent(myEvent);		
		}
	}
	</pre></code></blockquote>
</p>
<p>This class uses little memory if there are no registered event listeners.</p>
<p>This class was inspired by <code>javax.swing.EventListenerList</code> 1.33 12/03/01 by Georges Saab, Hans Muller, and James Gosling.</p>
@author Garret Wilson
@see javax.swing.event.EventListenerList
*/
public class EventListenerManager	//TODO fix to not use WeakHashSet, which isn't a good idea, as anonymous listeners may not be referenced after being added to this class
{

	//Rather than synchronizing on the map, each method that accesses the map is
	//	synchronized because the map can be created and destroyed.

	/**The map containing weak sets of event listeners; only allocated when needed.*/
	private Map<Class<? extends EventListener>, Set<? extends EventListener>> listenerSetMap=null;

	/**Adds a listener to the manager, associated with the given key.
	If no listener set map or no listener set exists, it will be created.
	<p>Example: <code>add(MyListener.class, listener);</code>.</p>
	@param key The key with which the listener should be associated.
	@param listener The event listener to be associated with the key.
	*/
	@SuppressWarnings("unchecked")
	public <T extends EventListener, L extends T> void add(final Class<T> key, final L listener)	//use generics to make sure that the listener is a subclass of the given type
	{
		synchronized(this)	//don't allow the map to be modified while we add the listener; otherwise the map might be removed and we could add a listener to a set in an orphaned map
		{
			if(listenerSetMap==null)	//if there is no map of listener sets
			{
				listenerSetMap=new HashMap<Class<? extends EventListener>, Set<? extends EventListener>>();	//create a map of listener sets
			}
			Set<T> listenerSet=(Set<T>)listenerSetMap.get(key);	//get the set of listeners associated with the key; we will have only stored subclasses of the class keyed to the given key
			if(listenerSet==null)	//if there is no set of listeners associated with the key
			{
	//TODO fix weak hash set, which will not work for anonymous classes with no other references			listenerSet=Collections.synchronizedSet(new WeakHashSet());	//create a new synchronized weak set in which to store the listeners
				listenerSet=new CopyOnWriteArraySet<T>();	//create a new set in which to store the listeners; the set allows quick read access and is thread safe
				listenerSetMap.put(key, listenerSet);	//store the set in the map keyed to the key
			}
			listenerSet.add(listener);	//add the listener to the set of listeners
		}
	}

	/**Removes a listener from the manager as associated with the given key.
	If the listener is not associated with the key, no action is taken.
	If all listeners for a class are removed, its set is removed from from the map. If the map is consequently empty, the map is removed.
	<p>Example: <code>remove(MyListener.class, listener);</code>
	@param key The key with which the listener was associated.
	@param listener The listener to remove from the manager.
	@return <code>true</code> if the manager contained the specified listener.
	*/
	@SuppressWarnings("unchecked")
	public <T extends EventListener, L extends T> boolean remove(final Class<T> key, final L listener)
	{
		boolean containedListener=false;	//start out assuming we don't have the listener
		synchronized(this)	//don't allow the map to be modified while we remove the listener
		{
			if(listenerSetMap!=null)	//if we have a map of listener sets
			{
				final Set<T> listenerSet=(Set<T>)listenerSetMap.get(key);	//get the set of listeners associated with this key; we will have only stored subclasses of the class keyed to the given key
				if(listenerSet!=null)	//if there is a set of listeners associated with this key
				{
					containedListener=listenerSet.remove(listener);	//remove the given listener
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
		}
		return containedListener;	//return whether the manager contained the listener before it was removed
	}

	/**Returns the number of listeners associated with the given key.
	<p>Example: <code>getListenerCount(MyListener.class);</code>
	@param key The key with which the listeners are associated.
	@return The number of listeners associated with the given key.
	*/
	@SuppressWarnings("unchecked")
	public <T extends EventListener> int getListenerCount(final Class<T> key)
	{
		final Set<T> listenerSet;	//we'll get the set of listeners associated with this key; we will have only stored subclasses of the class keyed to the given key
		synchronized(this)	//only synchronize long enough to get the set
		{
			listenerSet=listenerSetMap!=null ? (Set<T>)listenerSetMap.get(key) : null;	//get the set of listeners associated with this key; we will have only stored subclasses of the class keyed to the given key
		}
		return listenerSet!=null ? listenerSet.size() : 0;	//if there is a set of listeners associated with this key, return the size (the set allows concurrent access); otherwise, show that we have no listeners registered with the given key
	}

	/**Determines whether there are listeners associated with the given key.
	<p>Example: <code>hasListeners(MyListener.class);</code>
	@param key The key with which the listeners are associated.
	@return <code>true</code> if there is at least one listener associated with the given key.
	@see #getListenerCount(Class)
	*/
	public <T extends EventListener> boolean hasListeners(final Class<T> key)
	{
		return getListenerCount(key)>0;	//see if there is more than one listener associated with the given key
	}

	/**Retrieves a thread-safe snapshot iterable of listeners associated with the given key. 
	<p>Example: <code>getListeners(MyListener.class);</code></p>
	@param key The key with which listeners have been associated.
	@return An iterable of all currently registered listeners.
	*/
	@SuppressWarnings("unchecked")
	public <T extends EventListener> Iterable<T> getListeners(final Class<T> key)
	{
		final Set<T> listenerSet;	//we'll get the set of listeners associated with this key; we will have only stored subclasses of the class keyed to the given key
		synchronized(this)	//only synchronize long enough to get the set
		{
			listenerSet=listenerSetMap!=null ? (Set<T>)listenerSetMap.get(key) : null;	//get the set of listeners associated with this key; we will have only stored subclasses of the class keyed to the given key
		}
		return listenerSet!=null ? listenerSet : (Iterable<T>)EMPTY_ITERABLE;	//if there is a set of listeners associated with this key, return the set (unsynchronized read access to the set is safe); otherwise, return an empty set 
	}

}
