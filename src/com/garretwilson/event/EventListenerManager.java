package com.garretwilson.event;

import java.util.*;
import com.garretwilson.util.*;

/**Class that stores and retrieves event listeners, facilitating the creation
	of another class that allows event listeners to be registered with it.
<p>This class maintains weak references to event listeners so that they may be
	collected by the garbage collector when they are no longer in ordinary use.</p>
<p>This class uses thread-safe access methods. Any calling class that iterates
	over installed listeners should synchronize on the object returned by
	<code>getLock()</code> in order to be thread-safe. As <code>getLock()</code>
	can create unneeded objects in the case of no listeners, that method should
	only be called if <code>getListenerCount()</code> returns a positive value.
	Example:
	<blockquote><pre><code>
	if(manager.getListenerCount(MyListener.class)>0)
	{
		synchronize(manager.getLock(MyListener.class))
		{
			final MyEvent myEvent=new MyEvent();
			final Iterator listenerIterator=manager.getListenerIterator(MyListener.class);
			while(listenerIterator.hasNext()
			{
				final MyListener myListener=(MyListener)listenerIterator.next()
				myListener.fireEvent(myEvent);
			}
		}
	}
	</pre></code></blockquote>
	This code does not remove the possibility that between checking the listener
	count and getting a lock, another thread will have created removed all
	listeners, thus leaving unneeded objects allocated. Howver, this will occur
	infrequently, no logic errors will occur, and subsequent calls to the manager
	will remove these unneeded objects. (i.e. in the above code, the unneeded
	objects would be removed when <code>getListenerIterator()</code> is called).
</p>
<p>This class uses little memory if there are no registered event listeners.</p>
<p>This class was inspired by <code>javax.swing.EventListenerList</code>
	1.33 12/03/01 by Georges Saab, Hans Muller, and James Gosling.</p>
@author Garret Wilson
@see javax.swing.EventListenerList
*/
public class EventListenerManager
{

	//Any access to a set is syncrhonized.

	//Rather than synchronizing on the map, each method that accesses the map is
	//	synchronized because the map can be created and destroyed.

	/**The shared iterator that contains no elements.*/
	protected final static Iterator EMPTY_ITERATOR=new IteratorUtilities.EmptyIterator();

	/**The map containing weak sets of event listeners; only allocated when needed.*/
	private Map listenerSetMap=null;

	/**Retrieves the set of listeners associated with the given key.
	If no listener set map or no listener set exists, it will be created.
	@param key The key with which the listeners would be associated.
	@return The set in which listeners associated with the given key are stored.
	*/
	protected synchronized Set getListenerSet(final Object key)
	{
		if(listenerSetMap==null)	//if there is no map of listener sets
		{
			listenerSetMap=new HashMap();	//create a map of listener sets
		}
		Set listenerSet=(Set)listenerSetMap.get(key);	//get the set of listeners associated with the key
		if(listenerSet==null)	//if there is no set of listeners associated with the key
		{
//G***fix			listenerSet=Collections.synchronizedSet(new WeakHashSet());	//create a new synchronized weak set in which to store the listeners
			listenerSet=Collections.synchronizedSet(new HashSet());	//create a new synchronized weak set in which to store the listeners
			listenerSetMap.put(key, listenerSet);	//store the set in the map keyed to the key
		}
		return listenerSet;	//return the set of listeners
	}

	/**Checks to see if a given listener set is empty and if so removes it from
		from the map. If the map is consequently empty, the map is removed.
	@param key The key with which the listener set is associated.
	@param listenerSet The set of listeners to check
	*/
	protected synchronized void checkListenerSet(final Object key, final Set listenerSet)
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
	public synchronized void add(final Object key, final EventListener listener)
	{
		final Set listenerSet=getListenerSet(key);	//get the set of listeners associated with the given key, creating a map and/or set if needed
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
	public synchronized boolean remove(final Object key, final EventListener listener)
	{
		boolean containedListener=false;	//start out assuming we don't have the listener
		if(listenerSetMap!=null)	//if we have a map of listener sets
		{
			final Set listenerSet=(Set)listenerSetMap.get(key);	//get the set of listeners associated with this key
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
	public synchronized int getListenerCount(final Object key)
	{
		if(listenerSetMap!=null)	//if we have a map of listener sets
		{
			final Set listenerSet=(Set)listenerSetMap.get(key);	//get the set of listeners associated with this key
			if(listenerSet!=null)	//if there is a set of listeners associated with this key
			{
				synchronized(listenerSet)	//don't allow other threads to access the set while we access it
				{
					checkListenerSet(key, listenerSet);	//remove the listener set and the map if they are no longer needed
					return listenerSet.size();	//return the size of the set of listeners
				}
			}
		}
		return 0;	//show that we have no listeners registered with the given key
	}

	/**Retrieves an iterator to the listeners associated with the given key. 
	For thread-safe access, the iterator should be within a block synchronized
		on the lock object obtained by <code>getLock()</code>.
	<p>Example: <code>getListenerIterator(MyListener.class);</code></p>
	@param key The key with which listeners have been associated.
	@return An iterator to all registered listeners.
	@see #getLock
	*/
	public synchronized Iterator getListenerIterator(final Object key)
	{
		if(listenerSetMap!=null)	//if we have a map of listener sets
		{
			final Set listenerSet=(Set)listenerSetMap.get(key);	//get the set of listeners associated with this key
			if(listenerSet!=null)	//if there is a set of listeners associated with this key
			{
				synchronized(listenerSet)	//don't allow other threads to access the set while we access it
				{
					if(listenerSet.size()>0)	//if there are elements in the listener set
					{
						return listenerSet.iterator();	//return an iterator to the weak set of listeners
					}
					else	//if there are no elements in the listener set
					{
						checkListenerSet(key, listenerSet);	//remove the listener set and the map if they are no longer needed, and just return an empty set
					}
				}
			}
		}
		return EMPTY_ITERATOR;	//return an empty iterator if we can't find a set of listeners 
	}

	/**Retrieves a lock to allow thread-safe access to a set of iterators
		associated with the given key.
	<p>Example: <code>synchronized(getLock(MyListener.class))</code>.</p>
	@param key The key with which the listeners are associated.
	@return A lock that allows thread synchronization before accessing listeners.
	*/
	public synchronized Object getLock(final Object key)
	{
		return getListenerSet(key);	//return the listener set, which should be used as the lock
	}

}
