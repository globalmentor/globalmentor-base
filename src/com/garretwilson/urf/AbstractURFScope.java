package com.garretwilson.urf;

import java.net.URI;
import java.util.*;
import java.util.concurrent.locks.*;

import com.garretwilson.util.*;
import static com.garretwilson.util.IteratorUtilities.*;

/**Abstract implementation of a scope of URF properties.
@author Garret Wilson
*/
public abstract class AbstractURFScope extends ReadWriteLockDecorator implements URFScope
{

	/**The parent scope of this scope, or <code>null</code> if this scope has no parent scope.*/
	private final URFScope parentScope;

		/**@return The parent scope of this scope, or <code>null</code> if this scope has no parent scope.*/
		public URFScope getParentScope() {return parentScope;}

	/**The map of property value contexts keyed to property URIs.*/
	private CollectionMap<URI, URFValueContext, List<URFValueContext>> propertyValuesMap=new DecoratorReadWriteLockCollectionMap<URI, URFValueContext, List<URFValueContext>>(new ArrayListHashMap<URI, URFValueContext>(), this);

	/**@return Whether this property has resources.*/
//	public boolean hasProperties() {return !properties.isEmpty();}

	/**@return The number of properties this resource has.*/
//	public int getPropertyCount() {return properties.size();}

	/**Read write lock and parent scope constructor.
	@param readWriteLock The lock for controlling access to the properties.
	@param parentScope The parent scope of this scope, or <code>null</code> if this scope has no parent scope.
	@exception NullPointerException if the given lock is <code>null</code>.
	*/
	public AbstractURFScope(final ReadWriteLock readWriteLock, final URFScope parentScope)
	{
		super(readWriteLock);	//construct the parent class
		this.parentScope=parentScope;	//save the parent scope
	}

	/**Retrieves the scope of a given property and value of this scope.
	@param propertyURI The URI of the property the scope of which to retrieve.
	@param propertyValue The value of the property the scope of which to retrieve.
	@return The scope of the given property and value, or <code>null</code> if no such property and value exists.
	*/
	public URFScope getScope(final URI propertyURI, final URFResource propertyValue)
	{
		readLock().lock();	//get a read lock
		try
		{
			final List<URFValueContext> valueContextList=propertyValuesMap.get(propertyURI);	//get the list of value contexts
			if(valueContextList!=null)	//if there is a value context for this property URI
			{
				for(final URFValueContext valueContext:valueContextList)	//look at each value context
				{
					if(propertyValue.equals(valueContext.getValue()))	//if we already have a context with this value
					{
						return valueContext.getScope();	//return this property value context
					}
				}
			}
			return null;	//if we didn't find the given property and value, there is no such context
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
	}

	/**@return Whether this scope has properties.*/
	public boolean hasProperties() {return !propertyValuesMap.isEmpty();}

	/**@return The number of properties this scope has.*/
	public int getPropertyCount() {return propertyValuesMap.size();}

	/**Retrieves an iterable to all property URIs.
	Any deletions made to the returned iterable will result in all corresponding properties being removed.
	@return An iterable to all property URIs.
	*/
	public Iterable<URI> getPropertyURIs()
	{
		return propertyValuesMap.keySet();	//return the set of keys, which are property URIs
	}
	
	/**Retrieves an iterable to all properties.
	All ordered properties will be returned in their correct order before any non-ordered properties.
	Unordered properties will be returned in an arbitrary order. 
	@param propertyURI The URI of the property for which values should be returned.
	@return An iterable to all values of the property with the given URI.
	*/
/*TODO fix
	public Iterable<URFResource> getPropertyValues(final URI propertyURI)
	{
		readLock().lock();	//get a read lock
		try
		{
			final List<URFValueContext> valueContextList=propertyValuesMap.get(propertyURI);	//get the list of value contexts
			if(valueContextList!=null && !valueContextList.isEmpty())	//if there is a non-empty context list
			{
				if(valueContextList.size()==1)	//if there is only one value
				{
					return new ObjectIterator<URFResource>(valueContextList.get(0).getValue());	//return the first value
				}
				else	//if there is more than one value
				{
					return new Iterable<URFResource>()	//return a new iterable that will return an iterator to the values
							{
								public Iterator<URFResource> iterator(){return new PropertyValueIterator(valueContextList);}
							};
				}
			}
			return emptyIterable();	//there is either no context list or no values in the list, so return an empty iterable
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
	}
*/

	/**Retrieves the first value of the property with the given URI.
	All ordered properties will be returned in their correct order before any non-ordered properties.
	Unordered properties will be returned in an arbitrary order. 
	@param propertyURI The URI of the property for which a value should be returned.
	@return The first value of the property with the given URI, or <code>null</code> if there is no such property.
	*/
	public URFResource getPropertyValue(final URI propertyURI)
	{
		readLock().lock();	//get a read lock
		try
		{
			final List<URFValueContext> valueContextList=propertyValuesMap.get(propertyURI);	//get the list of value contexts, if any
			return valueContextList!=null && !valueContextList.isEmpty() ? valueContextList.get(0).getValue() : null;	//if there is a non-empty context list, return the first one
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
	}

	/**Retrieves an iterable to the values of the property with the given URI.
	All ordered properties will be returned in their correct order before any non-ordered properties.
	Unordered properties will be returned in an arbitrary order. 
	@param propertyURI The URI of the property for which values should be returned.
	@return An iterable to all values of the property with the given URI.
	*/
	public Iterable<URFResource> getPropertyValues(final URI propertyURI)
	{
		readLock().lock();	//get a read lock
		try
		{
			final List<URFValueContext> valueContextList=propertyValuesMap.get(propertyURI);	//get the list of value contexts
			if(valueContextList!=null && !valueContextList.isEmpty())	//if there is a non-empty context list
			{
				if(valueContextList.size()==1)	//if there is only one value
				{
					return new ObjectIterator<URFResource>(valueContextList.get(0).getValue());	//return the first value
				}
				else	//if there is more than one value
				{
					return new Iterable<URFResource>()	//return a new iterable that will return an iterator to the values
							{
								public Iterator<URFResource> iterator(){return new PropertyValueIterator(valueContextList);}
							};
				}
			}
			return emptyIterable();	//there is either no context list or no values in the list, so return an empty iterable
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
	}

	/**Adds a property value for the property with the given URI.
	If the given property and value already exists, no action occurs.
	@param propertyURI The URI of the property of the value to add.
	@param propertyValue The value to add for the given property.
	*/
	public void addPropertyValue(final URI propertyURI, final URFResource propertyValue)
	{
		writeLock().lock();	//get a write lock
		try
		{
			final List<URFValueContext> valueContextList=propertyValuesMap.getCollection(propertyURI);	//get the list of value contexts
			for(final URFValueContext valueContext:valueContextList)	//look at each value context
			{
				if(propertyValue.equals(valueContext.getValue()))	//if we already have a context with this value
				{
					return;	//don't add the property value; it's already there
				}
			}
			valueContextList.add(new URFValueContext(this, propertyValue, new ChildURFScope()));	//add a new value context
		}
		finally
		{
			writeLock().unlock();	//always release the write lock
		}
	}

	/**Sets a property value for the property with the given URI.
	@param propertyURI The URI of the property of the value to set.
	@param propertyValue The value to set for the given property.
	*/
	public void setPropertyValue(final URI propertyURI, final URFResource propertyValue)
	{
		final List<URFValueContext> valueContextList=propertyValuesMap.createCollection();	//create a list of value contexts to hold the new value
		valueContextList.add(new URFValueContext(this, propertyValue, new ChildURFScope()));	//add a new value context
		propertyValuesMap.put(propertyURI, valueContextList);	//change the value
	}

	/**Implementation of a child URF scope subordinate to another URF scope.
	@author Garret Wilson
	*/
	private class ChildURFScope extends AbstractURFScope
	{

		/**Default constructor.*/
		public ChildURFScope()
		{
			super(AbstractURFScope.this, AbstractURFScope.this);	//construct the parent class, using the parent scope for locking
		}

		/**Sets a property value for the property with the given URI.
		@param propertyURI The URI of the property of the value to set.
		@param propertyValue The value to set for the given property.
		*/
/*TODO fix this sort of thing to initiate a resort if an index or an order was changed
		public void setPropertyValue(final URI propertyURI, final URFResource propertyValue)
		{
			
		}
*/

	}

	/**A list iterator that can iterate over property values.
	This iterator does not support setting or adding values.
	@author Garret Wilson
	*/
	private class PropertyValueIterator extends AbstractListIterator<URFResource, URFValueContext>
	{

		/**List constructor starting at the first index.
		@param list The list over which to iterate.
		@exception NullPointerException if the given list is <code>null</code>.
		*/
		public PropertyValueIterator(final List<URFValueContext> list)
		{
			this(list, 0);	//construct the class with a next index of the first available index
		}

		/**List and index constructor.
		@param list The list over which to iterate.
		@param index The index of first value to be returned from the list iterator (by a call to the {@link #next()} method).
		@exception NullPointerException if the given list is <code>null</code>.
		@exception IndexOutOfBoundsException if the index is out of range (<var>index</var> &lt; 0 || <var>index</var> &gt; <code>size()</code>).
		*/
		public PropertyValueIterator(final List<URFValueContext> list, final int index)
		{
			super(list, index);	//construct the parent class
		}

		/**Retrieves an item representing the element at the given position in the list.
		@param index The list index
		@return An item representing the element at the given index in the list
		@exception IndexOutOfBoundsException if the index is out of range (<var>index</var> &lt; 0 || <var>index</var> &gt;= <code>size()</code>).
		*/
		protected URFResource getItem(final int index)
		{
			return getList().get(index).getValue();	//return the value from the context
		}

		/**Sets the element at the given position in the list.
		@param index The list index
		@param item The item representing the element to be stored at the specified position.
		@return An item representing the element previously at the specified position.
		@exception UnsupportedOperationException if the {@link #set(Object)} operation is not supported by this list iterator.
		@exception IndexOutOfBoundsException if the index is out of range (<var>index</var> &lt; 0 || <var>index</var> &gt;= <code>size()</code>).
		*/
		protected URFResource setItem(final int index, final URFResource item)
		{
			throw new UnsupportedOperationException("Setting a value is not supported by this iterator.");
		}

		/**Inserts an element at the given position in the list.
		@param index The list index
		@param item The item representing the element to be inserted at the specified position.
		@exception UnsupportedOperationException if the {@link #add(Object)} operation is not supported by this list iterator.
		@exception IndexOutOfBoundsException if the index is out of range (<var>index</var> &lt; 0 || <var>index</var> &gt; <code>size()</code>).
		*/
		protected void addItem(final int index, final URFResource item)
		{
			throw new UnsupportedOperationException("Adding a value is not supported by this iterator.");
		}
	}

}
