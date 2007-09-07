package com.garretwilson.urf;

import java.net.URI;
import java.util.*;
import java.util.concurrent.locks.*;

import static com.garretwilson.lang.ObjectUtilities.*;
import com.garretwilson.util.*;
import static com.garretwilson.util.IteratorUtilities.*;

/**Manages URF properties.
@author Garret Wilson
*/
public class URFPropertyManager implements ReadWriteLock
{

	/**The decorated read write lock.*/
	private final ReadWriteLock readWriteLock;

	/**Returns the lock used for reading.
	@return the lock used for reading.
	*/
	public Lock readLock() {return readWriteLock.readLock();}

	/**Returns the lock used for writing.
	@return the lock used for writing.
	*/
	public Lock writeLock() {return readWriteLock.writeLock();}

	/**The map of property value contexts keyed to property URIs.*/
	private CollectionMap<URI, URFValueContext, List<URFValueContext>> propertyValuesMap=new DecoratorReadWriteLockCollectionMap<URI, URFValueContext, List<URFValueContext>>(new ArrayListHashMap<URI, URFValueContext>(), this);

	/**@return Whether this property has resources.*/
//	public boolean hasProperties() {return !properties.isEmpty();}

	/**@return The number of properties this resource has.*/
//	public int getPropertyCount() {return properties.size();}


	/**Read write lock constructor.
	@param readWriteLock The lock for controlling access to the properties.
	@exception NullPointerException if the given lock is <code>null</code>.
	*/
	public URFPropertyManager(final ReadWriteLock readWriteLock)
	{
		this.readWriteLock=checkInstance(readWriteLock, "Read write lock cannot be null.");
	}

	protected URFValueContext getPropertyValueContext(final URI propertyURI, final URFResource propertyValue)
	{
		readLock().lock();	//get a read lock
		try
		{
			final List<URFValueContext> valueContextList=propertyValuesMap.getCollection(propertyURI);	//get the list of value contexts
			for(final URFValueContext valueContext:valueContextList)	//look at each value context
			{
				if(propertyValue.equals(valueContext.getValue()))	//if we already have a context with this value
				{
					return valueContext;	//return this property value context
				}
			}
			return null;
/*TODO del
			final URFValueContext valueContext=new URFValueContext(propertyValue);	//create a new value context
			valueContextList.add(valueContext);	//add the new value context
			return valueContext;	//return the new value context
*/
		}
		finally
		{
			readLock().unlock();	//release the read lock
		}
	}

	/**Retrieves the first value of the property with the given URI.
	All contextually ordered properties will be returned in their correct order before any non-ordered properties.
	Unordered properties will be returned in an arbitrary order. 
	@param propertyURI The URI of the property for which a value should be returned.
	@return The first value of the property with the given URI, or <code>null</code> if there is no such property.
	*/
	public URFResource getPropertyValue(final URI propertyURI)
	{
		final List<URFValueContext> valueContextList=propertyValuesMap.getCollection(propertyURI);	//get the list of value contexts
		return valueContextList!=null && !valueContextList.isEmpty() ? valueContextList.get(0).getValue() : null;	//if there is a non-empty context list, return the first one
	}

	/**Retrieves an iterable to the values of the property with the given URI.
	All contextually ordered properties will be returned in their correct order before any non-ordered properties.
	Unordered properties will be returned in an arbitrary order. 
	@param propertyURI The URI of the property for which values should be returned.
	@return An iterable to all values of the property with the given URI.
	*/
	public Iterable<URFResource> getPropertyValues(final URI propertyURI)
	{
		readLock().lock();	//get a read lock
		try
		{
			final List<URFValueContext> valueContextList=propertyValuesMap.getCollection(propertyURI);	//get the list of value contexts
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
			readLock().unlock();	//release the read lock
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
			valueContextList.add(new URFValueContext(readWriteLock, propertyValue));	//add a new value context
		}
		finally
		{
			writeLock().unlock();	//release the write lock
		}
	}

	/**Sets a property value for the property with the given URI.
	@param propertyURI The URI of the property of the value to set.
	@param propertyValue The value to set for the given property.
	*/
	public void setPropertyValue(final URI propertyURI, final URFResource propertyValue)
	{
		final List<URFValueContext> valueContextList=propertyValuesMap.createCollection();	//create a list of value contexts to hold the new value
		valueContextList.add(new URFValueContext(readWriteLock, propertyValue));	//add a new value context
		propertyValuesMap.put(propertyURI, valueContextList);	//change the value
	}

	/**Adds a contextual property value for the contextual property with the given URI, in the context of a given property and value.
	If the given context property and value do not exists, no action occurs.
	If the given contextual property and value already exists, no action occurs.
	@param contextPropertyURI The URI of the context property of the value to add.
	@param contextPropertyValue The context value of the property value to add.
	@param contextualPropertyURI The URI of the property of the contextual value to add.
	@param contextualPropertyValue The value to add for the given contextual property.
	*/
	public void addContextPropertyValue(final URI contextPropertyURI, final URFResource contextPropertyValue, final URI contextualPropertyURI, final URFResource contextualPropertyValue)
	{
		writeLock().lock();	//get a write lock
		try
		{
			final URFValueContext valueContext=getPropertyValueContext(contextPropertyURI, contextPropertyValue);	//get the value context, if any
			if(valueContext!=null)	//if there is a value context
			{
				valueContext.addPropertyValue(contextualPropertyURI, contextualPropertyValue);	//get the contextual property value
			}
		}
		finally
		{
			writeLock().unlock();	//release the write lock
		}
	}

	/**Sets a contextual property value for the contextual property with the given URI, in the context of a given property and value.
	If the given context property and value do not exists, no action occurs.
	@param contextPropertyURI The URI of the context property of the value to set.
	@param contextPropertyValue The context value of the property value to set.
	@param contextualPropertyURI The URI of the property of the contextual value to set.
	@param contextualPropertyValue The value to set for the given contextual property.
	*/
	public void setContextPropertyValue(final URI contextPropertyURI, final URFResource contextPropertyValue, final URI contextualPropertyURI, final URFResource contextualPropertyValue)
	{
		writeLock().lock();	//get a write lock
		try
		{
			final URFValueContext valueContext=getPropertyValueContext(contextPropertyURI, contextPropertyValue);	//get the value context, if any
			if(valueContext!=null)	//if there is a value context
			{
				valueContext.setPropertyValue(contextualPropertyURI, contextualPropertyValue);	//set the contextual property value
			}
		}
		finally
		{
			writeLock().unlock();	//release the write lock
		}
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
