package com.garretwilson.urf;

import java.net.URI;
import java.util.*;

import static java.util.Collections.*;
import java.util.concurrent.locks.*;

import com.garretwilson.lang.LongUtilities;
import com.garretwilson.lang.NumberUtilities;

import static com.garretwilson.lang.ObjectUtilities.*;

import com.garretwilson.util.*;
import static com.garretwilson.util.IteratorUtilities.*;
import static com.garretwilson.urf.URF.*;

/**Abstract implementation of a scope of URF properties.
@author Garret Wilson
*/
public abstract class AbstractURFScope extends ReadWriteLockDecorator implements URFScope
{

	/**The order in which this scope was created; useful as a unique ID when sorting.*/
	private final long creationOrder;

		/**@return The order in which this scope was created; useful as a unique ID when sorting.*/
		public long getCreationOrder() {return creationOrder;}

	/**The parent scope of this scope, or <code>null</code> if this scope has no parent scope.*/
	private final URFScope parentScope;

		/**@return The parent scope of this scope, or <code>null</code> if this scope has no parent scope.*/
		public URFScope getParentScope() {return parentScope;}

	/**Comparator for sorting property URIs.
	Property URIs that are numbers are always sorted first and in order. 
	*/
	private final static Comparator<URI> PROPERTY_URI_COMPARATOR=new Comparator<URI>()
			{
		
				/**Compares its two arguments for order.
				Returns a negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second.
				This implementation compares by number order if the property URIs are numbers.
				@param propertyURI1 The first object to be compared.
				@param propertyURI2 The second object to be compared.
				@return A negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second.
				*/
				public int compare(final URI propertyURI1, final URI propertyURI2)
				{
					final Number number1=asNumber(propertyURI1);	//see if the first property is a number
					final Number number2=asNumber(propertyURI2);	//see if the second property is a number
					if(number1!=null || number2!=null)	//if one of them is a number
					{
						return NumberUtilities.sort(number1, number2);	//sort the objects
					}
					else	//if neither is a number
					{
						return propertyURI1.compareTo(propertyURI2);	//compare URIs TODO add support for i18n ordering of fragments of identical namespaces
					}
				}
			};

	/**The map of property value contexts keyed to property URIs.
	The keys in the map are always kept sorted in order so that number properties are listed first and in order.
	@see #PROPERTY_URI_COMPARATOR 
	*/
	private CollectionMap<URI, URFValueContext, List<URFValueContext>> propertURIValueContextsMap=new DecoratorReadWriteLockCollectionMap<URI, URFValueContext, List<URFValueContext>>(new ArrayListMap<URI, URFValueContext>(new TreeMap<URI, List<URFValueContext>>(PROPERTY_URI_COMPARATOR)), this);

	/**Comparator to use for sorting value contexts to ensure scoped order.
	This comparator places scoped ordered value contexts in front of value contexts that have no scoped order.
	Two value contexts with scoped orders will be sorted according to those orders.
	Resorting must be performed manually, as scoped order can be changed independent of the list.
	*/
	private final static Comparator<URFValueContext> VALUE_CONTEXT_COMPARATOR=new Comparator<URFValueContext>()
			{
		
				/**Compares its two arguments for order.
				Returns a negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second.
				This implementation compares first by scoped order.
				@param valueContext1 The first object to be compared.
				@param valueContext2 The second object to be compared.
				@return A negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second.
				*/
				public int compare(final URFValueContext valueContext1, final URFValueContext valueContext2)
				{
					final Number order1=valueContext1.getScope().getOrder();	//get the scoped order of the first context
					final Number order2=valueContext2.getScope().getOrder();	//get the scoped order of the second context
					if(order1!=null || order2!=null)	//if one of the contexts has a scoped order
					{
						return NumberUtilities.sort(order1, order2);	//sort the scoped orders
					}
					else	//if neither context has a scoped order
					{
						return LongUtilities.compare(valueContext1.getValue().getCreationOrder(), valueContext2.getValue().getCreationOrder());	//compare value creation orders TODO improve to do more advanced sorting
					}
				}
			};


	/**The constant iterable for returning an iterator to this scope's properties.*/
	private final Iterable<URFProperty> propertyIterable=new Iterable<URFProperty>()
			{
				/**Returns an iterator over all this scope's properties.
				@return An iterator to all available properties.
				*/
			  public Iterator<URFProperty> iterator()
			  {
			  	return new PropertyIterator();	//create and return a new property iterator
			  }
			};

	/**Read write lock and parent scope constructor.
	@param readWriteLock The lock for controlling access to the properties.
	@param parentScope The parent scope of this scope, or <code>null</code> if this scope has no parent scope.
	@exception NullPointerException if the given lock is <code>null</code>.
	*/
	public AbstractURFScope(final ReadWriteLock readWriteLock, final URFScope parentScope)
	{
		super(readWriteLock);	//construct the parent class
		creationOrder=generateScopeCreationOrder();	//set the creation order of this scope
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
Debug.trace("checking for scope", propertyURI, propertyValue);
			final List<URFValueContext> valueContextList=propertURIValueContextsMap.get(propertyURI);	//get the list of value contexts
Debug.trace("got value context list for", propertyURI, valueContextList);
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
//TODO fix; this doesn't count overall properties	public boolean hasProperties() {return !propertURIValueContextsMap.isEmpty();}

	/**@return The number of properties this scope has.*/
//TODO fix; this doesn't count overall properties	public int getPropertyCount() {return propertURIValueContextsMap.size();}

	/**Returns an iterable to the properties of this scope.
	@return An iterable to all available properties.
	*/
	public Iterable<URFProperty> getProperties()
	{
		return propertyIterable;	//return the constant iterable to properties
	}

	/**Returns an iterable to the properties of this scope within a particular namespace.
	@param namespaceURI The URI of the namespace of the properties to be returned.
	@return An iterable to all available properties.
	@exception NullPointerException if the given namespace URI is <code>null</code>.
	*/
	public Iterable<URFProperty> getNamespaceProperties(final URI namespaceURI)
	{
		checkInstance(namespaceURI, "Namespace URI cannot be null.");
		return new Iterable<URFProperty>()	//return a new iterable over only the properties in the given namespace
		{
			/**Returns an iterator over all this scope's properties.
			@return An iterator to all available properties.
			*/
		  public Iterator<URFProperty> iterator()
		  {
		  	return new PropertyIterator(namespaceURI);	//create and return a new property iterator
		  }
		};		
	}

	/**Retrieves an iterable to all property URIs.
	Any deletions made to the returned iterable will result in all corresponding properties being removed.
	@return An iterable to all property URIs.
	*/
	public Iterable<URI> getPropertyURIs()
	{
		return propertURIValueContextsMap.keySet();	//return the set of keys, which are property URIs
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
/*TODO fix
	public URFResource getPropertyValue(final URI propertyURI)
	{
		readLock().lock();	//get a read lock
		try
		{
			final List<URFValueContext> valueContextList=propertURIValueContextsMap.get(propertyURI);	//get the list of value contexts, if any
			return valueContextList!=null && !valueContextList.isEmpty() ? valueContextList.get(0).getValue() : null;	//if there is a non-empty context list, return the first one
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
			final List<URFValueContext> valueContextList=propertURIValueContextsMap.get(propertyURI);	//get the list of value contexts, if any
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
			final List<URFValueContext> valueContextList=propertURIValueContextsMap.get(propertyURI);	//get the list of value contexts
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

	/**Determines whether there exists a property with the given property URI and the given property value.
	@param propertyURI The URI of the property of the value to check.
	@param propertyValue The value to match for the given property.
	@return <code>true</code> if a property exists with the given property URI and property value.
	*/
	public boolean hasPropertyValue(final URI propertyURI, final URFResource propertyValue)
	{
		readLock().lock();	//get a read lock
		try
		{
			final List<URFValueContext> valueContextList=propertURIValueContextsMap.getCollection(propertyURI);	//get the list of value contexts
			for(final URFValueContext valueContext:valueContextList)	//look at each value context
			{
				if(propertyValue.equals(valueContext.getValue()))	//if we have a context with this value
				{
					return true;	//indicate that we found a matching property value
				}
			}
			return false;	//indicate that the given property value could not be found
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
	}

	/**Determines whether there exists a property with the given property URI and the given property value URI.
	@param propertyURI The URI of the property of the value to check.
	@param propertyValueURI The value URI to match for the given property.
	@return <code>true</code> if a property exists with the given property URI and property value URI.
	@exception NullPointerException if the given property URI and/or property value URI is <code>null</code>.
	*/
	public boolean hasPropertyValueURI(final URI propertyURI, final URI propertyValueURI)
	{
		readLock().lock();	//get a read lock
		try
		{
			final List<URFValueContext> valueContextList=propertURIValueContextsMap.get(propertyURI);	//get the list of value contexts, if there is one
			if(valueContextList!=null)	//if there is a list of property values
			{
				for(final URFValueContext valueContext:valueContextList)	//look at each value context
				{
					if(propertyValueURI.equals(valueContext.getValue().getURI()))	//if we have a context with this value URI
					{
						return true;	//indicate that we found a matching property value URI
					}
				}
			}
			return false;	//indicate that the given property value URI could not be found
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
			final List<URFValueContext> valueContextList=propertURIValueContextsMap.getCollection(propertyURI);	//get the list of value contexts
			for(final URFValueContext valueContext:valueContextList)	//look at each value context
			{
				if(propertyValue.equals(valueContext.getValue()))	//if we already have a context with this value
				{
					return;	//don't add the property value; it's already there
				}
			}
			valueContextList.add(new URFValueContext(this, propertyValue, new ChildURFScope(propertyURI)));	//add a new value context
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
		final List<URFValueContext> valueContextList=propertURIValueContextsMap.createCollection();	//create a list of value contexts to hold the new value
		valueContextList.add(new URFValueContext(this, propertyValue, new ChildURFScope(propertyURI)));	//add a new value context
		propertURIValueContextsMap.put(propertyURI, valueContextList);	//change the value
	}

	/**Retrieves the order of this scope.
	The order is considered to be the number value of the property with URI {@value URF#ORDER_PROPERTY_URI}.
	@return The order of this scope, or <code>null</code> if no order is indicated or the order is not a number.
	*/
	public Number getOrder()
	{
		return asNumber(getPropertyValue(ORDER_PROPERTY_URI));
	}
	
	/**Implementation of a child URF scope subordinate to another URF scope.
	This version ensures that the value orders for the parent scope remain sorted if a scoped order is changed.
	@author Garret Wilson
	*/
	private class ChildURFScope extends AbstractURFScope
	{

		/**The URI of the property for this scope.*/
		private final URI propertyURI;

			/**@return The URI of the property for this scope.*/
			public URI getPropertyURI() {return propertyURI;}

		/**Property URI constructor.
		@param propertyURI The URI of the property for this scope.
		@exception NullPointerException if the given property URI is <code>null</code>.
		*/
		public ChildURFScope(final URI propertyURI)
		{
			super(AbstractURFScope.this, AbstractURFScope.this);	//construct the parent class, using the parent scope for locking
			this.propertyURI=checkInstance(propertyURI, "Property URI cannot be null.");
		}

		/**Adds a property value for the property with the given URI.
		If the given property and value already exists, no action occurs.
		This version resorts the value orders for the parent scope if this method changes a scoped order.
		@param propertyURI The URI of the property of the value to add.
		@param propertyValue The value to add for the given property.
		*/
		public void addPropertyValue(final URI propertyURI, final URFResource propertyValue)
		{
			writeLock().lock();	//get a write lock
			try
			{
				super.addPropertyValue(propertyURI, propertyValue);	//add the property value normally
				if(ORDER_PROPERTY_URI.equals(propertyURI))	//if the scoped order changed, resort the value orders for the parent scope
				{
					final List<URFValueContext> valueContextList=AbstractURFScope.this.propertURIValueContextsMap.get(getPropertyURI());	//get the list of value contexts from the parent scope
					if(valueContextList==null)	//if there is no value context for that property, this scope must be being used after the property was removed
					{
						throw new IllegalStateException("Scope for property URI "+getPropertyURI()+" being used after property value was changed.");
					}
					sort(valueContextList, VALUE_CONTEXT_COMPARATOR);	//re-sort the values based upon the new scoped order
				}
			}
			finally
			{
				writeLock().unlock();	//always release the write lock
			}
		}

		/**Sets a property value for the property with the given URI.
		This version resorts the value orders for the parent scope if this method changes a scoped order.
		@param propertyURI The URI of the property of the value to set.
		@param propertyValue The value to set for the given property.
		@exception IllegalStateException if this method is called after this scope is no longer available.
		*/
		public void setPropertyValue(final URI propertyURI, final URFResource propertyValue)
		{
			writeLock().lock();	//get a write lock
			try
			{
				super.setPropertyValue(propertyURI, propertyValue);	//set the property value normally
				if(ORDER_PROPERTY_URI.equals(propertyURI))	//if the scoped order changed, resort the value orders for the parent scope
				{
					final List<URFValueContext> valueContextList=AbstractURFScope.this.propertURIValueContextsMap.get(getPropertyURI());	//get the list of value contexts from the parent scope
					if(valueContextList==null)	//if there is no value context for that property, this scope must be being used after the property was removed
					{
						throw new IllegalStateException("Scope for property URI "+getPropertyURI()+" being used after property value was changed.");
					}
					sort(valueContextList, VALUE_CONTEXT_COMPARATOR);	//re-sort the values based upon the new scoped order
				}
			}
			finally
			{
				writeLock().unlock();	//always release the write lock
			}			
		}

	}

	/**An iterator that can iterate over all properties of this scope, or only those with a given namespace.
	This iterator does not support property removal.
	@author Garret Wilson
	*/
	private class PropertyIterator implements Iterator<URFProperty>
	{

		/**The URI of the namespace of the properties to be returned, or <code>null</code> if all properties should be returned.*/
		private final URI namespaceURI;

			/**@return The URI of the namespace of the properties to be returned, or <code>null</code> if all properties should be returned.*/
//TODO del			public URI getNamespaceURI() {return namespaceURI;}

		/**The iterator to property URIs.*/
		private final Iterator<URI> propertyURIIterator;

		/**The current property URI, or <code>null</code> if there is no property URI.*/
		private URI propertyURI=null;
		
		/**The iterator to value contexts for a single property URI.*/
		private Iterator<URFValueContext> valueContextIterator=null;
		
		/**Default constructor to iterate over all properties.*/
		public PropertyIterator()
		{
			this(null);	//construct the class, iterating over all namespaces
		}

		/**Namespace URI constructor.
		@param namespaceURI The URI of the namespace of the properties to be returned, or <code>null</code> if all properties should be returned.
		*/
		public PropertyIterator(final URI namespaceURI)
		{
			this.namespaceURI=namespaceURI;	//save the namespace URI
			this.propertyURIIterator=propertURIValueContextsMap.keySet().iterator();	//get an iterator to all the property URIs
			prime();	//prime the iterator
		}

		/**Primes the iterator by ensuring that, as long as there are property URIs available from the property URI iterator,
		the value context iterator will be non-<code>null</code> and have a next element.
		If there is a value context available, the property URI variable will be set when this method finishes.
		*/
		protected void prime()
		{
			while((valueContextIterator==null || !valueContextIterator.hasNext()) && propertyURIIterator.hasNext())	//while we don't have a value context iterator ready and there are more property URIs
			{
				propertyURI=propertyURIIterator.next();	//get the next property URI
				if(namespaceURI==null || namespaceURI.equals(getNamespaceURI(propertyURI)))	//only use values for this property URI if we're using all namespaces or this property is in the correct namespace
				{
					final List<URFValueContext> valueContextList=propertURIValueContextsMap.get(propertyURI);	//get the list of value contexts for this property
					valueContextIterator=valueContextList!=null ? valueContextList.iterator() : null;	//get an iterator to the value context list, if there is one
				}
			}
		}

		/**@return <code>true</code> if the iteration has more elements.*/
		public boolean hasNext()
		{
			return valueContextIterator!=null && valueContextIterator.hasNext();	//see if we have a value context iterator
		}

		/**Returns the next element in the iteration.
		@return The next element in the iteration.
		@exception NoSuchElementException if the iteration has no more elements.
		*/
		public URFProperty next()
		{
			if(hasNext())	//if we have a next item
			{
				final URFValueContext valueContext=valueContextIterator.next();	//get the next value context
				assert propertyURI!=null : "Priming should have fetched a property URI.";
				final URFProperty property=new URFProperty(AbstractURFScope.this, propertyURI, valueContext.getValue(), valueContext.getScope());	//create a property from our new values
				prime();	//prime the iterator for the next time
				return property;	//return the property
			}
			else	//if we have no next item
			{
				throw new NoSuchElementException("No more properties available.");
			}
		}

		/**Removes from the underlying collection the last element returned by the iterator.
		This implementation does not support removal, and will always throw an {@link UnsupportedOperationException}.
		@exception UnsupportedOperationException if the <tt>remove</tt> operation is not supported by this iterator.
		*/
		public void remove()
		{
			throw new UnsupportedOperationException("Property removal not supported by this iterator.");
		}
	}

	/**A list iterator that can iterate over values of a single property URI.
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
