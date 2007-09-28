package com.garretwilson.urf;

import java.net.URI;
import java.util.*;
import static java.util.Collections.*;
import java.util.concurrent.locks.*;

import com.garretwilson.lang.*;
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
			  	return new PropertyIterator();	//create and return a new property iterator over all properties
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
//Debug.trace("checking for scope", propertyURI, propertyValue);
			final List<URFValueContext> valueContextList=propertURIValueContextsMap.get(propertyURI);	//get the list of value contexts
//Debug.trace("got value context list for", propertyURI, valueContextList);
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

	/**The number of properties.*/
	private int propertyCount=0;
	
		/**@return Whether this scope has properties.*/
		public boolean hasProperties()
		{
			return propertyCount>0;	//return whether there are properties
		}
	
		/**@return The number of properties this scope has.*/
		public long getPropertyCount()
		{
			return propertyCount;	//return the property count
		}

	/**Determines whether there exists a property with the given property URI.
	@param propertyURI The URI of the property to check.
	@return <code>true</code> if a property exists with the given property URI.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public boolean hasProperty(final URI propertyURI)
	{
		readLock().lock();	//get a read lock
		try
		{
			final List<URFValueContext> valueContextList=propertURIValueContextsMap.get(propertyURI);	//get the list of value contexts, if there is one
			return valueContextList!=null && !valueContextList.isEmpty();	//this property exists if there is a non-empty list of value contexts
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
	}

	/**Determines whether there exists a property within the given namespace.
	@param namesapceURI The URI of the namespace of the property to check.
	@return <code>true</code> if a property exists in the given namespace.
	@exception NullPointerException if the given namespace URI is <code>null</code>.
	*/
	public boolean hasNamespaceProperty(final URI namespaceURI)
	{
		readLock().lock();	//get a read lock
		try
		{
			for(final Map.Entry<URI, List<URFValueContext>> propertyURIValueContextListEntry:propertURIValueContextsMap.entrySet())	//for each property map entry
			{
				if(namespaceURI.equals(getNamespaceURI(propertyURIValueContextListEntry.getKey())) && !propertyURIValueContextListEntry.getValue().isEmpty())	//if this property is in the requested namespace and there is at least one value
				{
					return true;	//indicate that we've found a property value in the given namespace
				}
			}
			return false;	//indicate that no properties in the given namespace could be found
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
	@exception NullPointerException if the given property URI and/or property value is <code>null</code>.
	*/
	public boolean hasPropertyValue(final URI propertyURI, final URFResource propertyValue)
	{
		readLock().lock();	//get a read lock
		try
		{
			final List<URFValueContext> valueContextList=propertURIValueContextsMap.get(propertyURI);	//get the list of value contexts, if there is one
			if(valueContextList!=null)	//if there is a list of property values
			{
				for(final URFValueContext valueContext:valueContextList)	//look at each value context
				{
					if(propertyValue.equals(valueContext.getValue()))	//if we have a context with this value
					{
						return true;	//indicate that we found a matching property value
					}
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

		/**Returns the number of properties in the given namespace.
		@param namespaceURI
		@return
		*/
/*TODO fix if needed
		public long getNamespacePropertyCount(final URI namespaceURI)
		{
			
		}
*/

	/**Returns an iterable to the properties of this scope.
	@return An iterable to all available properties.
	*/
	public Iterable<URFProperty> getProperties()
	{
		return propertyIterable;	//return the constant iterable to properties
	}

	/**Returns an iterable to the properties of this scope with the given property URI.
	@param propertyURI The URI of the properties to be returned.
	@return An iterable to all available properties with the given property URI.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public Iterable<URFProperty> getProperties(final URI propertyURI)
	{
		checkInstance(propertyURI, "Property URI cannot be null.");
		return new Iterable<URFProperty>()	//return a new iterable over only the properties with the given property URI
		{
			/**@return An iterator to all requested properties.*/
		  public Iterator<URFProperty> iterator()
		  {
		  	return new PropertyIterator(null, propertyURI);	//create and return a new property iterator over all properties with the given property URI
		  }
		};
	}

	/**Returns an iterable to the properties of this scope within a particular namespace.
	@param namespaceURI The URI of the namespace of the properties to be returned.
	@return An iterable to all available properties within the given namespace.
	@exception NullPointerException if the given namespace URI is <code>null</code>.
	*/
	public Iterable<URFProperty> getNamespaceProperties(final URI namespaceURI)
	{
		checkInstance(namespaceURI, "Namespace URI cannot be null.");
		return new Iterable<URFProperty>()	//return a new iterable over only the properties in the given namespace
		{
			/**@return An iterator to all requested properties.*/
		  public Iterator<URFProperty> iterator()
		  {
		  	return new PropertyIterator(namespaceURI);	//create and return a new property iterator over all properties in the given namespace
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

	/**Retrieves the first value context of the property with the given URI.
	All ordered properties will be returned in their correct order before any non-ordered properties.
	Unordered properties will be returned in an arbitrary order.
	@param propertyURI The URI of the property for which a value context should be returned.
	@return The first value context of the property with the given URI, or <code>null</code> if there is no such property.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public URFValueContext getPropertyValueContext(final URI propertyURI)
	{
		readLock().lock();	//get a read lock
		try
		{
			final List<URFValueContext> valueContextList=propertURIValueContextsMap.get(propertyURI);	//get the list of value contexts, if any
			return valueContextList!=null && !valueContextList.isEmpty() ? valueContextList.get(0) : null;	//if there is a non-empty context list, return the first one
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
	}

	/**Retrieves an iterable to the value contexts of the property with the given URI.
	All ordered properties will be returned in their correct order before any non-ordered properties.
	Unordered properties will be returned in an arbitrary order.
	@param propertyURI The URI of the property for which value contexts should be returned.
	@return An iterable to all value contexts of the property with the given URI.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public Iterable<URFValueContext> getPropertyValueContexts(final URI propertyURI)
	{
		readLock().lock();	//get a read lock
		try
		{
			final List<URFValueContext> valueContextList=propertURIValueContextsMap.get(propertyURI);	//get the list of value contexts
			return (Iterable<URFValueContext>)(valueContextList!=null ? unmodifiableList(valueContextList) : emptyIterable());	//return an unmodifiable version of the list, or an empty iterable if there is no list 
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
	}

	/**Retrieves the first value of the property with the given URI.
	All ordered properties will be returned in their correct order before any non-ordered properties.
	Unordered properties will be returned in an arbitrary order.
	@param propertyURI The URI of the property for which a value should be returned.
	@return The first value of the property with the given URI, or <code>null</code> if there is no such property.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public URFResource getPropertyValue(final URI propertyURI)
	{
		readLock().lock();	//get a read lock
		try
		{
			final URFValueContext valueContext=getPropertyValueContext(propertyURI);	//get the property value context, if any
			return valueContext!=null ? valueContext.getValue() : null;	//if there is a property value context, return its value
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
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public Iterable<URFResource> getPropertyValues(final URI propertyURI)
	{
		return getPropertyValues(propertyURI, URFResource.class);	//iterate over all the values (which are already guaranteed to be URF resources)
	}

	/**Retrieves an iterable to the values of a given type of the property with the given URI.
	All values of the property with the given URI that are not of the specified type will be ignored.
	All ordered properties will be returned in their correct order before any non-ordered properties.
	Unordered properties will be returned in an arbitrary order.
	@param <V> The type of values to be returned.
	@param propertyURI The URI of the property for which values should be returned.
	@param valueClass The class indicating the type of values to be returned in the iterator.
	@return An iterable to all values of the given type of the property with the given URI.
	@exception NullPointerException if the given property URI and/or value class is <code>null</code>.
	*/
	public <V extends URFResource> Iterable<V> getPropertyValues(final URI propertyURI, final Class<V> valueClass)
	{
		checkInstance(valueClass, "Value class cannot be null.");	//make sure the value class isn't null up front; it may be a while before we use it
		readLock().lock();	//get a read lock
		try
		{
			final List<URFValueContext> valueContextList=propertURIValueContextsMap.get(propertyURI);	//get the list of value contexts
			if(valueContextList!=null && !valueContextList.isEmpty())	//if there is a non-empty context list
			{
				if(valueContextList.size()==1)	//if there is only one value
				{
					final URFResource value=valueContextList.get(0).getValue();	//get the sole value
					if(valueClass.isInstance(value))	//if the value is of the correct type
					{
						return new ObjectIterator<V>(valueClass.cast(valueContextList.get(0).getValue()));	//return an iterator to the first value, cast to the correct type						
					}
				}
				else	//if there is more than one value
				{
					return new Iterable<V>()	//return a new iterable that will return an iterator to the values of the correct type
							{
								public Iterator<V> iterator(){return new PropertyValueIterator<V>(valueContextList, valueClass);}	//iterate over the values of the requested type
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
	@return The context of the added value.
	@exception NullPointerException if the given property URI and/or property value is <code>null</code>.
	*/
	public URFValueContext addPropertyValue(final URI propertyURI, final URFResource propertyValue)
	{
		writeLock().lock();	//get a write lock
		try
		{
			final List<URFValueContext> valueContextList=propertURIValueContextsMap.getCollection(propertyURI);	//get the list of value contexts
			for(final URFValueContext valueContext:valueContextList)	//look at each value context
			{
				if(propertyValue.equals(valueContext.getValue()))	//if we already have a context with this value
				{
					return valueContext;	//don't add the property value; it's already there
				}
			}
			final URFValueContext valueContext=new DefaultURFValueContext(this, propertyValue, new ChildURFScope(propertyURI));	//create a new value context
			valueContextList.add(valueContext);	//add a new value context
			++propertyCount;	//note that another property has been added
			return valueContext;	//return the new value context
		}
		finally
		{
			writeLock().unlock();	//always release the write lock
		}
	}

	/**Sets a property value for the property with the given URI by removing all properties with the given URI and adding the given property value.
	@param propertyURI The URI of the property of the value to set.
	@param propertyValue The value to set for the given property, or <code>null</code> if there should be no such property.
	@return The old property value, or <code>null</code> if there was no property value previously.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public URFResource setPropertyValue(final URI propertyURI, final URFResource propertyValue)
	{
		final List<URFValueContext> oldValueContextList;	//we'll keep track of the old list of values, if any, so as to keep track of the number of properties
		if(propertyValue!=null)	//if a property value was given
		{
			final List<URFValueContext> valueContextList=propertURIValueContextsMap.createCollection();	//create a list of value contexts to hold the new value
			valueContextList.add(new DefaultURFValueContext(this, propertyValue, new ChildURFScope(propertyURI)));	//add a new value context
			oldValueContextList=propertURIValueContextsMap.put(propertyURI, valueContextList);	//change the list of context values, noting the old list of values, if any
			++propertyCount;	//note that another property has been added
		}
		else	//if no property value was given
		{
			oldValueContextList=propertURIValueContextsMap.remove(propertyURI);	//remove the old list of values, if any
		}
		if(oldValueContextList!=null)	//if there were values before this operation
		{
			propertyCount-=oldValueContextList.size();	//note the properties that were removed
			if(!oldValueContextList.isEmpty())	//if the old list isn't empty
			{
				return oldValueContextList.get(0).getValue();	//return its first value
			}
		}
		return null;	//indicate that there was no previous property value
	}

	/**Sets a string property value for the property with the given URI by removing all properties with the given URI and adding the given property value.
	@param propertyURI The URI of the property of the value to set.
	@param propertyValue The value to set for the given property, or <code>null</code> if there should be no such property.
	@return The old property value, or <code>null</code> if there was no property value previously.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public URFResource setPropertyValue(final URI propertyURI, final String propertyValue)
	{
		return setPropertyValue(propertyURI, DEFAULT_URF_RESOURCE_FACTORY.createResource(propertyValue));	//set the property value with a string
	}

	/**Sets a URI property value for the property with the given URI by removing all properties with the given URI and adding the given property value.
	@param propertyURI The URI of the property of the value to set.
	@param propertyValue The value to set for the given property, or <code>null</code> if there should be no such property.
	@return The old property value, or <code>null</code> if there was no property value previously.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public URFResource setPropertyValue(final URI propertyURI, final URI propertyValue)
	{
		return setPropertyValue(propertyURI, DEFAULT_URF_RESOURCE_FACTORY.createResource(propertyValue));	//set the property value with a string
	}

	/**Removes all properties of this scope.
	@return The number of properties removed.
	*/
	public long removeProperties()
	{
		writeLock().lock();	//get a write lock
		try
		{
			final long removedPropertyCount=propertyCount;	//get the number of properties before removal
			propertURIValueContextsMap.clear();	//remove all property value contexts for all property URIs
			propertyCount=0;	//indicate that there are no properties
			return removedPropertyCount;	//return the number of properties we removed
		}
		finally
		{
			writeLock().unlock();	//always release the write lock
		}					
	}

	/**Removes all properties of this scope within a particular namespace.
	@param namespaceURI The URI of the namespace of the properties to be removed.
	@return The number of properties removed.
	@exception NullPointerException if the given namespace URI is <code>null</code>.
	*/
	public long removeNamespaceProperties(final URI namespaceURI)
	{
		long removedPropertyCount=0;	//we haven't removed any properties, yet
		writeLock().lock();	//get a write lock
		try
		{
			final Iterator<Map.Entry<URI, List<URFValueContext>>> propertyURIValueContextListEntryIterator=propertURIValueContextsMap.entrySet().iterator();	//get an iterator to the property map entries
			while(propertyURIValueContextListEntryIterator.hasNext())	//while there are more property map entries
			{
				final Map.Entry<URI, List<URFValueContext>> propertyURIValueContextListEntry=propertyURIValueContextListEntryIterator.next();	//get the next property map entry
				if(namespaceURI.equals(getNamespaceURI(propertyURIValueContextListEntry.getKey())))	//if this property is in the requested namespace
				{
					final int valueCount=propertyURIValueContextListEntry.getValue().size();	//see how many values we're going to remove
					propertyURIValueContextListEntryIterator.remove();	//remove all the values for this property
					propertyCount-=valueCount;	//update our total number of properties
					removedPropertyCount+=valueCount;	//show that we removed more properties
				}
			}
			return removedPropertyCount;	//return the number of properties we removed
		}
		finally
		{
			writeLock().unlock();	//always release the write lock
		}			
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
		@return The context of the added value.
		@exception NullPointerException if the given property URI and/or property value is <code>null</code>.
		@exception IllegalStateException if this method is called after this scope is no longer available.
		*/
		public URFValueContext addPropertyValue(final URI propertyURI, final URFResource propertyValue)
		{
			writeLock().lock();	//get a write lock
			try
			{
				final URFValueContext valueContext=super.addPropertyValue(propertyURI, propertyValue);	//add the property value normally and save the value context
				if(ORDER_PROPERTY_URI.equals(propertyURI))	//if the scoped order changed, resort the value orders for the parent scope
				{
					final List<URFValueContext> valueContextList=AbstractURFScope.this.propertURIValueContextsMap.get(getPropertyURI());	//get the list of value contexts from the parent scope
					if(valueContextList==null)	//if there is no value context for that property, this scope must be being used after the property was removed
					{
						throw new IllegalStateException("Scope for property URI "+getPropertyURI()+" being used after property value was changed.");
					}
					sort(valueContextList, VALUE_CONTEXT_COMPARATOR);	//re-sort the values based upon the new scoped order
				}
				return valueContext;	//return the added value context
			}
			finally
			{
				writeLock().unlock();	//always release the write lock
			}
		}

		/**Sets a property value for the property with the given URI by removing all properties with the given URI and adding the given property value.
		This version resorts the value orders for the parent scope if this method changes a scoped order.
		@param propertyURI The URI of the property of the value to set.
		@param propertyValue The value to set for the given property, or <code>null</code> if there should be no such property.
		@return The old property value, or <code>null</code> if there was no property value previously.
		@exception NullPointerException if the given property URI is <code>null</code>.
		@exception IllegalStateException if this method is called after this scope is no longer available.
		*/
		public URFResource setPropertyValue(final URI propertyURI, final URFResource propertyValue)
		{
			writeLock().lock();	//get a write lock
			try
			{
				final URFResource oldValue=super.setPropertyValue(propertyURI, propertyValue);	//set the property value normally, saving the old value
				if(ORDER_PROPERTY_URI.equals(propertyURI))	//if the scoped order changed, resort the value orders for the parent scope
				{
					final List<URFValueContext> valueContextList=AbstractURFScope.this.propertURIValueContextsMap.get(getPropertyURI());	//get the list of value contexts from the parent scope
					if(valueContextList==null)	//if there is no value context for that property, this scope must be being used after the property was removed
					{
						throw new IllegalStateException("Scope for property URI "+getPropertyURI()+" being used after property value was changed.");
					}
					sort(valueContextList, VALUE_CONTEXT_COMPARATOR);	//re-sort the values based upon the new scoped order
				}
				return oldValue;	//return the old value
			}
			finally
			{
				writeLock().unlock();	//always release the write lock
			}			
		}

		/**Removes all properties of this scope within a particular namespace
		This version resorts the value orders for the parent scope if this method changes a scoped order.
		@param namespaceURI The URI of the namespace of the properties to be removed.
		@return The number of properties removed.
		@exception NullPointerException if the given namespace URI is <code>null</code>.
		@exception IllegalStateException if this method is called after this scope is no longer available.
		*/
		public long removeNamespaceProperties(final URI namespaceURI)
		{
			writeLock().lock();	//get a write lock
			try
			{
				final long removedPropertyCount=super.removeNamespaceProperties(namespaceURI);	//remove the namespaced properties normally, saving the number of properties removed
				if(URF_NAMESPACE_URI.equals(namespaceURI))	//if all the URF properties were removed, the scoped order might have changed, resort the value orders for the parent scope
				{
					final List<URFValueContext> valueContextList=AbstractURFScope.this.propertURIValueContextsMap.get(getPropertyURI());	//get the list of value contexts from the parent scope TODO combine common code
					if(valueContextList==null)	//if there is no value context for that property, this scope must be being used after the property was removed
					{
						throw new IllegalStateException("Scope for property URI "+getPropertyURI()+" being used after property value was changed.");
					}
					sort(valueContextList, VALUE_CONTEXT_COMPARATOR);	//re-sort the values based upon the new scoped order
				}
				return removedPropertyCount;	//return the number of properties we removed
			}
			finally
			{
				writeLock().unlock();	//always release the write lock
			}			
		}
	}

	/**An iterator that can iterate over all properties of this scope, only those with a given namespace, or only those with a given property URI.
	This iterator does not support property removal.
	@author Garret Wilson
	*/
	private class PropertyIterator implements Iterator<URFProperty>
	{

		/**The URI of the namespace of the properties to be returned, or <code>null</code> if all properties should be returned.*/
		private final URI namespaceURI;

		/**The iterator to property URIs.*/
		private final Iterator<URI> propertyURIIterator;

		/**The current property URI, or <code>null</code> if there is no property URI.*/
		private URI propertyURI=null;
		
		/**The iterator to value contexts for a single property URI.*/
		private Iterator<URFValueContext> valueContextIterator=null;
		
		/**Default constructor to iterate over all properties.*/
		public PropertyIterator()
		{
			this(null);	//construct the class, iterating over all namespaces and properties
		}

		/**Namespace constructor.
		@param namespaceURI The URI of the namespace of the properties to be returned, or <code>null</code> if all properties should be returned.
		*/
		public PropertyIterator(final URI namespaceURI)
		{
			this(namespaceURI, (Set<URI>)null);	//construct the class, iterating over all property URIs in the given namespace, if any
		}

		/**Namespace and single property URI constructor.
		@param namespaceURI The URI of the namespace of the properties to be returned, or <code>null</code> if all properties should be returned.
		@param propertyURI The URI of a specific property to be returned if it is in the given namespace, if any.
		@exception NullPointerException if the given property URI is <code>null</code>.
		*/
		public PropertyIterator(final URI namespaceURI, final URI propertyURI)
		{
			this(namespaceURI, new ObjectIterator<URI>(checkInstance(propertyURI, "Property URI cannot be null.")));
		}

		/**Namespace and property URIs constructor.
		@param namespaceURI The URI of the namespace of the properties to be returned, or <code>null</code> if all properties should be returned.
		@param propertyURIs The URIs of the specific properties to be returned, or <code>null</code> if all properties should be returned.
		*/
		public PropertyIterator(final URI namespaceURI, final Set<URI> propertyURIs)
		{
			this(namespaceURI, propertyURIs!=null ? propertyURIs.iterator() : propertURIValueContextsMap.keySet().iterator());	//construct the class using an iterator to all available property URIs if none were given
		}

		/**Namespace and property URI iterator constructor.
		@param namespaceURI The URI of the namespace of the properties to be returned, or <code>null</code> if all properties should be returned.
		@param propertyURIIterator The iterator to the URIs of the specific properties to be returned.
		@exception NullPointerException if the given property URI iterator is <code>null</code>.
		*/
		protected PropertyIterator(final URI namespaceURI, final Iterator<URI> propertyURIIterator)
		{
			this.namespaceURI=namespaceURI;	//save the namespace URI
			this.propertyURIIterator=propertyURIIterator;	//save the property URI iterator
			prime();	//prime this iterator
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
				final URFProperty property=new DefaultURFProperty(AbstractURFScope.this, AbstractURFScope.this, propertyURI, valueContext.getValue(), valueContext.getScope());	//create a property from our new values
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
	@param <V> The type of values to be returned.
	@author Garret Wilson
	*/
	private class PropertyValueIterator<V extends URFResource> extends AbstractListIterator<V, URFValueContext>
	{

		/**The class indicating the type of values to be returned in the iterator.*/
		private final Class<V> valueClass;

			/**@return The class indicating the type of values to be returned in the iterator.*/
			public Class<V> getValueClass() {return valueClass;}

		/**List and value class constructor starting at the first index.
		@param list The list over which to iterate.
		@param valueClass The class indicating the type of values to be returned in the iterator.
		@exception NullPointerException if the given list and/or value class is <code>null</code>.
		*/
		public PropertyValueIterator(final List<URFValueContext> list, final Class<V> valueClass)
		{
			super(list);	//construct the parent class
			this.valueClass=checkInstance(valueClass, "Value class cannot be null.");
		}

		/**Determines whether the item at the given index should be included.
		This version only includes values that are instances of the value class.
		@param index The index of the item to check.
		@return <code>true</code> if the item at the given index should be included in the iteration, else <code>false</code> if it should be ignored.
		@see #getValueClass()
		*/
		protected boolean isIncluded(final int index)
		{
			return getValueClass().isInstance(getList().get(index).getValue());	//determine if the item at this index is an instance of the value class
		}

		/**Removes from the list the last element that was returned by {@link #next()} or {@link #previous()}.
		This version updates the property count of this scope.
		@exception IllegalStateException neither {@link #next()} nor {@link #previous()} have been called,
			or {@link #remove()} or {@link #add(Object)} have been called after the last call to {@link #next()} or {@link #previous()}.
		*/
		public void remove()
		{
			super.remove();	//do the default removal
			--propertyCount;	//note that a property has been removed
		}

		/**Retrieves an item representing the element at the given position in the list.
		@param index The list index
		@return An item representing the element at the given index in the list
		@exception IndexOutOfBoundsException if the index is out of range (<var>index</var> &lt; 0 || <var>index</var> &gt;= <code>size()</code>).
		*/
		protected V getItem(final int index)
		{
			return getValueClass().cast(getList().get(index).getValue());	//return the value from the context
		}

		/**Sets the element at the given position in the list.
		@param index The list index
		@param item The item representing the element to be stored at the specified position.
		@return An item representing the element previously at the specified position.
		@exception UnsupportedOperationException if the {@link #set(Object)} operation is not supported by this list iterator.
		@exception IndexOutOfBoundsException if the index is out of range (<var>index</var> &lt; 0 || <var>index</var> &gt;= <code>size()</code>).
		*/
		protected V setItem(final int index, final URFResource item)
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
