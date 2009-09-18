package com.globalmentor.urf;

import java.net.URI;
import java.util.*;
import static java.util.Collections.*;
import java.util.concurrent.locks.*;

import com.globalmentor.collections.ArrayListMap;
import com.globalmentor.collections.CollectionMap;
import com.globalmentor.collections.DecoratorReadWriteLockCollectionMap;
import com.globalmentor.collections.ReadWriteLockDecorator;
import com.globalmentor.collections.iterators.AbstractListIterator;
import com.globalmentor.collections.iterators.Iterators;
import com.globalmentor.collections.iterators.ObjectIterator;
import com.globalmentor.java.*;
import com.globalmentor.util.*;

import static com.globalmentor.collections.iterators.Iterators.*;
import static com.globalmentor.java.Objects.*;
import static com.globalmentor.urf.URF.*;

/**Abstract implementation of a scope of URF properties.
This implementation maintains a map of URF value context lists representing property values, each keyed to the URI of the corresponding URI.
The value context lists are not allowed to be empty; once all values of a particular property is removed, the entire list is removed from the map. 
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
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
						return Numbers.sort(number1, number2);	//sort the objects
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
	private CollectionMap<URI, URFValueContext, List<URFValueContext>> propertyURIValueContextsMap=new DecoratorReadWriteLockCollectionMap<URI, URFValueContext, List<URFValueContext>>(new ArrayListMap<URI, URFValueContext>(new TreeMap<URI, List<URFValueContext>>(PROPERTY_URI_COMPARATOR)), this);

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
						return Numbers.sort(order1, order2);	//sort the scoped orders
					}
					else	//if neither context has a scoped order
					{
						return Longs.compare(valueContext1.getValue().getCreationOrder(), valueContext2.getValue().getCreationOrder());	//compare value creation orders TODO improve to do more advanced sorting
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
			final List<URFValueContext> valueContextList=propertyURIValueContextsMap.get(propertyURI);	//get the list of value contexts
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

	/**The number of property values.*/
	private long propertyValueCount=0;

		/**@return Whether this scope has properties.*/
		public boolean hasProperties()
		{
			return propertyValueCount>0;	//return whether there are properties
		}

		/**Determines the number of distinct properties that have at least one value.
		@return The number of propertis this scope has.
		*/
		public long getPropertyCount()
		{
			return propertyURIValueContextsMap.size();	//return the number of mappings in the property-value context list map
		}

		/**@return The number of property values this scope has.*/
		public long getPropertyValueCount()
		{
			return propertyValueCount;	//return the property value count
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
			final List<URFValueContext> valueContextList=propertyURIValueContextsMap.get(propertyURI);	//get the list of value contexts, if there is one
			return valueContextList!=null && !valueContextList.isEmpty();	//this property exists if there is a non-empty list of value contexts TODO remove all the isEmpty() checks, or change them to assertions 
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
	}

	/**Determines whether there exists a property within the given namespace.
	@param namespaceURI The URI of the namespace of the property to check.
	@return <code>true</code> if a property exists in the given namespace.
	@exception NullPointerException if the given namespace URI is <code>null</code>.
	*/
	public boolean hasNamespaceProperty(final URI namespaceURI)
	{
		readLock().lock();	//get a read lock
		try
		{
			for(final Map.Entry<URI, List<URFValueContext>> propertyURIValueContextListEntry:propertyURIValueContextsMap.entrySet())	//for each property map entry
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

	/**Determines the number of values there exists for a property with the given property URI.
	@param propertyURI The URI of the property to count.
	@return The number of values of the property with the given property URI.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public long getPropertyValueCount(final URI propertyURI)
	{
		readLock().lock();	//get a read lock
		try
		{
			final List<URFValueContext> valueContextList=propertyURIValueContextsMap.get(propertyURI);	//get the list of value contexts, if there is one
			return valueContextList!=null ? valueContextList.size() : 0;	//if this property exists, return the number of values
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
	}

	/**Determines whether there exists a property with the given property and value.
	This is a convenient method that is equivalent to {@link #hasPropertyValue(URI, URFResource)}.
	@param property The property and value to check.
	@return <code>true</code> if a property exists with the given property and value.
	@exception NullPointerException if the given property is <code>null</code>.
	*/
	public boolean hasProperty(final URFProperty property)
	{
		return hasPropertyValue(property.getPropertyURI(), property.getValue());	//delegate to the property URI and value method
	}

	/**Determines whether there exists a property with the given property URI and the given property value.
	This implementation delegates to {@link #getPropertyValueContext(URI, URFResource)}.
	@param propertyURI The URI of the property of the value to check.
	@param propertyValue The value to match for the given property.
	@return <code>true</code> if a property exists with the given property URI and property value.
	@exception NullPointerException if the given property URI and/or property value is <code>null</code>.
	*/
	public boolean hasPropertyValue(final URI propertyURI, final URFResource propertyValue)
	{
		return getPropertyValueContext(propertyURI, propertyValue)!=null;
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
			final List<URFValueContext> valueContextList=propertyURIValueContextsMap.get(propertyURI);	//get the list of value contexts, if there is one
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
	public Iterable<URI> getPropertyURIs()	//TODO fix; by allowing this iterator to be live, we could end up with an inconsistent property count
	{
		return propertyURIValueContextsMap.keySet();	//return the set of keys, which are property URIs
	}

	/**Retrieves the property with the given URI and value.
	@param propertyURI The URI of the property.
	@param propertyValue The value of the property.
	@return The scope property with the given URI and value, or <code>null</code> if no such property and value exists.
	*/
/*TODO del
	public URFProperty getProperty(final URI propertyURI, final URFResource propertyValue)
	{
		readLock().lock();	//get a read lock
		try
		{
			final List<URFValueContext> valueContextList=propertyURIValueContextsMap.get(propertyURI);	//get the list of value contexts
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
*/

	/**Retrieves the value context of the property with the given preoprty URI and the given property value.
	@param propertyURI The URI of the property for which a value context should be returned.
	@param propertyValue The value of the property for which a value context should be returned.
	@return The value context of the property with the given URI and value, or <code>null</code> if there is no such property with the given value.
	@exception NullPointerException if the given property URI and/or property value is <code>null</code>.
	*/
	public URFValueContext getPropertyValueContext(final URI propertyURI, final URFResource propertyValue)
	{
		readLock().lock();	//get a read lock
		try
		{
			final List<URFValueContext> valueContextList=propertyURIValueContextsMap.get(propertyURI);	//get the list of value contexts, if there is one
			if(valueContextList!=null)	//if there is a list of property values
			{
				for(final URFValueContext valueContext:valueContextList)	//look at each value context
				{
					if(propertyValue.equals(valueContext.getValue()))	//if we have a context with this value
					{
						return valueContext;	//return the matching property value
					}
				}
			}
			return null;	//indicate that the given property value could not be found
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
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
			final List<URFValueContext> valueContextList=propertyURIValueContextsMap.get(propertyURI);	//get the list of value contexts, if any
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
			final List<URFValueContext> valueContextList=propertyURIValueContextsMap.get(propertyURI);	//get the list of value contexts
			return valueContextList!=null ? unmodifiableList(valueContextList) : Iterators.<URFValueContext>emptyIterable();	//return an unmodifiable version of the list, or an empty iterable if there is no list
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

	/**Retrieves the URI of the first value of the property with the given URI.
	All ordered properties will be returned in their correct order before any non-ordered properties.
	Unordered properties will be returned in an arbitrary order.
	@param propertyURI The URI of the property for which the URI of a value should be returned.
	@return The URI of the first value of the property with the given URI, or <code>null</code> if there is no such property or the first property value has no URI.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public URI getPropertyValueURI(final URI propertyURI)
	{
		final URFResource propertyValue=getPropertyValue(propertyURI);	//get the property value, if any
		return propertyValue!=null ? propertyValue.getURI() : null;	//if there is a property value, return its URI
	}

	/**Retrieves an iterable to the values of the property with the given URI.
	All ordered properties will be returned in their correct order before any non-ordered properties.
	Unordered properties will be returned in an arbitrary order.
	@param propertyURI The URI of the property for which values should be returned.
	@return A live iterable to all values of the property with the given URI.
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
	@return A live iterable to all values of the given type of the property with the given URI.
	@exception NullPointerException if the given property URI and/or value class is <code>null</code>.
	*/
	public <V extends URFResource> Iterable<V> getPropertyValues(final URI propertyURI, final Class<V> valueClass)
	{
		checkInstance(valueClass, "Value class cannot be null.");	//make sure the value class isn't null up front; it may be a while before we use it
		readLock().lock();	//get a read lock
		try
		{
			final List<URFValueContext> valueContextList=propertyURIValueContextsMap.get(propertyURI);	//get the list of value contexts
			if(valueContextList!=null && !valueContextList.isEmpty())	//if there is a non-empty context list
			{
				return new Iterable<V>()	//return a new iterable that will return an iterator to the values of the correct type
						{
							public Iterator<V> iterator(){return new PropertyValueIterator<V>(valueContextList, valueClass);}	//iterate over the values of the requested type
						};
			}
			return emptyIterable();	//there is either no context list or no values in the list, so return an empty iterable
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
	}

	/**Adds a property and its scoped properties recursively.
	If the given property and value already exists, the scoped properties, if any, will still be added recursively if they don't exist.
	@param scope The scope containing the properties to add.
	@return <code>true</code> if one or more properties was added, else <code>false</code> if all given property URI and value pairs already existed.
	@exception NullPointerException if the given scope is <code>null</code>.
	*/
	public boolean addAllProperties(final URFScope scope)
	{
		writeLock().lock();	//get a write lock
		try
		{
			boolean propertyAdded=false;	//we'll see if we add a property
			for(final URFProperty property:scope.getProperties())	//for all the properties in the scope
			{
				if(addProperty(property))	//add the given property; if the property was added
				{
					propertyAdded=true;	//we added a property
				}
			}
			return propertyAdded;	//indicate whether we added a new property value context
		}
		finally
		{
			writeLock().unlock();	//always release the write lock
		}
	}
	
	/**Adds a property and its scoped properties recursively.
	If the given property and value already exists, the scoped properties, if any, will still be added recursively if they don't exist.
	@param property The property to add.
	@return <code>true</code> if the property was added, else <code>false</code> if the property URI and value already existed.
	@exception NullPointerException if the given property is <code>null</code>.
	*/
	public boolean addProperty(final URFProperty property)
	{
		writeLock().lock();	//get a write lock
		try
		{
			final URI propertyURI=property.getPropertyURI();	//get the property URI
			final URFResource propertyValue=property.getValue();	//get the property value
			URFValueContext propertyValueContext=null;	//we'll try to find a value context for a matching value
			boolean propertyAdded=false;	//we'll see if we add a property
			final List<URFValueContext> valueContextList=propertyURIValueContextsMap.getCollection(propertyURI);	//get the list of value contexts
			for(final URFValueContext valueContext:valueContextList)	//look at each value context
			{
				if(propertyValue.equals(valueContext.getValue()))	//if we already have a context with this value
				{
					propertyValueContext=valueContext;	//don't add the property value; it's already there
					break;	//stop looking for a matching value context
				}
			}
			if(propertyValueContext==null)	//if there is no existing property value context
			{
				propertyValueContext=new DefaultURFValueContext(this, propertyValue, new ChildURFScope(propertyURI));	//create a new value context
				valueContextList.add(propertyValueContext);	//add a new value context
				++propertyValueCount;	//note that another property has been added
				propertyAdded=true;	//we added a property
			}
			final URFScope propertyValueScope=propertyValueContext.getScope();	//get the scope to which we'll add scoped properties
			for(final URFProperty scopedProperty:property.getScope().getProperties())	//for each scoped property given to us
			{
				propertyValueScope.addProperty(scopedProperty);	//add the scoped property to the existing property scope
			}
			return propertyAdded;	//indicate whether we added a new property value context
		}
		finally
		{
			writeLock().unlock();	//always release the write lock
		}
	}

	/**Adds a property value for the property with the given URI.
	If the given property and value already exists, no action occurs.
	@param propertyURI The URI of the property of the value to add.
	@param propertyValue The value to add for the given property.
	@return <code>true</code> if the value was added for the indicated property, else <code>false</code> if the property and value already existed.
	@exception NullPointerException if the given property URI and/or property value is <code>null</code>.
	*/
	public boolean addPropertyValue(final URI propertyURI, final URFResource propertyValue)
	{
		writeLock().lock();	//get a write lock
		try
		{
			final List<URFValueContext> valueContextList=propertyURIValueContextsMap.getCollection(propertyURI);	//get the list of value contexts
			for(final URFValueContext valueContext:valueContextList)	//look at each value context
			{
				if(propertyValue.equals(valueContext.getValue()))	//if we already have a context with this value
				{
					return false;	//don't add the property value; it's already there
				}
			}
			final URFValueContext valueContext=new DefaultURFValueContext(this, propertyValue, new ChildURFScope(propertyURI));	//create a new value context
			valueContextList.add(valueContext);	//add a new value context
			++propertyValueCount;	//note that another property has been added
			return true;	//indicate that we added a new property value context
		}
		finally
		{
			writeLock().unlock();	//always release the write lock
		}
	}

	/**Adds a date time property value for the property with the given URI.
	If the given property and value already exists, no action occurs.
	@param propertyURI The URI of the property of the value to add.
	@param propertyValue The value to add for the given property.
	@return <code>true</code> if the value was added for the indicated property, else <code>false</code> if the property and value already existed.
	@exception NullPointerException if the given property URI and/or property value is <code>null</code>.
	*/
	public boolean addPropertyValue(final URI propertyURI, final URFDateTime propertyValue)
	{
		return addPropertyValue(propertyURI, DEFAULT_URF_RESOURCE_FACTORY.createDateTimeResource(propertyValue));	//create a resource add the property value
	}

	/**Adds a number property value for the property with the given URI.
	If the given property and value already exists, no action occurs.
	This implementation delegates to {@link #addPropertyValue(URI, long)} for integral number types,
	and to {@link #addPropertyValue(URI, double)} for all other number types.
	@param propertyURI The URI of the property of the value to add.
	@param propertyValue The value to add for the given property.
	@return <code>true</code> if the value was added for the indicated property, else <code>false</code> if the property and value already existed.
	@exception NullPointerException if the given property URI and/or property value is <code>null</code>.
	*/
	public boolean addPropertyValue(final URI propertyURI, final Number propertyValue)
	{
		if(Numbers.isIntegral(propertyValue))	//integral
		{
			return addPropertyValue(propertyURI, propertyValue.longValue());
		}
		else	//decimal and all others
		{
			return addPropertyValue(propertyURI, propertyValue.doubleValue());
		}
	}

	/**Adds an integer property value for the property with the given URI.
	If the given property and value already exists, no action occurs.
	@param propertyURI The URI of the property of the value to add.
	@param propertyValue The value to add for the given property.
	@return <code>true</code> if the value was added for the indicated property, else <code>false</code> if the property and value already existed.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public boolean addPropertyValue(final URI propertyURI, final long propertyValue)
	{
		return addPropertyValue(propertyURI, DEFAULT_URF_RESOURCE_FACTORY.createIntegerResource(propertyValue));	//create a resource add the property value
	}

	/**Adds a rational property value for the property with the given URI.
	If the given property and value already exists, no action occurs.
	@param propertyURI The URI of the property of the value to add.
	@param propertyValue The value to add for the given property.
	@return <code>true</code> if the value was added for the indicated property, else <code>false</code> if the property and value already existed.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public boolean addPropertyValue(final URI propertyURI, final double propertyValue)
	{
		return addPropertyValue(propertyURI, DEFAULT_URF_RESOURCE_FACTORY.createRationalResource(propertyValue));	//create a resource add the property value
	}

	/**Adds a string property value for the property with the given URI.
	If the given property and value already exists, no action occurs.
	@param propertyURI The URI of the property of the value to add.
	@param propertyValue The value to add for the given property.
	@return <code>true</code> if the value was added for the indicated property, else <code>false</code> if the property and value already existed.
	@exception NullPointerException if the given property URI and/or property value is <code>null</code>.
	*/
	public boolean addPropertyValue(final URI propertyURI, final String propertyValue)
	{
		return addPropertyValue(propertyURI, DEFAULT_URF_RESOURCE_FACTORY.createStringResource(propertyValue));	//create a resource add the property value
	}

	/**Adds a URI property value for the property with the given URI.
	If the given property and value already exists, no action occurs.
	@param propertyURI The URI of the property of the value to add.
	@param propertyValue The value to add for the given property.
	@return <code>true</code> if the value was added for the indicated property, else <code>false</code> if the property and value already existed.
	@exception NullPointerException if the given property URI and/or property value is <code>null</code>.
	*/
	public boolean addPropertyValue(final URI propertyURI, final URI propertyValue)
	{
		return addPropertyValue(propertyURI, DEFAULT_URF_RESOURCE_FACTORY.createURIResource(propertyValue));	//create a resource add the property value
	}
	
	/**Sets a value and its scoped properties recursively by removing all properties with the URI of the given property and adding the given property value and scoped properties recursively.
	@param property The property to set.
	@return The old property value, or <code>null</code> if there was no property value previously.
	@exception NullPointerException if the given property is <code>null</code>.
	*/
	public URFResource setProperty(final URFProperty property)
	{
		writeLock().lock();	//get a write lock
		try
		{
			final URI propertyURI=property.getPropertyURI();	//get the property URI
			final List<URFValueContext> valueContextList=propertyURIValueContextsMap.getCollection(propertyURI);	//get the list of value contexts for this property, creating one if necessary
			final int oldPropertyValueCount=valueContextList.size();	//see how many values there currently are
			final URFResource oldPropertyValue=oldPropertyValueCount>0 ? valueContextList.get(0).getValue() : null;	//retrieve the old property value, if any
			propertyValueCount-=oldPropertyValueCount;	//all its contents, if any, will be removed
			valueContextList.clear();	//always clear the context list if we have one
			final URFScope propertyValueScope=new ChildURFScope(propertyURI);	//create a scope for the new property
			final URFValueContext valueContest=new DefaultURFValueContext(this, property.getValue(), propertyValueScope);	//create a new value context for the value
			valueContextList.add(valueContest);	//add the new value context
			++propertyValueCount;	//note that another property has been added
			for(final URFProperty scopedProperty:property.getScope().getProperties())	//for each scoped property given to us
			{
				propertyValueScope.addProperty(scopedProperty);	//add the scoped property to the new property scope
			}
			return oldPropertyValue;	//return the old property value, if any
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
		writeLock().lock();	//get a write lock
		try
		{
			final List<URFValueContext> valueContextList;
			if(propertyValue!=null)	//if there is a property value to set
			{
				valueContextList=propertyURIValueContextsMap.getCollection(propertyURI);	//get the list of value contexts for this property, creating one if necessary
			}
			else	//if there is no new property value
			{
				valueContextList=propertyURIValueContextsMap.get(propertyURI);	//get the list of value contexts for this property, if any, but don't create a new one
			}
			if(valueContextList!=null)	//if there is a value context list
			{
				final int oldPropertyValueCount=valueContextList.size();	//see how many values there currently are
				final URFResource oldPropertyValue=oldPropertyValueCount>0 ? valueContextList.get(0).getValue() : null;	//retrieve the old property value, if any
				propertyValueCount-=oldPropertyValueCount;	//all its contents, if any, will be removed
				if(propertyValue!=null)	//if we have a property value to add
				{
					valueContextList.clear();	//always clear the context list if we have one
					valueContextList.add(new DefaultURFValueContext(this, propertyValue, new ChildURFScope(propertyURI)));	//add a new value context
					++propertyValueCount;	//note that another property has been added
				}
				else	//if we have no properties to add
				{
					propertyURIValueContextsMap.remove(propertyURI);	//remove the entire list of property value contexts
				}
				return oldPropertyValue;	//return the old property value, if any
			}
			else	//if there is no value context list, that can only mean that there was no old value and there will be no new value, so we have nothing to do
			{
				return null;	//there was no old value
			}
		}
		finally
		{
			writeLock().unlock();	//always release the write lock
		}
	}

	/**Sets a date property value for the property with the given URI by removing all properties with the given URI and adding the given property value.
	@param propertyURI The URI of the property of the value to set.
	@param propertyValue The value to set for the given property, or <code>null</code> if there should be no such property.
	@return The old property value, or <code>null</code> if there was no property value previously.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public URFResource setPropertyValue(final URI propertyURI, final URFDate propertyValue)
	{
		return setPropertyValue(propertyURI, propertyValue!=null ? DEFAULT_URF_RESOURCE_FACTORY.createDateResource(propertyValue) : null);	//create a resource and set the property value
	}

	/**Sets a date time property value for the property with the given URI by removing all properties with the given URI and adding the given property value.
	@param propertyURI The URI of the property of the value to set.
	@param propertyValue The value to set for the given property, or <code>null</code> if there should be no such property.
	@return The old property value, or <code>null</code> if there was no property value previously.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public URFResource setPropertyValue(final URI propertyURI, final URFDateTime propertyValue)
	{
		return setPropertyValue(propertyURI, propertyValue!=null ? DEFAULT_URF_RESOURCE_FACTORY.createDateTimeResource(propertyValue) : null);	//create a resource and set the property value
	}

	/**Sets a number property value for the property with the given URI by removing all properties with the given URI and adding the given property value.
	This implementation delegates to {@link #setPropertyValue(URI, long)} for integral number types,
	and to {@link #setPropertyValue(URI, double)} for all other number types.
	@param propertyURI The URI of the property of the value to set.
	@param propertyValue The value to set for the given property, or <code>null</code> if there should be no such property.
	@return The old property value, or <code>null</code> if there was no property value previously.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public URFResource setPropertyValue(final URI propertyURI, final Number propertyValue)
	{
		if(propertyValue!=null)	//if a property value was given
		{
			if(Numbers.isIntegral(propertyValue))	//integral
			{
				return setPropertyValue(propertyURI, propertyValue.longValue());
			}
			else	//decimal and all others
			{
				return setPropertyValue(propertyURI, propertyValue.doubleValue());
			}
		}
		else	//if there is no property value
		{
			return setPropertyValue(propertyURI, (URFResource)null);	//remove the property value
		}
	}

	/**Sets an boolean property value for the property with the given URI by removing all properties with the given URI and adding the given property value.
	@param propertyURI The URI of the property of the value to set.
	@param propertyValue The value to set for the given property.
	@return The old property value, or <code>null</code> if there was no property value previously.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public URFResource setPropertyValue(final URI propertyURI, final boolean propertyValue)
	{
		return setPropertyValue(propertyURI, DEFAULT_URF_RESOURCE_FACTORY.createBooleanResource(propertyValue));	//create a resource and set the property value
	}

	/**Sets an integer property value for the property with the given URI by removing all properties with the given URI and adding the given property value.
	@param propertyURI The URI of the property of the value to set.
	@param propertyValue The value to set for the given property.
	@return The old property value, or <code>null</code> if there was no property value previously.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public URFResource setPropertyValue(final URI propertyURI, final long propertyValue)
	{
		return setPropertyValue(propertyURI, DEFAULT_URF_RESOURCE_FACTORY.createIntegerResource(propertyValue));	//create a resource and set the property value
	}

	/**Sets a rational property value for the property with the given URI by removing all properties with the given URI and adding the given property value.
	@param propertyURI The URI of the property of the value to set.
	@param propertyValue The value to set for the given property.
	@return The old property value, or <code>null</code> if there was no property value previously.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public URFResource setPropertyValue(final URI propertyURI, final double propertyValue)
	{
		return setPropertyValue(propertyURI, DEFAULT_URF_RESOURCE_FACTORY.createRationalResource(propertyValue));	//create a resource and set the property value
	}

	/**Sets a string property value for the property with the given URI by removing all properties with the given URI and adding the given property value.
	@param propertyURI The URI of the property of the value to set.
	@param propertyValue The value to set for the given property, or <code>null</code> if there should be no such property.
	@return The old property value, or <code>null</code> if there was no property value previously.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public URFResource setPropertyValue(final URI propertyURI, final String propertyValue)
	{
		return setPropertyValue(propertyURI, propertyValue!=null ? DEFAULT_URF_RESOURCE_FACTORY.createStringResource(propertyValue) : null);	//create a resource and set the property value
	}

	/**Sets a URI property value for the property with the given URI by removing all properties with the given URI and adding the given property value.
	@param propertyURI The URI of the property of the value to set.
	@param propertyValue The value to set for the given property, or <code>null</code> if there should be no such property.
	@return The old property value, or <code>null</code> if there was no property value previously.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public URFResource setPropertyValue(final URI propertyURI, final URI propertyValue)
	{
		return setPropertyValue(propertyURI, propertyValue!=null ? DEFAULT_URF_RESOURCE_FACTORY.createURIResource(propertyValue) : null);	//create a resource and set the property value
	}

	/**Sets values for the property with the given URI by removing all properties with the given URI and adding the given property values.
	Duplicate property values are ignored.
	@param propertyURI The URI of the property of the value to set.
	@param ordered Whether each added property value should be given a contextual order.
	@param propertyValues The values to set for the given property.
	@return The old property values.
	@exception NullPointerException if the given property URI and/or property values is <code>null</code>.
	*/
	public URFResource[] setPropertyValues(final URI propertyURI, final URFResource... propertyValues)
	{
		return setPropertyValues(propertyURI, false, propertyValues);	//set unordered property values
	}

	/**Sets integer values for the property with the given URI by removing all properties with the given URI and adding the given property values.
	Duplicate property values are ignored.
	@param propertyURI The URI of the property of the value to set.
	@param ordered Whether each added property value should be given a contextual order.
	@param propertyValues The values to set for the given property.
	@return The old property values.
	@exception NullPointerException if the given property URI and/or property values is <code>null</code>.
	*/
	public URFResource[] setPropertyValues(final URI propertyURI, final long... propertyValues)
	{
		final int propertyValueCount=propertyValues.length;	//find the number of properties
		final URFResource[] propertyValueResources=new URFResource[propertyValueCount];	//create an array of resource property values
		for(int i=0; i<propertyValueCount; ++i)	//for each property value
		{
			propertyValueResources[i]=DEFAULT_URF_RESOURCE_FACTORY.createIntegerResource(propertyValues[i]);	//create a resource for this value
		}
		return setPropertyValues(propertyURI, propertyValueResources);	//set the property values
	}

	/**Sets rational values for the property with the given URI by removing all properties with the given URI and adding the given property values.
	Duplicate property values are ignored.
	@param propertyURI The URI of the property of the value to set.
	@param ordered Whether each added property value should be given a contextual order.
	@param propertyValues The values to set for the given property.
	@return The old property values.
	@exception NullPointerException if the given property URI and/or property values is <code>null</code>.
	*/
	public URFResource[] setPropertyValues(final URI propertyURI, final double... propertyValues)
	{
		final int propertyValueCount=propertyValues.length;	//find the number of properties
		final URFResource[] propertyValueResources=new URFResource[propertyValueCount];	//create an array of resource property values
		for(int i=0; i<propertyValueCount; ++i)	//for each property value
		{
			propertyValueResources[i]=DEFAULT_URF_RESOURCE_FACTORY.createRationalResource(propertyValues[i]);	//create a resource for this value
		}
		return setPropertyValues(propertyURI, propertyValueResources);	//set the property values
	}

	/**Sets string values for the property with the given URI by removing all properties with the given URI and adding the given property values.
	Duplicate property values are ignored.
	@param propertyURI The URI of the property of the value to set.
	@param ordered Whether each added property value should be given a contextual order.
	@param propertyValues The values to set for the given property.
	@return The old property values.
	@exception NullPointerException if the given property URI and/or property values is <code>null</code>.
	*/
	public URFResource[] setPropertyValues(final URI propertyURI, final String... propertyValues)
	{
		final int propertyValueCount=propertyValues.length;	//find the number of properties
		final URFResource[] propertyValueResources=new URFResource[propertyValueCount];	//create an array of resource property values
		for(int i=0; i<propertyValueCount; ++i)	//for each property value
		{
			propertyValueResources[i]=DEFAULT_URF_RESOURCE_FACTORY.createStringResource(propertyValues[i]);	//create a resource for this value
		}
		return setPropertyValues(propertyURI, propertyValueResources);	//set the property values
	}

	/**Sets URI values for the property with the given URI by removing all properties with the given URI and adding the given property values.
	Duplicate property values are ignored.
	@param propertyURI The URI of the property of the value to set.
	@param ordered Whether each added property value should be given a contextual order.
	@param propertyValues The values to set for the given property.
	@return The old property values.
	@exception NullPointerException if the given property URI and/or property values is <code>null</code>.
	*/
	public URFResource[] setPropertyValues(final URI propertyURI, final URI... propertyValues)
	{
		final int propertyValueCount=propertyValues.length;	//find the number of properties
		final URFResource[] propertyValueResources=new URFResource[propertyValueCount];	//create an array of resource property values
		for(int i=0; i<propertyValueCount; ++i)	//for each property value
		{
			propertyValueResources[i]=DEFAULT_URF_RESOURCE_FACTORY.createURIResource(propertyValues[i]);	//create a resource for this value
		}
		return setPropertyValues(propertyURI, propertyValueResources);	//set the property values
	}

	/**Sets ordered values for the property with the given URI by removing all properties with the given URI and adding the given property values.
	Duplicate property values are ignored.
	@param propertyURI The URI of the property of the value to set.
	@param ordered Whether each added property value should be given a contextual order.
	@param propertyValues The values to set for the given property.
	@return The old property values.
	@exception NullPointerException if the given property URI and/or property values is <code>null</code>.
	*/
	public URFResource[] setOrderedPropertyValues(final URI propertyURI, final URFResource... propertyValues)
	{
		return setPropertyValues(propertyURI, true, propertyValues);	//set ordered property values
	}

	/**Sets ordered integer values for the property with the given URI by removing all properties with the given URI and adding the given property values.
	Duplicate property values are ignored.
	@param propertyURI The URI of the property of the value to set.
	@param ordered Whether each added property value should be given a contextual order.
	@param propertyValues The values to set for the given property.
	@return The old property values.
	@exception NullPointerException if the given property URI and/or property values is <code>null</code>.
	*/
	public URFResource[] setOrderedPropertyValues(final URI propertyURI, final long... propertyValues)
	{
		final int propertyValueCount=propertyValues.length;	//find the number of properties
		final URFResource[] propertyValueResources=new URFResource[propertyValueCount];	//create an array of resource property values
		for(int i=0; i<propertyValueCount; ++i)	//for each property value
		{
			propertyValueResources[i]=DEFAULT_URF_RESOURCE_FACTORY.createIntegerResource(propertyValues[i]);	//create a resource for this value
		}
		return setOrderedPropertyValues(propertyURI, propertyValueResources);	//set the property values
	}

	/**Sets ordered string values for the property with the given URI by removing all properties with the given URI and adding the given property values.
	Duplicate property values are ignored.
	@param propertyURI The URI of the property of the value to set.
	@param ordered Whether each added property value should be given a contextual order.
	@param propertyValues The values to set for the given property.
	@return The old property values.
	@exception NullPointerException if the given property URI and/or property values is <code>null</code>.
	*/
	public URFResource[] setOrderedPropertyValues(final URI propertyURI, final String... propertyValues)
	{
		final int propertyValueCount=propertyValues.length;	//find the number of properties
		final URFResource[] propertyValueResources=new URFResource[propertyValueCount];	//create an array of resource property values
		for(int i=0; i<propertyValueCount; ++i)	//for each property value
		{
			propertyValueResources[i]=DEFAULT_URF_RESOURCE_FACTORY.createStringResource(propertyValues[i]);	//create a resource for this value
		}
		return setOrderedPropertyValues(propertyURI, propertyValueResources);	//set the property values
	}

	/**Sets ordered URI values for the property with the given URI by removing all properties with the given URI and adding the given property values.
	Duplicate property values are ignored.
	@param propertyURI The URI of the property of the value to set.
	@param ordered Whether each added property value should be given a contextual order.
	@param propertyValues The values to set for the given property.
	@return The old property values.
	@exception NullPointerException if the given property URI and/or property values is <code>null</code>.
	*/
	public URFResource[] setOrderedPropertyValues(final URI propertyURI, final URI... propertyValues)
	{
		final int propertyValueCount=propertyValues.length;	//find the number of properties
		final URFResource[] propertyValueResources=new URFResource[propertyValueCount];	//create an array of resource property values
		for(int i=0; i<propertyValueCount; ++i)	//for each property value
		{
			propertyValueResources[i]=DEFAULT_URF_RESOURCE_FACTORY.createURIResource(propertyValues[i]);	//create a resource for this value
		}
		return setOrderedPropertyValues(propertyURI, propertyValueResources);	//set the property values
	}

	/**Sets values for the property with the given URI by removing all properties with the given URI and adding the given property values.
	Duplicate property values are ignored.
	@param propertyURI The URI of the property of the value to set.
	@param ordered Whether each added property value should be given a contextual order.
	@param propertyValues The values to set for the given property.
	@return The old property values.
	@exception NullPointerException if the given property URI and/or property values is <code>null</code>.
	*/
	protected URFResource[] setPropertyValues(final URI propertyURI, final boolean ordered, final URFResource... propertyValues)
	{
		writeLock().lock();	//get a write lock
		try
		{
			final List<URFValueContext> valueContextList=propertyURIValueContextsMap.getCollection(propertyURI);	//get the list of value contexts for this property, creating one if necessary
			final int oldPropertyValueCount=valueContextList.size();	//see how many values there currently are
			final URFResource[] oldPropertyValues;	//we'll collect the values previously present for this property
			if(oldPropertyValueCount>0)	//if there are old values
			{
				oldPropertyValues=new URFResource[oldPropertyValueCount];	//create a new array of old values
				int oldPropertyValueIndex=0;	//keep track of the old property value index
				for(final URFValueContext oldPropertyValue:valueContextList)	//for each old property value
				{
					oldPropertyValues[oldPropertyValueIndex++]=oldPropertyValue.getValue();	//retrieve this old value and go to the next index
				}
				valueContextList.clear();	//clear the context list; we'll be potentially adding replacements
			}
			else	//if there were no property value contexts before
			{
				oldPropertyValues=NO_RESOURCES;	//there were no resources; use the existing empty array instead of creating a new one
			}
			int order=0;	//start with the first order
			for(final URFResource propertyValue:propertyValues)	//for each new property value
			{
				boolean addPropertyValue=true;	//start out assuming we'll add this property value
				for(final URFValueContext valueContext:valueContextList)	//look at all the existing value contexts to make sure this one doesn't exist
				{
					if(valueContext.getValue().equals(propertyValue))	//if this property value already exists in the list
					{
						addPropertyValue=false;	//don't add this property value
						break;	//stop looking for duplicates
					}
				}
				if(addPropertyValue)	//if we should add this property value
				{
					final URFScope valueScope=new ChildURFScope(propertyURI);	//create a new child scope for the value
					if(ordered)	//if these properties are ordered
					{
						valueScope.setPropertyValue(ORDER_PROPERTY_URI, order++);	//set the scoped order and increment the order variable
					}
					valueContextList.add(new DefaultURFValueContext(this, propertyValue, valueScope));	//add a new value context with the optional scoped order
				}
			}
			propertyValueCount=propertyValueCount-oldPropertyValueCount+propertyValues.length;	//note that properties have been removed and/or added
			return oldPropertyValues;	//return the old property values
		}
		finally
		{
			writeLock().unlock();	//always release the write lock
		}
	}

	/**Removes all properties of this scope.
	@return The number of properties removed.
	*/
	public long removeProperties()
	{
		writeLock().lock();	//get a write lock
		try
		{
			final long removedPropertyCount=propertyValueCount;	//get the number of properties before removal
			propertyURIValueContextsMap.clear();	//remove all property value contexts for all property URIs
			propertyValueCount=0;	//indicate that there are no properties
			return removedPropertyCount;	//return the number of properties we removed
		}
		finally
		{
			writeLock().unlock();	//always release the write lock
		}
	}

	/**Removes all property values for a given property URI.
	@param propertyURI The URI of the property the values of which to remove.
	@return The number of property values removed.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public long removePropertyValues(final URI propertyURI)
	{
		writeLock().lock();	//get a write lock
		try
		{
			final List<URFValueContext> removedValueContextList=propertyURIValueContextsMap.remove(propertyURI);	//remove the list of value contexts, if any
			final int removedValueCount=removedValueContextList!=null ? removedValueContextList.size() : 0;	//find out how many property values were removed
			propertyValueCount-=removedValueCount;	//note that we removed property values
			return removedValueCount;	//return the number of values removed
		}
		finally
		{
			writeLock().unlock();	//always release the write lock
		}
	}

	/**Removes all properties of this scope within a particular namespace.
	@param namespaceURI The URI of the namespace of the properties to be removed.
	@return The number of property values removed.
	@exception NullPointerException if the given namespace URI is <code>null</code>.
	*/
	public long removeNamespacePropertyValues(final URI namespaceURI)
	{
		long removedPropertyCount=0;	//we haven't removed any properties, yet
		writeLock().lock();	//get a write lock
		try
		{
			final Iterator<Map.Entry<URI, List<URFValueContext>>> propertyURIValueContextListEntryIterator=propertyURIValueContextsMap.entrySet().iterator();	//get an iterator to the property map entries
			while(propertyURIValueContextListEntryIterator.hasNext())	//while there are more property map entries
			{
				final Map.Entry<URI, List<URFValueContext>> propertyURIValueContextListEntry=propertyURIValueContextListEntryIterator.next();	//get the next property map entry
				if(namespaceURI.equals(getNamespaceURI(propertyURIValueContextListEntry.getKey())))	//if this property is in the requested namespace
				{
					final int valueCount=propertyURIValueContextListEntry.getValue().size();	//see how many values we're going to remove
					propertyURIValueContextListEntryIterator.remove();	//remove all the values for this property
					propertyValueCount-=valueCount;	//update our total number of properties
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

	/**Removes a given property value for the indicated property and value.
	If the given property and value do not exist, no action occurs.
	This is a convenient method that is equivalent to {@link #removePropertyValue(URI, URFResource)}.
	@param property The property and value to remove.
	@return <code>true</code> if the value was removed from the indicated property, else <code>false</code> if the property and value did not exist.
	@exception NullPointerException if the given property is <code>null</code>.
	*/
	public boolean removeProperty(final URFProperty property)
	{
		return removePropertyValue(property.getPropertyURI(), property.getValue());	//delegate to the property URI and value removal method
	}

	/**Removes a given property value for the property with the given URI.
	If the given property and value do not exist, no action occurs.
	@param propertyURI The URI of the property of the value to remove.
	@param propertyValue The value to remove for the given property.
	@return <code>true</code> if the value was removed from the indicated property, else <code>false</code> if the property and value did not exist.
	@exception NullPointerException if the given property URI and/or property value is <code>null</code>.
	*/
	public boolean removePropertyValue(final URI propertyURI, final URFResource propertyValue)
	{
		writeLock().lock();	//get a write lock
		try
		{
			final List<URFValueContext> valueContextList=propertyURIValueContextsMap.get(propertyURI);	//get the list of value contexts, if any
			if(valueContextList!=null)	//if there is a list of value contexts for this property
			{
				final Iterator<URFValueContext> valueContextIterator=valueContextList.iterator();	//get an iterator to the value contexts
				while(valueContextIterator.hasNext())	//while there are more value contexts
				{
					final URFValueContext valueContext=valueContextIterator.next();	//get the next value context
					if(propertyValue.equals(valueContext.getValue()))	//if this is the value context to remove
					{
						valueContextIterator.remove();	//remove this value context
						--propertyValueCount;	//indicate that we removed a value
						return true;	//indicate that we found and removed the value
					}
				}
				if(valueContextList.isEmpty())	//if the list of value contexts is now empty
				{
					propertyURIValueContextsMap.remove(propertyURI);	//remove the list from the map altogether
				}
			}
			return false;	//indicate that we couldn't find the property and value to remove
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

	/**Returns a hash code for the scope.
	This implementation returns a hash code based upon the number of distinct properties, the number of property values,
	the URIs of properties, and the URIs of property values or the the number of their properties.
	@return A hash code to represent this scope.
	*/
	public int hashCode()
	{
		int hashCode=17;	//start with a base hash code
		readLock().lock();	//get a read lock
		try
		{
			hashCode=37*hashCode+(int)getPropertyCount();	//hash the number of properties; ignore overflows, as all we care about is consistent numbers
			hashCode=37*hashCode+(int)getPropertyValueCount();	//hash the number of property values; ignore overflows, as all we care about is consistent numbers
			for(final Map.Entry<URI, List<URFValueContext>> propertyURIValueContextsEntry:propertyURIValueContextsMap.entrySet())	//look at the entries in the map; these are always returned in the same order
			{
				hashCode=37*hashCode+propertyURIValueContextsEntry.getKey().hashCode();	//hash the property URI hash code
				for(final URFValueContext valueContext:propertyURIValueContextsEntry.getValue())	//look at the value contexts
				{
					final URFResource value=valueContext.getValue();	//get the value
					final URI valueURI=value.getURI();	//get the value's URI, if any
					if(valueURI!=null)	//if the value has a URI
					{
						hashCode=37*hashCode+valueURI.hashCode();	//hash the value URI hash code
					}
					else	//if the value doesn't have a URI
					{
						hashCode=37*hashCode+(int)value.getPropertyCount();	//hash the value's number of properties; ignore overflows, as all we care about is consistent numbers
						hashCode=37*hashCode+(int)value.getPropertyValueCount();	//hash the value's number of property values; ignore overflows, as all we care about is consistent numbers
					}
				}
			}
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
		return hashCode;
	}

	/**Compares this resource with another for equality.
	This implementation returns <code>true</code> if the other object is an URF scope with  the same number of properties and the properties are equal (including scoped properties).
	@param object The object with which to compare this resource.
	@return <code>true<code> if the other object is the same scope or another scope with equal properties and scoped properties.
	*/
	public boolean equals(final Object object)
	{
		if(this==object)	//if the resources are identical
		{
			return true;	//identical resources are always equal
		}
		if(!(object instanceof URFScope))	//if we're being compared with something other than an URF scope
		{
			return false;	//non-scopes aren't equal
		}
		final URFScope scope=(URFScope)object;
		readLock().lock();	//get a read lock
		try
		{
			if(getPropertyCount()!=scope.getPropertyCount())	//see if the number of properties are different
			{
				return false;
			}
			if(getPropertyValueCount()!=scope.getPropertyValueCount())	//see if the number of property values are different
			{
				return false;
			}
			if(hashCode()!=scope.hashCode())	//see if the hash codes are different
			{
				return false;
			}
			for(final URFProperty property:getProperties())	//because everything looks equal so far, we'll have to look at all our properties to make sure
			{
				final URFValueContext scopePropertyValueContext=scope.getPropertyValueContext(property.getPropertyURI(), property.getValue());	//get the context of the property value from the other scope
				if(scopePropertyValueContext==null)	//if the other scope doesn't have this property
				{
					return false;
				}
				if(!property.getScope().equals(scopePropertyValueContext.getScope()))	//even though the property and property value are the same, check the scoped properties; if the scoped properties aren't the same
				{
					return false;
				}
			}
			return true;	//the other scope has all the same properties as this scope
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
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
		This version re-sorts the value orders for the parent scope if this method changes a scoped order.
		@param propertyURI The URI of the property of the value to add.
		@param propertyValue The value to add for the given property.
		@return <code>true</code> if the value was added for the indicated property, else <code>false</code> if the property and value already existed.
		@exception NullPointerException if the given property URI and/or property value is <code>null</code>.
		@exception IllegalStateException if this method is called after this scope is no longer available.
		*/
		public boolean addPropertyValue(final URI propertyURI, final URFResource propertyValue)
		{
			writeLock().lock();	//get a write lock
			try
			{
				final boolean modified=super.addPropertyValue(propertyURI, propertyValue);	//add the property value normally and save whether the scope was modified
				if(modified && ORDER_PROPERTY_URI.equals(propertyURI))	//if the scoped order changed
				{
					resortOrder();	//resort the values of the parent scope
				}
				return modified;	//return whether the scope was modified
			}
			finally
			{
				writeLock().unlock();	//always release the write lock
			}
		}

		/**Sets a property value for the property with the given URI by removing all properties with the given URI and adding the given property value.
		This version re-sorts the value orders for the parent scope if this method changes a scoped order.
		@param propertyURI The URI of the property of the value to set.
		@param propertyValue The value to set for the given property, or <code>null</code> if there should be no such property.
		@return The old property value, or <code>null</code> if there was no property value previously.
		@exception NullPointerException if the given property URI is <code>null</code>.
		@exception IllegalStateException if this method is called after this scope is no longer available.
		@see #resortOrder()
		*/
		public URFResource setPropertyValue(final URI propertyURI, final URFResource propertyValue)
		{
			writeLock().lock();	//get a write lock
			try
			{
				final URFResource oldValue=super.setPropertyValue(propertyURI, propertyValue);	//set the property value normally, saving the old value
				if(ORDER_PROPERTY_URI.equals(propertyURI))	//if the scoped order changed
				{
					resortOrder();	//resort the values of the parent scope
				}
				return oldValue;	//return the old value
			}
			finally
			{
				writeLock().unlock();	//always release the write lock
			}
		}

		/**Removes all property values for a given property URI.
		This version re-sorts the value orders for the parent scope if this method changes a scoped order.
		@param propertyURI The URI of the property the values of which to remove.
		@return The number of properties removed.
		@exception NullPointerException if the given property URI is <code>null</code>.
		@exception IllegalStateException if this method is called after this scope is no longer available.
		@see #resortOrder()
		*/
		public long removePropertyValues(final URI propertyURI)
		{
			writeLock().lock();	//get a write lock
			try
			{
				final long removedPropertyCount=super.removePropertyValues(propertyURI);	//remove the properties normally, saving the number of properties removed
				if(removedPropertyCount>0 && ORDER_PROPERTY_URI.equals(propertyURI))	//if an order property was removed
				{
					resortOrder();	//resort the values of the parent scope
				}
				return removedPropertyCount;	//return the number of properties we removed
			}
			finally
			{
				writeLock().unlock();	//always release the write lock
			}
		}

		/**Removes all properties of this scope within a particular namespace
		This version re-sorts the value orders for the parent scope if this method changes a scoped order.
		@param namespaceURI The URI of the namespace of the properties to be removed.
		@return The number of properties removed.
		@exception NullPointerException if the given namespace URI is <code>null</code>.
		@exception IllegalStateException if this method is called after this scope is no longer available.
		@see #resortOrder()
		*/
		public long removeNamespacePropertyValues(final URI namespaceURI)
		{
			writeLock().lock();	//get a write lock
			try
			{
				final long removedPropertyCount=super.removeNamespacePropertyValues(namespaceURI);	//remove the namespaced properties normally, saving the number of properties removed
				if(removedPropertyCount>0 && NAMESPACE_URI.equals(namespaceURI))	//if all the URF properties were removed, the scoped order might have changed
				{
					resortOrder();	//resort the values of the parent scope
				}
				return removedPropertyCount;	//return the number of properties we removed
			}
			finally
			{
				writeLock().unlock();	//always release the write lock
			}
		}

		/**Removes a given property value for the property with the given URI.
		If the given property and value do not exist, no action occurs.
		This version re-sorts the value orders for the parent scope if this method changes a scoped order.
		@param propertyURI The URI of the property of the value to remove.
		@param propertyValue The value to remove for the given property.
		@return <code>true</code> if the value was removed from the indicated property, else <code>false</code> if the property and value did not exist.
		@exception NullPointerException if the given property URI and/or property value is <code>null</code>.
		@exception IllegalStateException if this method is called after this scope is no longer available.
		*/
		public boolean removePropertyValue(final URI propertyURI, final URFResource propertyValue)
		{
			writeLock().lock();	//get a write lock
			try
			{
				final boolean modified=super.removePropertyValue(propertyURI, propertyValue);	//remove the property value normally and save whether the scope was modified
				if(modified && ORDER_PROPERTY_URI.equals(propertyURI))	//if the scoped order changed
				{
					resortOrder();	//resort the values of the parent scope
				}
				return modified;	//return whether the scope was modified
			}
			finally
			{
				writeLock().unlock();	//always release the write lock
			}

		}

		/**Re-sorts the value orders for the parent scope, which is the containing class.
		This method acquires a write lock.
		@exception IllegalStateException if this method is called after this scope is no longer available.
		*/
		protected void resortOrder()
		{
			writeLock().lock();	//get a write lock
			try
			{
				final List<URFValueContext> valueContextList=AbstractURFScope.this.propertyURIValueContextsMap.get(getPropertyURI());	//get the list of value contexts from the parent scope
				if(valueContextList==null)	//if there is no value context for that property, this scope must be being used after the property was removed
				{
					throw new IllegalStateException("Scope for property URI "+getPropertyURI()+" being used after property value was changed.");
				}
				sort(valueContextList, VALUE_CONTEXT_COMPARATOR);	//re-sort the values based upon the new scoped order
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
			this(namespaceURI, propertyURIs!=null ? propertyURIs.iterator() : propertyURIValueContextsMap.keySet().iterator());	//construct the class using an iterator to all available property URIs if none were given
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
					final List<URFValueContext> valueContextList=propertyURIValueContextsMap.get(propertyURI);	//get the list of value contexts for this property
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
		@exception UnsupportedOperationException if the remove operation is not supported by this iterator.
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
	private class PropertyValueIterator<V extends URFResource> extends AbstractListIterator<V, URFValueContext>	//TODO fix; by allowing this iterator to be live, we could end up leaving orphaned value context lists
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
			updateIncludedIndexes();	//initialize the iterator
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
			--propertyValueCount;	//note that a property has been removed
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
