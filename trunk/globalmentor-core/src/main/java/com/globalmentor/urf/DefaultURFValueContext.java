package com.globalmentor.urf;

import java.net.URI;
import java.util.concurrent.locks.ReadWriteLock;

import static com.globalmentor.java.Objects.*;

import com.globalmentor.collections.ReadWriteLockDecorator;

/**Default implementatioon of An URF value with its scope.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public class DefaultURFValueContext extends ReadWriteLockDecorator implements URFValueContext
{

	/**The value resource.*/
	private final URFResource value;

		/**@return The value resource.*/
		public URFResource getValue() {return value;}

	/**The scope of the value in the context of some property.*/ 
	private final URFScope scope;

		/**The scope of the value in the context of some property.*/ 
		public URFScope getScope() {return scope;}

	/**Read write lock, value, and scope constructor.
	@param readWriteLock The lock for controlling access to the properties.
	@param value The value resource.
	@param scope The scope of the value in the context of some property.
	@exception NullPointerException if the given lock, value, and/or scope is <code>null</code>.
	*/
	public DefaultURFValueContext(final ReadWriteLock readWriteLock, final URFResource value, final URFScope scope)
	{
		super(readWriteLock);	//construct the parent class
		this.value=checkInstance(value, "Value cannot be null.");
		this.scope=checkInstance(scope, "Scope cannot be null.");
	}

	/**Determines whether there exists a property with the given property URI in this context,
	either as a property of the context value or as a property of the context scope.
	@param propertyURI The URI of the property to check.
	@return <code>true</code> if a property exists with the given property URI.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public boolean hasProperty(final URI propertyURI)
	{
		readLock().lock();	//get a read lock
		try
		{
			return getValue().hasProperty(propertyURI) || getScope().hasProperty(propertyURI);	//see if the value or the scope has the property
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
	}

	/**Retrieves the first value context of the property with the given URI in this context,
	either as a property of the context value or as a property of the context scope.
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
			URFValueContext valueContext=getValue().getPropertyValueContext(propertyURI);	//try to get the value context from the value
			if(valueContext==null)	//if the value has no value context for this property
			{
				valueContext=getScope().getPropertyValueContext(propertyURI);	//try to get the value context from the scope
			}
			return valueContext;	//return the value context we found, if any
		}
		finally
		{
			readLock().unlock();	//always release the read lock
		}
	}

	/**Retrieves the first value of the property with the given URI in this context,
	either as a property of the context value or as a property of the context scope.
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
}
