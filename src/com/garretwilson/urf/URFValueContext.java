package com.garretwilson.urf;

import java.net.URI;
import java.util.concurrent.locks.ReadWriteLock;

/**An URF value with its scope.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public interface URFValueContext extends ReadWriteLock
{

	/**@return The value resource.*/
	public URFResource getValue();

	/**The scope of the value in the context of some property.*/ 
	public URFScope getScope();

	/**Determines whether there exists a property with the given property URI in this context,
	either as a property of the context value or as a property of the context scope.
	@param propertyURI The URI of the property to check.
	@return <code>true</code> if a property exists with the given property URI.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public boolean hasProperty(final URI propertyURI);

	/**Retrieves the first value context of the property with the given URI in this context,
	either as a property of the context value or as a property of the context scope.
	All ordered properties will be returned in their correct order before any non-ordered properties.
	Unordered properties will be returned in an arbitrary order. 
	@param propertyURI The URI of the property for which a value context should be returned.
	@return The first value context of the property with the given URI, or <code>null</code> if there is no such property.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public URFValueContext getPropertyValueContext(final URI propertyURI);

	/**Retrieves the first value of the property with the given URI in this context,
	either as a property of the context value or as a property of the context scope.
	All ordered properties will be returned in their correct order before any non-ordered properties.
	Unordered properties will be returned in an arbitrary order. 
	@param propertyURI The URI of the property for which a value should be returned.
	@return The first value of the property with the given URI, or <code>null</code> if there is no such property.
	@exception NullPointerException if the given property URI is <code>null</code>.
	*/
	public URFResource getPropertyValue(final URI propertyURI);

}
