package com.garretwilson.urf;

import java.net.URI;

import com.garretwilson.net.Resource;

/**An URF resource.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public interface URFResource extends Resource, URFScope
{

	/**Returns the string value of the «{@value URF#LABEL_PROPERTY_URI}» property.
	@return The string value of the label property, or <code>null</code> if there is no such property or the property value is not a string.
	*/
	public String getLabel();

	/**Determines a string value to use for representation.
	This method may take into account the current properties of the resource in order to provide the best possible string representation.
	@return A string label to use for representation of the resource.
	*/
	public String determineLabel();

	/**Retrieves the types declared for this resource, if any.
	@return An iterable to all types declared for this resource.
	@see URF#TYPE_PROPERTY_URI
	*/
	public Iterable<URFResource> getTypes();

	/**Determines whether this resource has a type with the given URI.
	@param typeURI The URI of the type for which to search.
	@return <code>true</code> if this resource has a type with the given URI.
	@exception NullPointerException if the given type URI is <code>null</code>.
	@see URF#TYPE_PROPERTY_URI
	*/
	public boolean hasTypeURI(final URI typeURI);

	/**Retrieves the first type declared for this resource, if any.
	@return The first type declared for this resource, or <code>null</code> if no types are declared for this resource.
	@see URF#TYPE_PROPERTY_URI
	*/
	public URFResource getType();

	/**Retrieves the URI of the first type declared for this resource, if any.
	@return The URI of the first type declared for this resource, or <code>null</code> if no types are declared for this resource or the first type has no URI.
	@see URF#TYPE_PROPERTY_URI
	*/
	public URI getTypeURI();

	/**Adds a type.
	@param type The type to add.
	*/
	public void addType(final URFResource type);

	/**Adds a type by the type URI.
	@param typeURI The URI of the type to add.
	*/
	public void addTypeURI(final URI typeURI);
}
