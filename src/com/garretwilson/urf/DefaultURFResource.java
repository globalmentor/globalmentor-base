package com.garretwilson.urf;

import java.net.URI;
import java.util.concurrent.locks.*;

import static com.garretwilson.urf.URF.*;
import static com.garretwilson.urf.TURF.*;

/**The default implementation of an URF resource.
This class provides compare functionality that sorts according to the resource URI.
@author Garret Wilson
*/
public class DefaultURFResource extends AbstractURFScope implements URFResource	//TODO fix, Cloneable
{

	/**The URI, or <code>null</code> if there is no URI.*/
	private URI uri;

		/**@return The URI, or <code>null</code> if there is no URI.*/
		public URI getURI() {return uri;}

	/**Default constructor with no URI.*/
	public DefaultURFResource()
	{
		this(null);	//create a resource without a URI
	}

	/**Constructs a resource with a URI.
	@param uri The URI for the resource.
	*/
	public DefaultURFResource(final URI uri)
	{
		super(new ReentrantReadWriteLock(), null);	//construct the parent class using a default lock and no parent scope
		this.uri=uri;	//save the URI, if any
	}

	/**Retrieves the types declared for this resource, if any.
	@return An iterable to all types declared for this resource.
	@see URF#TYPE_PROPERTY_URI
	*/
	public Iterable<URFResource> getTypes()
	{
		return getPropertyValues(TYPE_PROPERTY_URI);	//return all the type property values
	}

	/**Determines whether this resource has a type with the given URI.
	@param typeURI The URI of the type for which to search.
	@return <code>true</code> if this resource has a type with the given URI.
	@see URF#TYPE_PROPERTY_URI
	*/
	public boolean hasType(final URI typeURI)
	{
		return hasPropertyValueURI(TYPE_PROPERTY_URI, typeURI);	//check for the given type URI
	}

//TODO add hash and equals() methods
	
	/**Returns a string representation of the resource.
	This version returns the URI, if there is one, between TURF URI reference delimiters; otherwise the default string representation of the object is returned.
	@return A string representation of the resource.
	*/
	public String toString()
	{
		final URI uri=getURI();	//get the URI, if any
		return uri!=null ? new StringBuilder().append(REFERENCE_BEGIN).append(uri).append(REFERENCE_END).toString() : super.toString();	//return the URI, if available
	}
}