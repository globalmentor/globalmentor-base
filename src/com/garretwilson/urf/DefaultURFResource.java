package com.garretwilson.urf;

import java.net.URI;
import java.util.concurrent.locks.*;

import com.garretwilson.lang.LongUtilities;
import static com.garretwilson.urf.URF.*;
import static com.garretwilson.urf.TURF.*;

/**The default implementation of an URF resource.
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

	/**URI constructor.
	@param uri The URI for the resource, or <code>null</code> if the resource should have no URI.
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

	/**@return A hashcode value composed from the reference URI, if available.*/
	public int hashCode()
	{
		final URI uri=getURI();	//get the resource URI
		return uri!=null ? uri.hashCode() : LongUtilities.hashCode(getCreationOrder());	//return the URI hash code, or the creation order hash code if there is no URI available
	}

	/**Compares this resource with another for equality.
	If this object has a URI and the other object is an URF resource with a URI, the URIs are compared.
	Otherwise, the default identity comparison is performed.
	@param object The object with which to compare this resource.
	@return <code>true<code> if the other object is the same resource or an URF resource the same non-<code>null</code> URI.
	@see #getURI()
	*/
	public boolean equals(final Object object)
	{
		if(object instanceof URFResource)	//if we're being compared with another URFresource
		{
			final URI uri=getURI();	//get the reference URI
			if(uri!=null)	//if this resource has a reference URI
			{
				return uri.equals(((URFResource)object).getURI());	//compare reference URIs
			}
			else	//if this resource has no reference URI
			{
				return super.equals(object);	//compare normally
			}
		}
		else	//if the object is not an URF resource
		{
			return false;	//we can't compare this object to a non-resource object
		}
	}

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