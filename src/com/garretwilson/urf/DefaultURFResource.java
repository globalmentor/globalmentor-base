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
		this(null, NO_RESOURCES);	//create a resource without a URI or types
	}

	/**URI and type URIs constructor.
	@param uri The URI for the resource, or <code>null</code> if the resource should have no URI.
	@param typeURIs The URIs of the types, if any, to add to the resource.
	*/
	public DefaultURFResource(final URI uri, final URI... typeURIs)
	{
		this(uri, createResources(typeURIs));	//create resources for the types and construct this class
	}

	/**URI and types constructor.
	@param uri The URI for the resource, or <code>null</code> if the resource should have no URI.
	@param types The types of the resource, if any.
	*/
	private DefaultURFResource(final URI uri, final URFResource... types)
	{
		super(new ReentrantReadWriteLock(), null);	//construct the parent class using a default lock and no parent scope
		this.uri=uri;	//save the URI, if any
	}

	/**Copy constructor.
	The new resource will use the same URI, if any, as the old.
	All properties will be copied from the given resource to the new one.
	@param resource The URF resource from which resources should be copied.
	@exception NullPointerException if the given resource is <code>null</code>.
	*/
	public DefaultURFResource(final URFResource resource)
	{
		this(resource, resource.getURI());	//create the resource using the existing resource's URI
	}

	/**Copy constructor with a specified URI.
	All properties will be copied from the given resource to the new one.
	@param resource The RDF resource from which resources should be copied.
	@param uri The URI for the new resource.
	@exception NullPointerException if the given resource is <code>null</code>.
	*/
	public DefaultURFResource(final URFResource resource, final URI uri)
	{
		this(uri, NO_RESOURCES);	//create the resource with the given URI
		writeLock().lock();	//get a write lock on this resource
		try
		{
			readLock().lock();	//get a read lock on the other resource
			try
			{
				for(final URFProperty property:resource.getProperties())	//for each property in the other resource
				{
					addPropertyValue(property.getPropertyURI(), property.getValue());	//add this property TODO add scoped properties
				}
			}
			finally
			{
				readLock().unlock();	//always release the read lock
			}
		}
		finally
		{
			writeLock().unlock();	//always release the write lock
		}
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
	@exception NullPointerException if the given type URI is <code>null</code>.
	@see URF#TYPE_PROPERTY_URI
	*/
	public boolean hasTypeURI(final URI typeURI)
	{
		return hasPropertyValueURI(TYPE_PROPERTY_URI, typeURI);	//check for the given type URI
	}

	/**Adds a type.
	@param type The type to add.
	*/
	public void addType(final URFResource type)
	{
		addPropertyValue(TYPE_PROPERTY_URI, type);	//add the given resource as a type
	}

	/**Creates default resources from the given URIs.
	@param uris The URIs, each of which may be <code>null</code>, of the resources to create.
	@return An array of default resources with the given URIs.
	*/
	protected final static URFResource[] createResources(final URI... uris)
	{
		final int uriCount=uris.length;	//find out how many URIs there are
		if(uriCount>0)	//if there is at least one URI
		{
			final URFResource[] resources=new URFResource[uriCount];	//create a new array of resources
			for(int i=0; i<uriCount; ++i)	//for each URI
			{
				resources[i]=new DefaultURFResource(uris[i], NO_RESOURCES);	//create a new default resource
			}
			return resources;	//return the resources we created
		}
		else	//if there are no URIs given
		{
			return NO_RESOURCES;	//return the pre-fabricated resource array
		}
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