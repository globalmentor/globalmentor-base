package com.garretwilson.urf;

import java.net.URI;
import java.util.concurrent.locks.*;

import com.garretwilson.lang.LongUtilities;
import static com.garretwilson.urf.URF.*;
import static com.garretwilson.urf.TURF.*;
import static com.garretwilson.urf.dcmi.DCMI.getTitle;

/**The default implementation of an URF resource.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) <http://www.urf.name/> specification and processing
written by Garret Wilson <http://www.garretwilson.com/> and Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public class DefaultURFResource extends AbstractURFScope implements URFResource
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
	@param resource The URF resource from which resources should be copied.
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

	/**Returns the string value of the «{@value URF#LABEL_PROPERTY_URI}» property.
	@return The string value of the label property, or <code>null</code> if there is no such property or the property value is not a string.
	*/
	public String getLabel()
	{
		return asString(getPropertyValue(LABEL_PROPERTY_URI));
	}

	/**Determines a string value to use for representation.
	This method may take into account the current properties of the resource in order to provide the best possible string representation.
	This implementation determines the label in the following sequence:
	<ol>
		<li>The string value of any literal «{@value URF#LABEL_PROPERTY_URI}» property.</li>
		<li>The string value of any literal «{@value DCMI#TITLE_PROPERTY_URI}» property.</li>
		<li>The lexical form of any resource with a URI in a lexical namespace.</li>
		<li>The reference URI.</li>
		<li>The Java string representation of the resource as given by its <code>toString()</code> method.</li>
	</ol>
	@return A string label to use for representation of the resource.
	@see #getLabel()
	*/
	public String determineLabel()
	{
		String label=getTitle(this);	//see if there is a dc.title
		if(label==null)	//if there is no title
		{
			final URI uri=getURI();	//get the resource URI
			if(uri!=null)	//if this resource has a URI
			{
				if(isLexicalNamespaceURI(uri))	//if the URI is in a lexical namespace
				{
					label=getLocalName(uri);	//get the local name of the URI, which will be the lexical form
				}
				else	//if the URI is not in a lexical namespace
				{
					label=uri.toString();	//use the string form of the URI as-is
				}
			}
			else	//if there is no URI
			{
				label=toString();	//use the Java string form of the resource as a last resort
			}
		}
		return label;	//return the label
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

	/**Retrieves the first type declared for this resource, if any.
	@return The first type declared for this resource, or <code>null</code> if no types are declared for this resource.
	@see URF#TYPE_PROPERTY_URI
	*/
	public URFResource getType()
	{
		return getPropertyValue(TYPE_PROPERTY_URI);	//return the first type
	}

	/**Retrieves the URI of the first type declared for this resource, if any.
	@return The URI of the first type declared for this resource, or <code>null</code> if no types are declared for this resource or the first type has no URI.
	@see URF#TYPE_PROPERTY_URI
	*/
	public URI getTypeURI()
	{
		return getPropertyValueURI(TYPE_PROPERTY_URI);	//return the URI of the first type
	}

	/**Adds a type.
	@param type The type to add.
	*/
	public void addType(final URFResource type)
	{
		addPropertyValue(TYPE_PROPERTY_URI, type);	//add the given resource as a type
	}

	/**Adds a type by the type URI.
	@param typeURI The URI of the type to add.
	*/
	public void addTypeURI(final URI typeURI)
	{
		addPropertyValue(TYPE_PROPERTY_URI, typeURI);	//add a type resource for the given type URI
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