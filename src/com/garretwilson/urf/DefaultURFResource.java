package com.garretwilson.urf;

import java.net.URI;
import java.util.concurrent.locks.*;

import com.garretwilson.lang.LongUtilities;
import static com.garretwilson.lang.Objects.*;
import com.garretwilson.net.URIs;
import static com.garretwilson.urf.URF.*;
import static com.garretwilson.urf.TURF.*;
import static com.garretwilson.urf.dcmi.DCMI.getTitle;

/**The default implementation of an URF resource.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public class DefaultURFResource extends AbstractURFScope implements URFResource
{

	/**The URI, or <code>null</code> if there is no URI.*/
	private URI uri;

		/**@return The URI, or <code>null</code> if there is no URI.*/
		public URI getURI() {return uri;}

		/**Sets the URI.
		@param uri The new URI, or <code>null</code> if there is no URI.
		*/
		protected void setURI(final URI uri) {this.uri=uri;}

	/**Default constructor with no URI.*/
	public DefaultURFResource()
	{
		this((URI)null);	//create a resource without a URI or types
	}

	/**URI and type URIs constructor.
	@param uri The URI for the resource, or <code>null</code> if the resource should have no URI.
	@param typeURIs The URIs of the types, if any, to add to the resource.
	@exception NullPointerException if one or more of the given type URIs is <code>null</code>.
	*/
	public DefaultURFResource(final URI uri, final URI... typeURIs)
	{
		super(new ReentrantReadWriteLock(), null);	//construct the parent class using a default lock and no parent scope
		this.uri=uri;	//save the URI, if any
		for(final URI typeURI:typeURIs)	//for each type URI
		{
			addTypeURI(checkInstance(typeURI, "Type URI cannot be null."));	//add a type for this URI
		}
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
		this(uri);	//create the resource with the given URI
		writeLock().lock();	//get a write lock on this resource
		try
		{
			resource.readLock().lock();	//get a read lock on the other resource
			try
			{
				for(final URFProperty property:resource.getProperties())	//for each property in the other resource
				{
					addPropertyValue(property.getPropertyURI(), property.getValue());	//add this property TODO add scoped properties
				}
			}
			finally
			{
				resource.readLock().unlock();	//always release the read lock
			}
		}
		finally
		{
			writeLock().unlock();	//always release the write lock
		}
	}

	/**Retrieves the first list of inits declared for this resource, if any.
	@return The first list of inits declared for this resource, or <code>null</code> if no inits list is declared for this resource.
	@see URF#INITS_PROPERTY_URI
	*/
	public URFListResource<?> getInits()
	{
		return asListInstance(getPropertyValue(INITS_PROPERTY_URI));	//return the first inits list
	}

	/**Returns the label of this resource, if any.
	@return The string value of the label property, or <code>null</code> if there is no such property or the property value is not a string.
	@see URF#LABEL_PROPERTY_URI
	*/
	public String getLabel()
	{
		return asString(getPropertyValue(LABEL_PROPERTY_URI));
	}

	/**Set the label of this resource.
	@param label The new label, or <code>null</code> if there should be no label.
	@see URF#LABEL_PROPERTY_URI
	*/
	public void setLabel(final String label)
	{
		setPropertyValue(LABEL_PROPERTY_URI, label);
	}

	/**Determines a string value to use for representation.
	This method may take into account the current properties of the resource in order to provide the best possible string representation.
	This implementation determines the label in the following sequence:
	<ol>
		<li>The string value of any «{@value URF#LABEL_PROPERTY_URI}» property.</li>
		<li>The string value of any «{@value DCMI#TITLE_PROPERTY_URI}» property.</li>
		<li>The lexical form of any resource with a URI in a lexical namespace.</li>
		<li>The decoded last past segment of a hierarchical URI.</li>
		<li>The reference URI.</li>
		<li>The Java string representation of the resource as given by its <code>toString()</code> method.</li>
	</ol>
	@return A string label to use for representation of the resource.
	@see #getLabel()
	*/
	public String determineLabel()
	{
		String label=getLabel();	//see if there is an urf.label
		if(label==null)	//if there is no label
		{
			label=getTitle(this);	//see if there is a dc.title
			if(label==null)	//if there is no title
			{
				final URI uri=getURI();	//get the resource URI
				if(uri!=null)	//if this resource has a URI
				{
					if(isLexicalURI(uri))	//if the URI is a lexical URI
					{
						label=getLocalName(uri);	//get the local name of the URI, which will be the lexical form
					}
					else	//if the URI is not in a lexical namespace
					{
						label=URIs.getName(uri);	//get the name of the URI, if any
						if(label==null)	//if no name could be determined from the URI
						{
							label=uri.toString();	//use the string form of the URI as-is
						}
					}
				}
				else	//if there is no URI
				{
					label=toString();	//use the Java string form of the resource as a last resort
				}
			}
		}
		return label;	//return the label
	}

	/**Returns the name of this resource, if any.
	@return The string value of the name property, or <code>null</code> if there is no such property or the property value is not a string.
	@see URF#NAME_PROPERTY_URI
	*/
	public String getName()
	{
		return asString(getPropertyValue(NAME_PROPERTY_URI));
	}

	/**Set the name of this resource.
	@param name The new name, or <code>null</code> if there should be no name.
	@see URF#NAME_PROPERTY_URI
	*/
	public void setName(final String name)
	{
		setPropertyValue(NAME_PROPERTY_URI, name);
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

	/**Adds a type specified by the type URI.
	@param typeURI The URI of the type to add.
	*/
	public void addTypeURI(final URI typeURI)
	{
		addPropertyValue(TYPE_PROPERTY_URI, new DefaultURFResource(typeURI));	//add a type resource for the given type URI
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
		return determineLabel();
/*TODO bring back
		final URI uri=getURI();	//get the URI, if any
		return uri!=null ? new StringBuilder().append(REFERENCE_BEGIN).append(uri).append(REFERENCE_END).toString() : super.toString();	//return the URI, if available
*/
	}

}