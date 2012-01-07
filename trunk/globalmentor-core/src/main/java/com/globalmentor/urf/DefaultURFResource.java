/*
 * Copyright © 2007-2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.urf;

import java.net.URI;
import java.util.concurrent.locks.*;

import com.globalmentor.net.URIs;

import static com.globalmentor.java.Objects.*;
import static com.globalmentor.urf.URF.*;
import static com.globalmentor.urf.dcmi.DCMI.getTitle;

/**The default implementation of an URF resource.
<p>Resources with equivalent non-<code>null</code> URIs are considered equal for {@link #equals(Object)};
otherwise, resources are equal only if they both have <code>null</code> URIs and they have the same properties with equivalent values.</p>
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

	/**Returns the icon of this resource, if any.
	@return The URI value of the icon property, or <code>null</code> if there is no such property or the property value is not an icon.
	@see URF#ICON_PROPERTY_URI
	*/
	public URI getIcon()
	{
		return asURI(getPropertyValue(ICON_PROPERTY_URI));
	}

	/**Set the icon of this resource.
	@param icon The new icon, or <code>null</code> if there should be no icon.
	@see URF#ICON_PROPERTY_URI
	*/
	public void setIcon(final URI icon)
	{
		setPropertyValue(ICON_PROPERTY_URI, icon);
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
		<li>The lexical form of any resource with a URI in an inline namespace.</li>
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
					if(isInlineURI(uri))	//if the URI is an inline URI
					{
						label=getInlineLexicalForm(uri);	//get the lexical form
					}
					else	//if the URI is not in an inline namespace
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
					label=super.toString();	//use the Java string form of the resource as a last resort
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

	/**Alters the resource according to the given resource alteration specification.
	@param resourceAlteration The specification of the alterations to be performed on the resource.
	@return This resource after the alterations.
	*/
	public URFResource alter(final URFResourceAlteration resourceAlteration)
	{
		writeLock().lock();	//get a write lock
		try
		{
			for(final URI removePropertyURI:resourceAlteration.getPropertyURIRemovals())	//look at each property URI to remove
			{
				removePropertyValues(removePropertyURI);	//remove all the values for this property URI
			}
			for(final URFProperty removeProperty:resourceAlteration.getPropertyRemovals())	//look at each property to remove
			{
				removeProperty(removeProperty);	//remove the property
			}
			for(final URFProperty addProperty:resourceAlteration.getPropertyAdditions())	//look at each property to add
			{
				addProperty(addProperty);	//add the property
			}
			return this;	//return this altered resource
		}
		finally
		{
			writeLock().unlock();	//always release the write lock
		}
	}
	
	/**Returns a hash code for the resource.
	If this resource has a URI, this implementation returns the hash code of the URI.
	Otherwise, this implementation returns the hash code of the underlying scope.
	@return A hash code to represent this resource.
	*/
	public int hashCode()	//TODO cache the hash codes, both here and for individual properties; use them in determining equals
	{
		final URI uri=getURI();	//get the resource URI
		return uri!=null ? uri.hashCode() : super.hashCode();	//if there is no URI, return the hash code produced by the scope implementation
	}

	/**Compares this resource with another for equality.
	This implementation returns <code>true</code> if the other object is an URF resource
	and either both resources have the same non-<code>null</code> URI, or neither resource
	has a URI but they have the same number of properties and the properties are equal
	(including scoped properties).
	@param object The object with which to compare this resource.
	@return <code>true<code> if the other object is the same resource, an URF resource with the same non-<code>null</code> URI, or a resource with the same <code>null</code> URI and equal properties.
	@see #getURI()
	*/
	public boolean equals(final Object object)
	{
		if(this==object)	//if the resources are identical
		{
			return true;	//identical resources are always equal
		}
		if(!(object instanceof URFResource))	//if we're being compared with something other than an URFresource
		{
			return false;	//non-resources aren't equal
		}
		final URFResource resource=(URFResource)object;
		final URI uri=getURI();	//get the resource URI
		final URI resourceURI=resource.getURI();	//get the URI of the other resource
		if(uri!=null)	//if this resource has a URI
		{
			return uri.equals(resourceURI);	//compare URIs
		}
		else	//if this resource has no URI
		{
			if(resourceURI!=null)	//if the other resource has a URI, and this one doesn't they aren't equal
			{
				return false;
			}
			return super.equals(object);	//neither resource has a URI; compare the scopes without regard to the URI
		}
	}

	/**Returns a string representation of the resource.
	This version returns the URI, if there is one, between TURF URI reference delimiters; otherwise a compact yet human-oriented TURF representation is returned.
	@return A string representation of the resource.
	*/
	public String toString()
	{
		final URI uri=getURI();	//get the URI, if any
		return uri!=null ? URFTURFGenerator.toReferenceString(uri) : URF.toString(this);	//return the URI, if available; otherwise, generate a string for the source
	}

}