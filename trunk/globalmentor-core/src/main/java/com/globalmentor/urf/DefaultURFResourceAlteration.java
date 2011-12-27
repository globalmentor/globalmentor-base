/*
 * Copyright © 2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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
import java.util.*;

import com.globalmentor.collections.Sets;

import static java.util.Arrays.*;
import static java.util.Collections.*;

import static com.globalmentor.java.Objects.*;

/**The encapsulation of changes to be performed on a resource, consisting of properties to be removed and/or added.
Alternations are to be performed in the following order:
<ol>
	<li>Any property URI-based removals are performed.</li>
	<li>Any property value removals, for property URIs not removed, are performed.</li>
	<li>Any property value additions are performed.</li>
</ol>
To set a property to the exclusion of all other values, a property addition should be provided and its property URI
included in the set of property URI removals. 
<p>If no resource URI is provided, it means that the current URI of the resource should remain unchanged.
Because of these semantics, this interface does not provide a means for indicating a resource should become unnamed.</p>
@author Garret Wilson
*/
public class DefaultURFResourceAlteration implements URFResourceAlteration
{

	/**The new URI of the resource, or <code>null</code> if the current URI of the resource, if any, should remain unchanged.*/
	private final URI resourceURI;

		/**@return The new URI of the resource, or <code>null</code> if the current URI of the resource, if any, should remain unchanged.*/
		public URI getResourceURI() {return resourceURI;}

	/**The immutable set of URIs of properties to remove.*/
	private final Set<URI> propertyURIRemovals; 

		/**@return The immutable set of URIs of properties to remove.*/
		public Set<URI> getPropertyURIRemovals() {return propertyURIRemovals;} 

	/**The immutable set of properties and values to remove.*/
	private final Set<URFProperty> propertyRemovals; 

		/**@return The immutable set of properties and values to remove.*/
		public Set<URFProperty> getPropertyRemovals() {return propertyRemovals;} 

	/**The immutable set of properties and values to add.*/
	private final Set<URFProperty> propertyAdditions; 

		/**@return The immutable set of properties and values to add.*/
		public Set<URFProperty> getPropertyAdditions() {return propertyAdditions;} 

	/**Default constructor with no alterations.*/
	DefaultURFResourceAlteration()
	{
		this(null, Collections.<URI>emptySet(), Collections.<URFProperty>emptySet(), Collections.<URFProperty>emptySet());
	}

	/**Property URI removals and property additions constructor.
	No resource URI change is indicated.
	@param propertyURIRemovals The URIs of properties to remove.
	@param propertyAdditions The properties and values to add.
	@throws NullPointerException if the given property URI removals and/or property additions is <code>null</code>.
	*/
	public DefaultURFResourceAlteration(final Iterable<URI> propertyURIRemovals, final Iterable<URFProperty> propertyAdditions)
	{
		this(propertyURIRemovals, Collections.<URFProperty>emptySet(), propertyAdditions);	//construct the class without indicating property removals
	}

	/**Resource URI, property URI removals, property additions constructor.
	@param resourceURI The new URI of the resource, or <code>null</code> if the current URI of the resource, if any, should remain unchanged.
	@param propertyURIRemovals The URIs of properties to remove.
	@param propertyAdditions The properties and values to add.
	@throws NullPointerException if the given property URI removals and/or property additions is <code>null</code>.
	*/
	public DefaultURFResourceAlteration(final URI resourceURI, final Iterable<URI> propertyURIRemovals, final Iterable<URFProperty> propertyAdditions)
	{
		this(resourceURI, propertyURIRemovals, Collections.<URFProperty>emptySet(), propertyAdditions);	//construct the class without indicating property removals
	}
		
	/**Property URI removals, property removals, and property additions constructor.
	No resource URI change is indicated.
	@param propertyURIRemovals The URIs of properties to remove.
	@param propertyRemovals The properties and values to remove.
	@param propertyAdditions The properties and values to add.
	@throws NullPointerException if the given property URI removals, property removals, and/or property additions is <code>null</code>.
	*/
	public DefaultURFResourceAlteration(final Iterable<URI> propertyURIRemovals, final Iterable<URFProperty> propertyRemovals, final Iterable<URFProperty> propertyAdditions)
	{
		this(null, propertyURIRemovals, propertyRemovals, propertyAdditions);	//construct the class without indicating a new resource URI
	}

	/**Resource URI, property URI removals, property removals, and property additions constructor.
	@param resourceURI The new URI of the resource, or <code>null</code> if the current URI of the resource, if any, should remain unchanged.
	@param propertyURIRemovals The URIs of properties to remove.
	@param propertyRemovals The properties and values to remove.
	@param propertyAdditions The properties and values to add.
	@throws NullPointerException if the given property URI removals, property removals, and/or property additions is <code>null</code>.
	*/
	public DefaultURFResourceAlteration(final URI resourceURI, final Iterable<URI> propertyURIRemovals, final Iterable<URFProperty> propertyRemovals, final Iterable<URFProperty> propertyAdditions)
	{
		this(resourceURI,
				Sets.immutableSetOf(propertyURIRemovals),
				Sets.immutableSetOf(propertyRemovals),
				Sets.immutableSetOf(propertyAdditions));
	}

	/**Resource URI, property URI removals, property removals, and property additions constructor.
	This constructor saves the given collections as-is, so only private unmodifiable sets should be provided
	@param resourceURI The new URI of the resource, or <code>null</code> if the current URI of the resource, if any, should remain unchanged.
	@param propertyURIRemovals The URIs of properties to remove.
	@param propertyRemovals The properties and values to remove.
	@param propertyAdditions The properties and values to add.
	@throws NullPointerException if the given property URI removals, property removals, and/or property additions is <code>null</code>.
	*/
	protected DefaultURFResourceAlteration(final URI resourceURI, final Set<URI> propertyURIRemovals, final Set<URFProperty> propertyRemovals, final Set<URFProperty> propertyAdditions)
	{
		this.resourceURI=resourceURI;
		this.propertyURIRemovals=checkInstance(propertyURIRemovals, "Property URI removals cannot be null");
		this.propertyRemovals=checkInstance(propertyRemovals, "Property removals cannot be null");
		this.propertyAdditions=checkInstance(propertyAdditions, "Property additions cannot be null");
	}

	/**Creates an alteration to set properties to match the given resource description.
	The new resource alteration will alter a resource with no properties or a resource with the same property URIs
	to have the same properties as the given resource.
	This is achieved by an alteration to set the given properties by removing all properties with the URIs of the properties to set
	and then adding the set properties.
	The resource alteration will not remove any property URIs not present in the given resource description.
	No resource URI change is indicated.
	@param resource The resource indicating the properties to set.
	@return A resource alteration indicating that the properties should be set to match the given resource description.
	@throws NullPointerException if the given resource is <code>null</code>.
	*/
	public static DefaultURFResourceAlteration createResourceAlteration(final URFResource resource)
	{
		final Set<URI> propertyURIRemovals=new HashSet<URI>();	//the URIs of the properties to remove
		final Set<URFProperty> propertyAdditions=new HashSet<URFProperty>();	//the properties to add
		for(final URFProperty property:resource.getProperties())	//for each property
		{
			propertyURIRemovals.add(property.getPropertyURI());	//indicate that we should remove all values of this property
			propertyAdditions.add(property);	//indicate that we should add this property
		}
		return new DefaultURFResourceAlteration(null, unmodifiableSet(propertyURIRemovals), Collections.<URFProperty>emptySet(), unmodifiableSet(propertyAdditions));
	}

	/**Creates an alteration to add the given properties.
	No resource URI change is indicated.
	@param properties The properties to set.
	@return A resource alteration indicating that the given properties should be added.
	@throws NullPointerException if the given properties is <code>null</code>.
	*/
	public static DefaultURFResourceAlteration createAddPropertiesAlteration(final URFProperty... properties)
	{
		return createAddPropertiesAlteration(asList(properties));
	}

	/**Creates an alteration to add the given properties.
	No resource URI change is indicated.
	@param properties The properties to set.
	@return A resource alteration indicating that the given properties should be added.
	@throws NullPointerException if the given properties is <code>null</code>.
	*/
	public static DefaultURFResourceAlteration createAddPropertiesAlteration(final Iterable<URFProperty> properties)
	{
		return new DefaultURFResourceAlteration(null, Collections.<URI>emptySet(), Collections.<URFProperty>emptySet(), Sets.immutableSetOf(properties));
	}

	/**Creates an alteration to set the given properties by removing all properties with the URIs of the properties to set
	and then adding the set properties.
	No resource URI change is indicated.
	@param properties The properties to set.
	@return A resource alteration indicating that the given properties should be set.
	@throws NullPointerException if the given properties is <code>null</code>.
	*/
	public static DefaultURFResourceAlteration createSetPropertiesAlteration(final URFProperty... properties)
	{
		return createSetPropertiesAlteration(asList(properties));
	}

	/**Creates an alteration to set the given properties by removing all properties with the URIs of the properties to set
	and then adding the set properties.
	No resource URI change is indicated.
	@param properties The properties to set.
	@return A resource alteration indicating that the given properties should be set.
	@throws NullPointerException if the given properties is <code>null</code>.
	*/
	public static DefaultURFResourceAlteration createSetPropertiesAlteration(final Iterable<URFProperty> properties)
	{
		final Set<URI> propertyURIRemovals=new HashSet<URI>();	//the URIs of the properties to remove
		final Set<URFProperty> propertyAdditions=new HashSet<URFProperty>();	//the properties to add
		for(final URFProperty property:properties)	//for each property
		{
			propertyURIRemovals.add(property.getPropertyURI());	//indicate that we should remove all values of this property
			propertyAdditions.add(property);	//indicate that we should add this property
		}
		return new DefaultURFResourceAlteration(null, unmodifiableSet(propertyURIRemovals), Collections.<URFProperty>emptySet(), unmodifiableSet(propertyAdditions));
	}

	/**Creates an alteration to remove all values of the properties with the given URIs.
	No resource URI change is indicated.
	@param propertyURIRemovals The URIs of properties to remove.
	@return A resource alteration indicating that the given properties should be removed.
	@throws NullPointerException if the given properties is <code>null</code>.
	*/
	public static DefaultURFResourceAlteration createRemovePropertiesAlteration(final URI... propertyURIRemovals)
	{
		return new DefaultURFResourceAlteration(null, Sets.immutableSetOf(propertyURIRemovals), Collections.<URFProperty>emptySet(), Collections.<URFProperty>emptySet());
	}

	/**Creates an alteration to remove all values of the properties with the given URIs.
	No resource URI change is indicated.
	@param propertyURIRemovals The URIs of properties to remove.
	@return A resource alteration indicating that the given properties should be removed.
	@throws NullPointerException if the given properties is <code>null</code>.
	*/
	public static DefaultURFResourceAlteration createRemovePropertiesAlteration(final Iterable<URI> propertyURIRemovals)
	{
		return new DefaultURFResourceAlteration(null, Sets.immutableSetOf(propertyURIRemovals), Collections.<URFProperty>emptySet(), Collections.<URFProperty>emptySet());
	}

	/**Combines the given alterations with the alterations specified in this object and returns a new specification of the union of alterations.
	The resource URI specified by the given resource alteration, if provided, will override the resource URI specified by these resource alterations, if any.
	@param resourceAlteration The alteration specification to add.
	@return An alteration specification with the combined alterations specified by this specification and the given specification.
	*/
	public URFResourceAlteration add(final URFResourceAlteration resourceAlteration)
	{
		final URI resourceAlterationResourceURI=resourceAlteration.getResourceURI();	//get the specified resource URI, if any
		final Set<URI> propertyURIRemovals=new HashSet<URI>(getPropertyURIRemovals());	//get the URIs of the properties to remove
		propertyURIRemovals.addAll(resourceAlteration.getPropertyURIRemovals());	//update the URIs of the properties to remove
		final Set<URFProperty> propertyRemovals=new HashSet<URFProperty>(getPropertyRemovals());	//get the properties to remove
		propertyRemovals.addAll(resourceAlteration.getPropertyRemovals());	//update the properties to remove
		final Set<URFProperty> propertyAdditions=new HashSet<URFProperty>(getPropertyAdditions());	//get the properties to add
		propertyAdditions.addAll(resourceAlteration.getPropertyAdditions());	//update the properties to add
		return new DefaultURFResourceAlteration(resourceAlterationResourceURI!=null ? resourceAlterationResourceURI : getResourceURI(), propertyURIRemovals, propertyRemovals, propertyAdditions);	//return a new resource alteration, overriding the specified resource URI if requested
	}

}
