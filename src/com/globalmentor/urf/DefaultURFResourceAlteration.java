package com.globalmentor.urf;

import java.net.URI;
import java.util.*;

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
<p>Copyright © 2008 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
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
		this.resourceURI=null;
		this.propertyURIRemovals=emptySet();
		this.propertyRemovals=emptySet();
		this.propertyAdditions=emptySet();
	}

	/**Property URI removals, property removals, and property additions constructor.
	No resource URI change is indicated.
	@param propertyURIRemovals The URIs of properties to remove.
	@param propertyRemovals The properties and values to remove.
	@param propertyAdditions The properties and values to add.
	@throws NullPointerException if the given property URI removals, property removals, and/or property additions is <code>null</code>.
	*/
	public DefaultURFResourceAlteration(final Collection<URI> propertyURIRemovals, final Collection<URFProperty> propertyRemovals, final Collection<URFProperty> propertyAdditions)
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
	public DefaultURFResourceAlteration(final URI resourceURI, final Collection<URI> propertyURIRemovals, final Collection<URFProperty> propertyRemovals, final Collection<URFProperty> propertyAdditions)
	{
		this.resourceURI=resourceURI;
		this.propertyURIRemovals=unmodifiableSet(new HashSet<URI>(checkInstance(propertyURIRemovals, "Property URI removals cannot be null")));
		this.propertyRemovals=unmodifiableSet(new HashSet<URFProperty>(checkInstance(propertyRemovals, "Property removals cannot be null")));
		this.propertyAdditions=unmodifiableSet(new HashSet<URFProperty>(checkInstance(propertyAdditions, "Property additions cannot be null")));
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
