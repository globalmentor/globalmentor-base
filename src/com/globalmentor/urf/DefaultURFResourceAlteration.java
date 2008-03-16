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

	/**Property URI removals, property removals, and property additions constructor.
	@param propertyURIRemovals The URIs of properties to remove.
	@param propertyRemovals The properties and values to remove.
	@param propertyAdditions The properties and values to add.
	@throws NullPointerException if the given property URI removals, property removals, and/or property additions is <code>null</code>.
	*/
	public DefaultURFResourceAlteration(final Collection<URI> propertyURIRemovals, final Collection<URFProperty> propertyRemovals, final Collection<URFProperty> propertyAdditions)
	{
		this.propertyURIRemovals=unmodifiableSet(new HashSet<URI>(checkInstance(propertyURIRemovals, "Property URI removals cannot be null")));
		this.propertyRemovals=unmodifiableSet(new HashSet<URFProperty>(checkInstance(propertyRemovals, "Property removals cannot be null")));
		this.propertyAdditions=unmodifiableSet(new HashSet<URFProperty>(checkInstance(propertyAdditions, "Property additions cannot be null")));
	}

}
