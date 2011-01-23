package com.globalmentor.urf;

import java.net.URI;

import static com.globalmentor.java.Objects.*;

import com.globalmentor.collections.comparators.AbstractSortOrderComparator;
import com.globalmentor.collections.comparators.SerialDelegateComparator;
import com.globalmentor.collections.comparators.SortOrder;
import com.globalmentor.util.*;

/**Abstract comparator that sorts resources based upon the value of some identified property.
<p>Copyright © 2008 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
@see SerialDelegateComparator
*/
public abstract class AbstractResourcePropertyComparator extends AbstractSortOrderComparator<URFResource>
{

	/**The property URI the value on which comparison will be made.*/
	private final URI propertyURI;

		/**@return The property URI the value on which comparison will be made.*/
		public URI getPropertyURI() {return propertyURI;}

	/**Property URI and sort order constructor.
	@param propertyURI The property URI the value on which comparison will be made.
	@param sortOrder The order in which to perform comparisons.
	@throws NullPointerException if the given property URI and/or sort order is <code>null</code>.
	*/
	protected AbstractResourcePropertyComparator(final URI propertyURI, final SortOrder sortOrder)
	{
		super(sortOrder);	//construct the parent class
		this.propertyURI=checkInstance(propertyURI, "Property URI cannot be null.");
	}

}
