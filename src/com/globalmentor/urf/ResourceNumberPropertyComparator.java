package com.globalmentor.urf;

import java.net.URI;

import com.globalmentor.collections.comparators.SerialDelegateComparator;
import com.globalmentor.collections.comparators.SortOrder;
import com.globalmentor.java.Numbers;
import static com.globalmentor.urf.URF.*;
import com.globalmentor.util.*;

/**Comparator that sorts resources based upon the numeric value of some identified property.
This comparator does not sort unambiguously, and should be used as a delegate comparator of a {@link SerialDelegateComparator}.
<p>Copyright © 2008 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
@see Numbers#sort(Object, Object, boolean)
@see SerialDelegateComparator
*/
public class ResourceNumberPropertyComparator extends AbstractResourcePropertyComparator
{

	/**Property URI constructor sorting in ascending order.
	@param propertyURI The property URI the value on which comparison will be made.
	@throws NullPointerException if the given property URI is <code>null</code>.
	*/
	public ResourceNumberPropertyComparator(final URI propertyURI)
	{
		this(propertyURI, SortOrder.ASCENDING);	//construct the class using ascending order
	}

	/**Property and sort order constructor.
	@param propertyURI The property URI the value on which comparison will be made.
	@param sortOrder The order in which to perform comparisons.
	@throws NullPointerException if the given property URI and/or sort order is <code>null</code>.
	*/
	protected ResourceNumberPropertyComparator(final URI propertyURI, final SortOrder sortOrder)
	{
		super(propertyURI, sortOrder);	//construct the parent class
	}

	/**Compares two resources for order.
	Returns a negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second.
	Identical resources are always considered equal.
	<p>This version compares resources based upon their the values of their properties specified by {@link #getPropertyURI()},
	but only if each value is a {@link Number}. Resourcs with no such property or with non-number property values are sorted
	before those with number property values.</p>
	@param resource1 The first resource to be compared.
	@param resource2 The second resource to be compared.
	@return A negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second.
	@throws ClassCastException if the arguments' types prevent them from being compared by this comparator.
	@see #getPropertyURI()
	@see Numbers#sort(Object, Object, boolean)
	*/
	public int compare(final URFResource resource1, final URFResource resource2)
	{
		if(resource1==resource2)	//if the resources are identical
		{
			return 0;	//identical resources are always equal
		}
		final URI propertyURI=getPropertyURI();
		final Object propertyValue1=asNumber(resource1.getPropertyValue(propertyURI));	//retrieve the property values, but only if they are numbers
		final Object propertyValue2=asNumber(resource2.getPropertyValue(propertyURI));	//retrieve the property values, but only if they are numbers
		return getSortOrder()==SortOrder.ASCENDING ? Numbers.sort(propertyValue1, propertyValue2, true) : Numbers.sort(propertyValue2, propertyValue1, true);	//compare in the requested order, but do so ambiguously; allow two non-numbers to be considered equivalent 
	}

}
