package com.globalmentor.urf.dcmi;

import com.globalmentor.urf.*;
import static com.globalmentor.urf.dcmi.DCMI.*;
import com.globalmentor.util.*;

/**Sorts resources based upon DCMI date.
This comparator does not sort unambiguously, and should be used as a delegate comparator of a {@link SerialDelegateComparator}.
<p>Copyright © 2008 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
@see DCMI#DATE_PROPERTY_URI
@see SerialDelegateComparator
*/
public class ResourceDCMIDateComparator extends AbstractSortOrderComparator<URFResource>
{

	/**The lazily-created ascending singleton instance of the comparator.*/
	private static ResourceDCMIDateComparator ascendingInstance=null;

	/**The lazily-created descending singleton instance of the comparator.*/
	private static ResourceDCMIDateComparator descendingInstance=null;

	/**Retrieves a singleton instance of the comparator with ascending order.
	@return The lazily-created singleton instance of the comparator ascending order.
	*/
	public static ResourceDCMIDateComparator getInstance()
	{
		return getInstance(SortOrder.ASCENDING);	//get the ascending singleton instance
	}

	/**Retrieves a singleton instance of the comparator with the correct sort order.
	@param sortOrder The order in which to perform comparisons.
	@return The lazily-created singleton instance of the comparator with the given sort order.
	@throws NullPointerException if the given sort order is <code>null</code>.
	*/
	public static ResourceDCMIDateComparator getInstance(final SortOrder sortOrder)
	{
		switch(sortOrder)	//see which sort order is requested
		{
			case ASCENDING:
				if(ascendingInstance==null)	//if there is not yet a singleton instance (the race condition here is benign)
				{
					ascendingInstance=new ResourceDCMIDateComparator(sortOrder);	//create a new instance
				}
				return ascendingInstance;	//return the singleton instance
			case DESCENDING:
				if(descendingInstance==null)	//if there is not yet a singleton instance (the race condition here is benign)
				{
					descendingInstance=new ResourceDCMIDateComparator(sortOrder);	//create a new instance
				}
				return descendingInstance;	//return the singleton instance
			default:
				throw new AssertionError("Unrecognized sort order: "+sortOrder);
		}
	}

	/**Sort order constructor.
	@param sortOrder The order in which to perform comparisons.
	@throws NullPointerException if the given sort order is <code>null</code>.
	*/
	protected ResourceDCMIDateComparator(final SortOrder sortOrder)
	{
		super(sortOrder);	//construct the parent class
	}

	/**Compares two resources for order.
	Returns a negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second.
	Identical resources are always considered equal.
	<p>This implementation compares resources based upon DCMI date.
	Resources without a date are considered less than those with a date property.
	</p>
	@param resource1 The first resource to be compared.
	@param resource2 The second resource to be compared.
	@return A negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second.
	@throws ClassCastException if the arguments' types prevent them from being compared by this comparator.
	@see DCMI#DATE_PROPERTY_URI
	*/
	public int compare(final URFResource resource1, final URFResource resource2)
	{
		if(resource1==resource2)	//if the resources are identical
		{
			return 0;	//identical resources are always equal
		}
		final SortOrder sortOrder=getSortOrder();	//get the sorting order
		final AbstractURFDateTime resource1Date=getDate(resource1);	//get the date of the first resource
		final AbstractURFDateTime resource2Date=getDate(resource2);	//get the date of the second resource
		if(resource1Date!=null)	//if the first resource has a date designation
		{
			if(resource2Date!=null)	//if the second resource has a date designation
			{
				return sortOrder==SortOrder.ASCENDING ? resource1Date.compareTo(resource2Date) : resource2Date.compareTo(resource1Date);	//compare in the requested order
			}
			else	//if only the first resource has a date designation
			{
				return sortOrder==SortOrder.ASCENDING ? 1 : -1;	//missing date values should be sorted lower
			}
		}
		else	//if the first resource has no date designation
		{
			if(resource2Date!=null)	//if the second resource has a date designation
			{
				return sortOrder==SortOrder.ASCENDING ? -1 : 1;	//missing date values should be sorted lower
			}
			else	//if neither resource has a date designation
			{
				return 0;	//consider the resources equal so that other comparators may resolve the order
			}
		}
	}

}
