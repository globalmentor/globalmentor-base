package com.globalmentor.urf.content;

import java.util.Comparator;

import com.globalmentor.urf.*;
import com.globalmentor.util.AbstractChainedSortOrderComparator;
import com.globalmentor.util.SortOrder;
import static com.globalmentor.urf.content.Content.*;

/**Sorts resources based upon URF content modified time and date.
The singleton instances sort secondarily by resource creation order.
<p>Copyright © 2008 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
@see Content#MODIFIED_PROPERTY_URI
@see ResourceCreationOrderComparator
*/
public class ResourceContentModifiedComparator extends AbstractChainedSortOrderComparator<URFResource>
{

	/**The lazily-created ascending singleton instance of the comparator.*/
	private static ResourceContentModifiedComparator ascendingInstance=null;

	/**The lazily-created descending singleton instance of the comparator.*/
	private static ResourceContentModifiedComparator descendingInstance=null;

	/**Retrieves a singleton instance of the comparator with the correct sort order.
	Subordinate sorting is performed by creation order.
	@param sortOrder The order in which to perform comparisons.
	@return The lazily-created singleton instance of the comparator with the given sort order.
	@throws NullPointerException if the given sort order is <code>null</code>.
	*/
	public static ResourceContentModifiedComparator getInstance(final SortOrder sortOrder)
	{
		switch(sortOrder)	//see which sort order is requested
		{
			case ASCENDING:
				if(ascendingInstance==null)	//if there is not yet a singleton instance (the race condition here is benign)
				{
					ascendingInstance=new ResourceContentModifiedComparator(sortOrder);	//create a new instance
				}
				return ascendingInstance;	//return the singleton instance
			case DESCENDING:
				if(descendingInstance==null)	//if there is not yet a singleton instance (the race condition here is benign)
				{
					descendingInstance=new ResourceContentModifiedComparator(sortOrder);	//create a new instance
				}
				return descendingInstance;	//return the singleton instance
			default:
				throw new AssertionError("Unrecognized sort order: "+sortOrder);
		}
	}

	/**Sort order constructor with a subordinate creation order comparator.
	@param sortOrder The order in which to perform comparisons.
	@throws NullPointerException if the given sort order is <code>null</code>.
	@see ResourceCreationOrderComparator
	*/
	protected ResourceContentModifiedComparator(final SortOrder sortOrder)
	{
		this(sortOrder, ResourceCreationOrderComparator.getInstance(sortOrder));	//construct the class with a subordinate resource creation order comparator
	}

	/**Sort order and subordinate comparator constructor.
	@param sortOrder The order in which to perform comparisons.
	@param subordinateComparator The comparator to perform subordinate sorting.
	@throws NullPointerException if the given subordinate comparator and/or sort order is <code>null</code>.
	*/
	public ResourceContentModifiedComparator(final SortOrder sortOrder, final Comparator<URFResource> subordinateComparator)
	{
		super(sortOrder, subordinateComparator);	//construct the parent class
	}

	/**Compares two resources for order.
	Returns a negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second.
	Identical resources are always considered equal.
	<p>This implementation compares resources based upon URF content modified time and date.
	Resources without a modified property are considered less than those with a modified property.
	If no modified property is available for either resource, the default creation order-based sorting is performed.
	</p>
	@param resource1 The first resource to be compared.
	@param resource2 The second resource to be compared.
	@return A negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second.
	@throws ClassCastException if the arguments' types prevent them from being compared by this comparator.
	@see Content#MODIFIED_PROPERTY_URI
	*/
	public int compare(final URFResource resource1, final URFResource resource2)
	{
		if(resource1==resource2)	//if the resources are identical
		{
			return 0;	//identical resources are always equal
		}
		final SortOrder sortOrder=getSortOrder();	//get the sorting order
		final URFDateTime resource1Modified=getModified(resource1);	//get the modified datetime of the first resource
		final URFDateTime resource2Modified=getModified(resource1);	//get the modified datetime of the first resource
		if(resource1Modified!=null)	//if the first resource has a modified designation
		{
			if(resource2Modified!=null)	//if the second resource has a modified designation
			{
				final int result=sortOrder==SortOrder.ASCENDING ? resource1Modified.compareTo(resource2Modified) : resource2Modified.compareTo(resource1Modified);	//compare in the requested order
				if(result==0 && !resource1.equals(resource2))	//if resources sorted equal but aren't really equal
				{
					return getSubordinateComparator().compare(resource1, resource2);	//perform subordinate comparison
				}
				return result;	//return the ending result
			}
			else	//if only the first resource has a modified designation
			{
				return sortOrder==SortOrder.ASCENDING ? 1 : -1;	//missing modified values should be sorted lower
			}
		}
		else	//if the first resource has no modified designation
		{
			if(resource2Modified!=null)	//if the second resource has a modified designation
			{
				return sortOrder==SortOrder.ASCENDING ? -1 : 1;	//missing modified values should be sorted lower
			}
			else	//if neither resource has a modified designation
			{
				return getSubordinateComparator().compare(resource1, resource2);	//perform subordinate comparison
			}
		}
	}

}
