package com.globalmentor.urf;

import com.globalmentor.java.Longs;
import com.globalmentor.util.AbstractChainedSortOrderComparator;
import com.globalmentor.util.AbstractSortOrderComparator;
import com.globalmentor.util.SortOrder;

/**An abstract comparator to sort resources based upon creation order.
Normally sorting based upon resource creation order alone is not useful.
This class is useful to provide a fallback mechanism for sorting on creation order to guarantee uniqueness distinction,
either as a superclass or as a subordinate chained comparator. 
<p>Copyright © 2008 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
@see URFScope#getCreationOrder()
@see AbstractChainedSortOrderComparator
*/
public class ResourceCreationOrderComparator extends AbstractSortOrderComparator<URFResource>
{

	/**The lazily-created ascending singleton instance of the comparator.*/
	private static ResourceCreationOrderComparator ascendingInstance=null;

	/**The lazily-created descending singleton instance of the comparator.*/
	private static ResourceCreationOrderComparator descendingInstance=null;

	/**Retrieves a singleton instance of the comparator with the correct sort order.
	@param sortOrder The order in which to perform comparisons.
	@return The lazily-created singleton instance of the comparator with the given sort order.
	@throws NullPointerException if the given sort order is <code>null</code>.
	*/
	public static ResourceCreationOrderComparator getInstance(final SortOrder sortOrder)
	{
		switch(sortOrder)	//see which sort order is requested
		{
			case ASCENDING:
				if(ascendingInstance==null)	//if there is not yet a singleton instance (the race condition here is benign)
				{
					ascendingInstance=new ResourceCreationOrderComparator(sortOrder);	//create a new instance
				}
				return ascendingInstance;	//return the singleton instance
			case DESCENDING:
				if(descendingInstance==null)	//if there is not yet a singleton instance (the race condition here is benign)
				{
					descendingInstance=new ResourceCreationOrderComparator(sortOrder);	//create a new instance
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
	protected ResourceCreationOrderComparator(final SortOrder sortOrder)
	{
		super(sortOrder);	//construct the parent class
	}

	/**Compares two resources for order.
	Returns a negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second.
	Identical resources are always considered equal.
	<p>This version compares resources based upon their creation order.</p>
	<p>Usually a subclass will call this version if sufficient comparison information is unavailable.</p>  
	@param resource1 The first resource to be compared.
	@param resource2 The second resource to be compared.
	@return A negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second.
	@throws ClassCastException if the arguments' types prevent them from being compared by this comparator.
	@see URFScope#getCreationOrder()
	*/
	public int compare(final URFResource resource1, final URFResource resource2)
	{
		if(resource1==resource2)	//if the resources are identical
		{
			return 0;	//identical resources are always equal
		}
		return getSortOrder()==SortOrder.ASCENDING ? Longs.compare(resource1.getCreationOrder(), resource2.getCreationOrder()) : Longs.compare(resource2.getCreationOrder(), resource1.getCreationOrder());	//compare in the requested order 
					
	}

}
