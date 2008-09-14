package com.globalmentor.urf;

import java.text.Collator;

import com.globalmentor.text.*;
import com.globalmentor.util.*;

/**Sorts resources based upon the determined resource string value used for representation.
This comparator does not sort unambiguously, and should be used as a delegate comparator of a {@link SerialDelegateComparator}.
<p>Copyright © 2008 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
@see URFResource#determineLabel()
@see SerialDelegateComparator
*/
public class ResourceDeterminedLabelComparator extends AbstractCollatedSortOrderComparator<URFResource>
{

	/**Collator factory constructor sorting in ascending order.
	@param collatorFactory The source of collators.
	@throws NullPointerException if the given collator factory is <code>null</code>.
	*/
	public ResourceDeterminedLabelComparator(final CollatorFactory collatorFactory)
	{
		this(collatorFactory, SortOrder.ASCENDING);	//construct the class using ascending order
	}

	/**Collator factory and sort order constructor.
	@param collatorFactory The source of collators.
	@param sortOrder The order in which to perform comparisons.
	@throws NullPointerException if the given collator factory and/or sort order is <code>null</code>.
	*/
	public ResourceDeterminedLabelComparator(final CollatorFactory collatorFactory, final SortOrder sortOrder)
	{
		super(collatorFactory, sortOrder);	//construct the parent class
	}

	/**Compares two resources for order.
	Returns a negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second.
	Identical resources are always considered equal.
	<p>This version compares resources based upon their determined label.
	Resources without a name are considered less than those with a name property.</p>
	@param resource1 The first resource to be compared.
	@param resource2 The second resource to be compared.
	@return A negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second.
	@throws ClassCastException if the arguments' types prevent them from being compared by this comparator.
	@see URFResource#determineLabel()
	*/
	public int compare(final URFResource resource1, final URFResource resource2)
	{
		if(resource1==resource2)	//if the resources are identical
		{
			return 0;	//identical resources are always equal
		}
		final String label1=resource1.determineLabel();		
		final String label2=resource2.determineLabel();		
		final SortOrder sortOrder=getSortOrder();	//get the sort order
		final Collator collator=getCollatorInstance();	//get the collator
		return sortOrder==SortOrder.ASCENDING ? collator.compare(label1, label2) : collator.compare(label2, label1);	//compare in the requested order
	}

}
