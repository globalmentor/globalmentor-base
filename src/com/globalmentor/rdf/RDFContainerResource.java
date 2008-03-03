/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.rdf;

import java.net.URI;
import java.util.*;
import static java.util.Collections.*;

import static com.globalmentor.rdf.RDF.*;
import static com.globalmentor.rdf.RDFUtilities.*;

/**Represents an RDF resource that is a container, such as a bag or a sequence.
@author Garret Wilson
*/
public abstract class RDFContainerResource extends TypedRDFResource
{

	/**Constructs an RDF container resource with a reference URI.
	@param newReferenceURI The reference URI for the new resource.
	*/
	public RDFContainerResource(final URI newReferenceURI)
	{
		super(newReferenceURI); //construct the parent class
	}

	/**Data model and reference URI constructor.
	@param rdf The data model associated with the container.
	@param newReferenceURI The reference URI for the new resource.
	*/
	RDFContainerResource(final RDF rdf, final URI newReferenceURI)
	{
		super(rdf, newReferenceURI); //construct the parent class
	}

	/**@return A collection of name/value pairs that represent <code>rdf:li_</code>
		properties and their values, in an undefined order.
	@see RDFPropertyValuePair
	*/
	protected List getItemProperties()
	{
		final List itemPropertyList=new ArrayList();  //create a list in which to store the items
		final Iterator propertyIterator=getPropertyIterator();  //get an iterator to all properties
		while(propertyIterator.hasNext()) //while there are more properties
		{
			final RDFPropertyValuePair propertyValuePair=(RDFPropertyValuePair)propertyIterator.next(); //get the next name/value pair
				//if this property reference URI begins with rdf:_
		  if(isContainerMemberPropertyReference(propertyValuePair.getProperty().getURI()))
				itemPropertyList.add(propertyValuePair);  //add this property and value to the list
		}
		return itemPropertyList;  //return the list of name/value pairs
	}

	/**@return A read-only collection of the items (specified by
		<code>rdf:li_</code> properties), in an order determined by this type of
		container. This version returns an unordered collection, and should be
		overridden in a child class if order is important.
	*/
	public Collection getItemCollection()
	{
		final List itemPropertyList=getItemProperties();  //get the <li> item properties
		return unmodifiableCollection(getItemList(itemPropertyList));  //return the items values as an unmodifiable collection
	}

	/**@return A read-only iterator to the items (specified by
		<code>rdf:li_</code> properties), in an order determined by this type of
		container.
	@see #getItemCollection
	*/
	public Iterator getItemIterator()
	{
		return getItemCollection().iterator();  //return an iterator to the collection
	}

	/**Creates a list of item property values from the name/value pairs stored
		in the given list. The list will maintain the order of the item values.
	@param itemPropertyList A list of name/value pairs, with the name holding
		the property resource and the value holding the property value.
	@return A list of values in the same order as the name/value pairs.
	@see PropertyValuePair
	*/
	protected List getItemList(final List itemPropertyList)
	{
		final List itemList=new ArrayList(itemPropertyList.size());  //create a list in which to store the actual values, making it the same length as the property list
		final Iterator itemPropertyIterator=itemPropertyList.iterator();  //get an iterator to all the item properties
		while(itemPropertyIterator.hasNext()) //while there are more item properties
		{
			final RDFPropertyValuePair propertyValuePair=(RDFPropertyValuePair)itemPropertyIterator.next(); //get the next name/value pair
		  itemList.add(propertyValuePair.getPropertyValue()); //store the value (which should be an RDFObject) in the list
		}
		return itemList;  //return the list list of items
	}

	/**@return The next number available for adding an item property, one more
		than the highest number present.*/
	protected int getNextItemNumber()
	{
		int highestNumber=0; //we haven't found a highest number, yet
		final Iterator propertyIterator=getPropertyIterator();  //get an iterator to all properties
		while(propertyIterator.hasNext()) //while there are more properties
		{
			final RDFPropertyValuePair propertyValuePair=(RDFPropertyValuePair)propertyIterator.next(); //get the next name/value pair
			final URI propertyReferenceURI=propertyValuePair.getProperty().getURI();  //get the reference URI of the property
		  if(isContainerMemberPropertyReference(propertyReferenceURI))  //if this property name begins with rdf:_
			{
					//get the current number by removing the start of the URI up to and including "#_"
				final String numberString=propertyReferenceURI.toString().substring(RDF_LI_REFERENCE_PREFIX.length());	//TODO fix better
				try
				{
				  final int number=Integer.parseInt(numberString);  //parse the integer from the string
					highestNumber=Math.max(highestNumber, number); //update the highest number if this number is higher TODO is it faster to use this method, or to check before assigning
				}
				catch(NumberFormatException numberFormatException) {}  //if the string does not contain a valid number, ignore this property
			}
		}
		return highestNumber+1;  //return one higher than the highest number we found
	}

	/**Changes all numbers starting with the given number of all members.
	@param minNumber The number that represents the lowest number to be changed.
	@param delta The amount each number should be changed by.
	*/
	protected void changeNumbers(final int minNumber, final int delta)
	{
		final ListIterator propertyIterator=getPropertyIterator();  //get an iterator to all properties
		while(propertyIterator.hasNext()) //while there are more properties
		{
			final RDFPropertyValuePair propertyValuePair=(RDFPropertyValuePair)propertyIterator.next(); //get the next name/value pair
			final URI propertyReferenceURI=propertyValuePair.getProperty().getURI();  //get the reference URI of the property
			if(isContainerMemberPropertyReference(propertyReferenceURI))  //if this property name begins with rdf:_
			{
					//get the current number by removing the start of the URI up to and including "#_"
				final String numberString=propertyReferenceURI.toString().substring(RDF_LI_REFERENCE_PREFIX.length());
				try
				{
				  final int number=Integer.parseInt(numberString);  //parse the integer from the string
					if(number>=minNumber) //if this number is within our range
					{
						  //create a new property pair with a new number in the property
						final RDFPropertyValuePair newProperty=new RDFPropertyValuePair(locateResource(this, getMemberPropertyURI(number+delta)), propertyValuePair.getPropertyValue());
						propertyIterator.set(newProperty);  //change this property
					}
				}
				catch(NumberFormatException numberFormatException) {}  //if the string does not contain a valid number, ignore this property
			}
		}
	}

	/**Adds an item to the container as an <code>&lt;rdf:li</code> property, using the next available item number.
	@param propertyValue The item to add to the container.
	*/
	public <T extends RDFObject> T add(final T propertyValue) //TODO should we replace all this with "member" instead of "item"?
	{
		return add(propertyValue, getNextItemNumber()); //store the item at the next available number
	}

	/**Adds an item to the container as an <code>&lt;rdf:li</code> property, using the given item number.
	@param propertyValue The item to add to the container.
	@param number The number to give to the item.
	*/
	public <T extends RDFObject> T add(final T propertyValue, final int number)
	{
		changeNumbers(number, 1); //increment by one any members that have this number or higher
		return addProperty(getMemberPropertyURI(number), propertyValue); //add the property and value to the resource
	}

	/**Determines the property URI to use for a member with the given number.
	@param number The number of a member.
	@return The URI of the property representing this numbered member.
	*/
	protected URI getMemberPropertyURI(final int number)
	{
		final String propertyLocalName=CONTAINER_MEMBER_PREFIX+number;  //create a local name for the number
		return createReferenceURI(RDF_NAMESPACE_URI, propertyLocalName); //use the URI containing the number as the property
	}

}