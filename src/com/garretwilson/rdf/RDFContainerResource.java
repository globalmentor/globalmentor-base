package com.garretwilson.rdf;

import java.net.URI;
import java.util.*;
import com.garretwilson.util.*;

/**Represents an RDF resource that is a container, such as a bag or a sequence.
@author Garret Wilson
*/
public abstract class RDFContainerResource extends DefaultRDFResource
{

	/**Constructs an RDF container resource with a reference URI.
	@param newReferenceURI The reference URI for the new resource.
	@exception IllegalArgumentException Thrown if the provided reference URI is
		<code>null</code>.
	*/
	public RDFContainerResource(final URI newReferenceURI) throws IllegalArgumentException
	{
		super(newReferenceURI); //construct the parent class
	}

	/**Convenience constructor that constructs an RDF container resource using a
		namespace URI and local name which will be combined to form the reference
		URI.
	@param newNamespaceURI The XML namespace URI used in the serialization.
	@param newLocalName The XML local name used in the serialization.
	*/
	public RDFContainerResource(final URI newNamespaceURI, final String newLocalName)
	{
		super(newNamespaceURI, newLocalName); //construct the parent class
	}

	/**@return A collection of name/value pairs that represent <code>rdf:li_</code>
		properties and their values, in an undefined order.
	@see NameValuePair
	*/
	protected List getItemProperties()
	{
		  //create the start of a reference URI from the rdf:li element qualified
			//  name (i.e. "rdfNamespaceURI#li_"), which we'll use to check for items
		final String RDF_LI_REFERENCE_PREFIX=RDFUtilities.createReferenceURI(RDF_NAMESPACE_URI, CONTAINER_MEMBER_PREFIX).toString();
		final List itemPropertyList=new ArrayList();  //create a list in which to store the items
		final Iterator propertyIterator=getPropertyIterator();  //get an iterator to all properties
		while(propertyIterator.hasNext()) //while there are more properties
		{
			final NameValuePair nameValuePair=(NameValuePair)propertyIterator.next(); //get the next name/value pair
				//if this property name begins with rdf:_
		  if(((Resource)nameValuePair.getName()).getReferenceURI().toString().startsWith(RDF_LI_REFERENCE_PREFIX))
				itemPropertyList.add(nameValuePair);  //add this name and value to the list
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
		return Collections.unmodifiableCollection(getItemList(itemPropertyList));  //return the items values as an unmodifiable collection
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

	/**Retrieves the item resource with the given reference URI.
	@param referenceURI The reference URI of the item resource to retrieve.
	@return The first encountered item resource with the given reference URI, or
		<code>null</code> if this container holds no such resource.
	*/
	public RDFResource getItem(final URI referenceURI)
	{
		final Iterator itemIterator=getItemIterator();  //get an iterator to look through the items
		while(itemIterator.hasNext()) //while there are more items
		{
		  final RDFResource item=(RDFResource)itemIterator.next();  //get the next item
			if(item.getReferenceURI().equals(referenceURI)) //if this item has the correct reference URI
				return item;  //return the item we found
		}
		return null;  //show that we couldn't find the given item resource
	}

	/**Creates a list of item property values from the name/value pairs stored
		in the given list. The list will maintain the order of the item values.
	@param itemPropertyList A list of name/value pairs, with the name holding
		the property resource and the value holding the property value.
	@return A list of values in the same order as the name/value pairs.
	@see NameValuePair
	*/
	protected List getItemList(final List itemPropertyList)
	{
//G***del Debug.traceStack(); //G***del
		final List itemList=new ArrayList(itemPropertyList.size());  //create a list in which to store the actual values, making it the same length as the property list
		final Iterator itemPropertyIterator=itemPropertyList.iterator();  //get an iterator to all the item properties
		while(itemPropertyIterator.hasNext()) //while there are more item properties
		{
			final NameValuePair nameValuePair=(NameValuePair)itemPropertyIterator.next(); //get the next name/value pair
Debug.trace("getting sorted value for: ", nameValuePair.getName()); //G***del
Debug.trace("sorted value is: ", nameValuePair.getValue()); //G***del
		  itemList.add(nameValuePair.getValue()); //store the value (which should be an RDFObject) in the list
		}
		return itemList;  //return the list list of items
	}

	/**@return The next number available for adding an item property, one more
		than the highest number present.*/
	protected int getNextItemNumber()
	{
		int highestNumber=0; //we haven't found a highest number, yet
		  //create the start of a reference URI from the rdf:li element qualified
			//  name (i.e. "rdfNamespaceURI#li_"), which we'll use to check for items
		final String RDF_LI_REFERENCE_PREFIX=RDFUtilities.createReferenceURI(RDF_NAMESPACE_URI, CONTAINER_MEMBER_PREFIX).toString();
		final Iterator propertyIterator=getPropertyIterator();  //get an iterator to all properties
		while(propertyIterator.hasNext()) //while there are more properties
		{
			final NameValuePair nameValuePair=(NameValuePair)propertyIterator.next(); //get the next name/value pair
			final URI propertyReferenceURI=((Resource)nameValuePair.getName()).getReferenceURI();  //get the reference URI of the property
		  if(propertyReferenceURI.toString().startsWith(RDF_LI_REFERENCE_PREFIX))  //if this property name begins with rdf:_	//G***fix better
			{
					//get the current number by removing the start of the URI up to and including "#_"
				final String numberString=propertyReferenceURI.toString().substring(RDF_LI_REFERENCE_PREFIX.length());	//G***fix better
				try
				{
				  final int number=Integer.parseInt(numberString);  //parse the integer from the string
					highestNumber=Math.max(highestNumber, number); //update the highest number if this number is higher G***is it faster to use this method, or to check before assigning
				}
				catch(NumberFormatException numberFormatException) {}  //if the string does not contain a valid number, ignore this property
			}
		}
		return highestNumber+1;  //return one higher than the highest number we found
	}

	/**Changes all numbers starting with the given number of all members.
	@param rdf The data model to use as a resource factory. G***should we eventually just allow a ResourceLocator interface parameter, or put this method in RDFUtilities?
	@param minNumber The number that represents the lowest number to be changed.
	@param delta The amount each number should be changed by.
	*/
	protected void changeNumbers(final RDF rdf, final int minNumber, final int delta)
	{
		  //create the start of a reference URI from the rdf:li element qualified
			//  name (i.e. "rdfNamespaceURI#li_"), which we'll use to check for items
		final String RDF_LI_REFERENCE_PREFIX=RDFUtilities.createReferenceURI(RDF_NAMESPACE_URI, CONTAINER_MEMBER_PREFIX).toString();
		final ListIterator propertyIterator=getPropertyIterator();  //get an iterator to all properties
		while(propertyIterator.hasNext()) //while there are more properties
		{
			final NameValuePair nameValuePair=(NameValuePair)propertyIterator.next(); //get the next name/value pair
			final URI propertyReferenceURI=((Resource)nameValuePair.getName()).getReferenceURI();  //get the reference URI of the property
		  if(propertyReferenceURI.toString().startsWith(RDF_LI_REFERENCE_PREFIX))  //if this property name begins with rdf:_ G***fix better
			{
					//get the current number by removing the start of the URI up to and including "#_"
				final String numberString=propertyReferenceURI.toString().substring(RDF_LI_REFERENCE_PREFIX.length());
				try
				{
				  final int number=Integer.parseInt(numberString);  //parse the integer from the string
					if(number>=minNumber) //if this number is within our range
					{
						  //create a new property pair with a new number in the property
						final NameValuePair newProperty=new NameValuePair(getMemberProperty(rdf, number+delta), nameValuePair.getValue());
						propertyIterator.set(newProperty);  //change this property
					}
				}
				catch(NumberFormatException numberFormatException) {}  //if the string does not contain a valid number, ignore this property
			}
		}
	}

	/**Adds an item to the container as an <code>&lt;rdf:li</code> property, using
		the next available item number.
	@param rdf The data model to use as a resource factory. G***should we eventually just allow a ResourceLocator interface parameter, or put this method in RDFUtilities?
	@param propertyValue The item to add to the container.
	*/
	public void add(final RDF rdf, final RDFObject propertyValue) //G***should we replace all this with "member" instead of "item"?
	{
		add(rdf, propertyValue, getNextItemNumber()); //store the item at the next available number
	}

	/**Adds an item to the container as an <code>&lt;rdf:li</code> property, using
		the given item number.
	@param rdf The data model to use as a resource factory. G***should we eventually just allow a ResourceLocator interface parameter, or put this method in RDFUtilities?
	@param propertyValue The item to add to the container.
	@param number The number to give to the item.
	*/
	public void add(final RDF rdf, final RDFObject propertyValue, final int number) //G***should we replace all this with "member" instead of "item"?
	{
		changeNumbers(rdf, number, 1); //increment by one any members that have this number or higher
		addProperty(getMemberProperty(rdf, number), propertyValue); //add the property and value to the resource
	}

	/**Determines the property to use for a member with the given number.
	@param rdf The data model to use as a resource factory. G***should we eventually just allow a ResourceLocator interface parameter, or put this method in RDFUtilities?
	@param number The number of a member.
	@return The URI of the property representing this numbered member.
	*/
	protected static RDFResource getMemberProperty(final RDF rdf, final int number)
	{
		final String propertyLocalName=CONTAINER_MEMBER_PREFIX+number;  //create a local name for the number
		return rdf.locateResource(RDF_NAMESPACE_URI, propertyLocalName); //use the URI containing the number as the property
	}

}