package com.garretwilson.rdf;

import java.net.URI;
import java.util.*;
import com.garretwilson.util.Debug;
import com.garretwilson.util.NameValuePair;

/**Represents an RDF sequence resource.
@author Garret Wilson
*/
public class RDFSequenceResource extends RDFContainerResource implements Comparator
{

	/**Constructs an RDF sequence resource with a reference URI.
	@param newReferenceURI The reference URI for the new resource.
	@exception IllegalArgumentException Thrown if the provided reference URI is
		<code>null</code>.
	*/
	public RDFSequenceResource(final URI newReferenceURI) throws IllegalArgumentException
	{
		super(newReferenceURI); //construct the parent class
	}

	/**Convenience constructor that constructs an RDF sequence resource using a
		namespace URI and local name which will be combined to form the reference
		URI.
	@param newNamespaceURI The XML namespace URI used in the serialization.
	@param newLocalName The XML local name used in the serialization.
	*/
	public RDFSequenceResource(final URI newNamespaceURI, final String newLocalName)
	{
		super(newNamespaceURI, newLocalName); //construct the parent class
	}

	/**Adds a property by creating a <code>NameValuePair</code> from the given
		property and value. For each property, this resource serves as the subject
		of an RDF statement with the property as the predicate and the value as
		the object.
		<p>Note that the property is not simply a property URI &mdash; it is a
		resource that is identified by the property URI.</p>
		<p>If an equivalent property already exists, no action is taken.</p>
		<p>This version ensures that properties are sorted after being added to
		the resource.</p>
	@param property A property resource; the predicate of an RDF statement.
	@param value A property value; the object of an RDF statement.
	@return The added property value.
	*/
	public RDFObject addProperty(final RDFResource property, final RDFObject value)
	{
		final RDFObject rdfObject=super.addProperty(property, value); //add this property normally
		Collections.sort(propertyList, this); //sort the items by the number contained in the names
		return rdfObject; //return the value we added
	}

	/**@return A read-only collection of the items (specified by
		<code>rdf:li_</code> properties), in the order specified by their
		<code>rdf:li_</code> properties.
	@see #getItemList
	*/
	public Collection getItemCollection()
	{
		return getItemList(); //return the ordered version
	}

	/**@return A read-only list of the items in the container, in the order
		 specified by their <code>rdf:li_</code> properties.
	*/
	public List getItemList()
	{
		final List itemPropertyList=getItemProperties();  //get the <li> item properties
		Collections.sort(itemPropertyList, this); //sort the items by the number contained in the names
		return Collections.unmodifiableList(getItemList(itemPropertyList)); //return the values of the item properties as an unmodifiable list
	}

	/**@return An iterator that allows traversal of all properties, each of which
		is a <code>NameValuePair</code>, with the name being the property predicate
		and the value being the property value. This version ensures that the
		properties are sorted in the correct order.
	*/
/*G***del
	public ListIterator getPropertyIterator()
	{
		final List sortedPropertyList=new ArrayList(propertyList);  //create a new list to sort G***later maybe do this automatically when a new property is added


		return propertyList.listIterator(); //return an iterator to the properties
	}
*/


	/**Compares this object to another object.
		This method determines order based upon first the name, and then the value.
		Both objects must implement <code>Comparable</code> or an exception will be
		thrown.
	@param object The object with which to compare the component. This must be
		another <code>NameValuePair</code> object.
	@return A negative integer, zero, or a positive integer as this name is
		less than, equal to, or greater than the name and value of the specified
		object, respectively.
	@exception ClassCastException Thrown if the specified object's type is not
		a <code>NameValuePair</code>, or if the name and/or the value does not
		implement <code>Comparable</code>.
	@see #getName
	@see #getValue
	*/
/*G***del
	public int compareTo(Object object) throws ClassCastException
	{
		final int result=((Comparable)getName()).compareTo(((NameValuePair)object).getName()); //compare names
			//if the names are equal, compare the values
		return result!=0 ? result : ((Comparable)getValue()).compareTo(((NameValuePair)object).getValue()); //compare values
	}
*/

	/**Compares two <code>NameValuePair</code> objectsfor order. Returns a
		negative integer, zero, or a positive integer as the first argument is less
		than, equal to, or greater than the second.
		<p>The name of the object is interpreted to be an RDF container property
		that contains as its reference URI the order within the container of the
		item.</p>
	@param object1 The first object to be compared, a <code>NameValuePair</code>.
	@param object2 The second object to be compared, a <code>NameValuePair</code>.
	@return A negative integer, zero, or a positive integer as the
		first argument is less than, equal to, or greater than the second.
	@throws ClassCastException If the arguments are not <cod>NameValuePair</code>s.
	*/
	public int compare(final Object object1, final Object object2)  //G***we might put this in RDFContainerResource, even though it isn't used by all containers---or maybe even make an internal class that does this comparation
	{
//G***del		final String RDF_LI_REFERENCE_URI=RDFUtilities.createReferenceURI(RDF_NAMESPACE_URI, ELEMENT_LI);  //create a reference URI from the rdf:li element qualified name G***testing
		  //create the URI string that will appear at the beginning of every rdf:li_XXX property
		  //G***testing; make this constant and static somewhere
		final String RDF_LI_REFERENCE_URI_PREFIX=RDF_NAMESPACE_URI+CONTAINER_MEMBER_PREFIX;
		  //G***testing; make this constant and static somewhere
		final int RDF_LI_REFERENCE_URI_PREFIX_LENGTH=RDF_LI_REFERENCE_URI_PREFIX.length();  //G***del RDF_NAMESPACE_URI.length()+/*G***del ELEMENT_LI.length()+*/CONTAINER_MEMBER_PREFIX.length();
//G***del Debug.trace("length: ", RDF_LI_REFERENCE_URI_PREFIX_LENGTH);  //G***del
//G***del		Debug.trace("comparation for li: ", RDF_LI_REFERENCE_URI+CONTAINER_MEMBER_PREFIX);  //G***del
		  //get the reference URIs of the two properties
		final URI typeURI1=((RDFResource)((NameValuePair)object1).getName()).getReferenceURI();
//G***del Debug.trace("type URI1: ", typeURI1); //G***del
		final URI typeURI2=((RDFResource)((NameValuePair)object2).getName()).getReferenceURI();
//G***del Debug.trace("type URI2: ", typeURI2); //G***del
//G***fix for better checking		if(typeURI1.startsWith(CONTAINER_MEMBER_PREFIX) && typeURI2.startsWith(CONTAINER_MEMBER_PREFIX))  //if

		if(typeURI1.toString().startsWith(RDF_LI_REFERENCE_URI_PREFIX))  //if the first property an rdf:li_XXX	//G***fix better
		{
		  if(typeURI2.toString().startsWith(RDF_LI_REFERENCE_URI_PREFIX))  //if the second property an rdf:li_XXX as well	//G***fix better
			{
					//G***make sure the strings actually start with the correct prefix, and that the numbers are actually numbers
				final int count1=Integer.parseInt(typeURI1.toString().substring(RDF_LI_REFERENCE_URI_PREFIX_LENGTH));  //get the first count by removing everyting before the count
				final int count2=Integer.parseInt(typeURI2.toString().substring(RDF_LI_REFERENCE_URI_PREFIX_LENGTH));  //get the first count by removing everyting before the count
				return count1-count2; //see which number is great
			}
			else  //if only the first is an rdf:li_XXX
			{
				return -1; //put members at the beginning
			}
		}
		else  //if the first property is not an rdf:li_XXX
		{
		  if(typeURI2.toString().startsWith(RDF_LI_REFERENCE_URI_PREFIX))  //if only the second property an rdf:li_XXX	//G***fix better
			{
			  return 1; //put non-members at the end
			}
			else  //if neither property is an rdf:li_XXX
				return typeURI1.compareTo(typeURI2);  //compare the URIs normally
		}


//G***fix this to sort non-li properties as well
/*G***del
			{
				final NameValuePair propertyNameValuePair=processProperty((Element)childNode);  //parse the element representing an RDF property
				final RDFResource property;  //we'll see whether we should convert <rdf:li>
				if(propertyNameValuePair.getName().equals(RDF_LI_REFERENCE_URI))  //if this is a rdf:li property
				{
					++memberCount;  //show that we have another member
					//create a local name in the form "_X"
					final String propertyLocalName=CONTAINER_MEMBER_PREFIX+memberCount;
					property=getRDF().locateResource(RDF_NAMESPACE_URI, propertyLocalName); //use the revised member form as the property
				}

*/
	}

}