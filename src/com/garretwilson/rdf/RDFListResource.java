package com.garretwilson.rdf;

import java.net.URI;
import java.util.*;

/**Represents an RDF list resource.
@author Garret Wilson
*/
public class RDFListResource extends DefaultRDFResource	//G***fix implements List//G***del, Comparator
{

	/**Constructs an RDF list resource with a reference URI.
	@param newReferenceURI The reference URI for the new resource.
	*/
	public RDFListResource(final URI newReferenceURI)
	{
		super(newReferenceURI); //construct the parent class
	}

	/**Convenience constructor that constructs an RDF sequence resource using a
		namespace URI and local name which will be combined to form the reference
		URI.
	@param newNamespaceURI The XML namespace URI used in the serialization.
	@param newLocalName The XML local name used in the serialization.
	*/
	public RDFListResource(final URI newNamespaceURI, final String newLocalName)
	{
		super(newNamespaceURI, newLocalName); //construct the parent class
	}

	/**Constructs an anonymous RDF list resource with a single element.
		The rest of the list is set to the RDF nil resource.
	@param rdf The RDF data model.
	@param first The first and only element of the list.
	*/
	public RDFListResource(final RDF rdf, final RDFResource first)
	{
//G***del		final RDFResource nil=rdf.locateResource(RDF_NAMESPACE_URI, NIL_RESOURCE_LOCAL_NAME);	//get the nil resource
		this(rdf, (URI)null, first);	//create a list with no other elements
	}

	/**Constructs an RDF list resource with a single element.
		The rest of the list is set to the RDF nil resource.
	@param rdf The RDF data model.
	@param newReferenceURI The reference URI for the new resource.
	@param first The first and only element of the list.
	*/
	public RDFListResource(final RDF rdf, final URI newReferenceURI, final RDFResource first)
	{
//G***del		final RDFResource nil=rdf.locateResource(RDF_NAMESPACE_URI, NIL_RESOURCE_LOCAL_NAME);	//get the nil resource
		this(rdf, newReferenceURI, first, RDFUtilities.locateNilResource(rdf));	//create a list with no other elements
	}

	/**Constructs an anonymous RDF list resource with a current element and the rest of the list.
	@param rdf The RDF data model.
	@param first The first element of the list.
	@param rest The list resource representing the rest of the list, or <code>null</code>
		if no rest should be specified.
	*/
	public RDFListResource(final RDF rdf, final RDFResource first, final RDFResource rest)
	{
		this(rdf, (URI)null, first, rest); //construct a list with an anonymous reference URI
	}

	/**Constructs an RDF list resource with a current element and the rest of the list.
	@param rdf The RDF data model.
	@param newReferenceURI The reference URI for the new resource.
	@param first The first element of the list.
	@param rest The list resource representing the rest of the list, or <code>null</code>
		if no rest should be specified.
	*/
	public RDFListResource(final RDF rdf, final URI newReferenceURI, final RDFResource first, final RDFResource rest)
	{
		super(newReferenceURI); //construct the list with the reference URI
		setFirst(rdf, this, first);	//set the first element
		if(rest!=null)	//if a rest is specified
			setRest(rdf, this, rest);	//set the rest of the list
	}

	/**Constructs an anonymous RDF list resource with the contents of a collection.
	@param rdf The RDF data model.
	@param collection The collection with which to populate the list.
	*/
	public static RDFListResource create(final RDF rdf, final Collection collection)
	{
		return create(rdf, (URI)null, collection);	//create and return a list with an anonymous reference URI
	}

	/**Constructs an RDF list resource with the contents of a collection.
	@param rdf The RDF data model.
	@param collection The collection with which to populate the list, each
		element of which is a <code>RDFResource</code>.
	*/
	public static RDFListResource create(final RDF rdf, final URI newReferenceURI, final Collection collection)
	{
//G***del		final RDFResource nil=rdf.locateResource(RDF_NAMESPACE_URI, NIL_RESOURCE_LOCAL_NAME);	//get the nil resource
		final RDFListResource listResource;	//we'll create a list resource for the collection 
		if(collection.size()>0)	//if there are elements in the collection
		{
			final Iterator resourceIterator=collection.iterator();	//get an iterator to the elements of the collection
				//G***maybe throw an illegal argument exception if the elements in the collection are not resources
			listResource=new RDFListResource(rdf, (RDFResource)resourceIterator.next());	//get the first resource in the collection
			RDFListResource list=listResource;	//we'll keep adding to the list as we go along
			while(resourceIterator.hasNext())	//while there are more elements in the collection
			{
				
				final RDFListResource nextList=new RDFListResource(rdf, (RDFResource)resourceIterator.next());	//get the next resource in the collection
				setRest(rdf, list, nextList);	//add the next list to this one
				list=nextList;	//we'll use the next list the next time around 
			}
		}
		else	//if the collection is empty
		{
			listResource=RDFUtilities.locateNilResource(rdf);	//use the nil list
		}
		return listResource;	//return whichever list resource we created or used
	}

	/**Retrieves the first element of the list. If this resource has more than one
		property of <code>rdf:first</code>, it is undefined which of those property
		values will be returned.
	@param resource The list resource for which the first element will be returned.
	@return The first element of the list, or <code>null</code> if the resource
		has no first element specified.
	@exception ClassCastException Thrown if the first element property value is
		not a <code>RDFResource</code> (such as a <code>Literal</code>), which would
		indicate that an incorrect value has been stored for the property.
	*/
	public static RDFResource getFirst(final RDFResource resource) throws ClassCastException
	{
		return (RDFResource)resource.getPropertyValue(RDF_NAMESPACE_URI, FIRST_PROPERTY_NAME); //return the first element property
	}

	/**Retrieves the rest of the list, which is usually another list.
		If this resource has more than one property of <code>rdf:rest</code>,
		it is undefined which of those property values will be returned.
	@param resource The list resource for which the rest property will be returned.
	@return The resource representing the rest of the list, or <code>null</code> if
		the resource has no rest of the list specified.
	@exception ClassCastException Thrown if the rest property value is
		not a <code>RDFResource</code> (such as a <code>Literal</code>), which would
		indicate that an incorrect value has been stored for the property.
	*/
	public static RDFResource getRest(final RDFResource resource) throws ClassCastException
	{
		return (RDFResource)resource.getPropertyValue(RDF_NAMESPACE_URI, REST_PROPERTY_NAME); //return the rest property
	}

	/**Replaces all <code>rdf:first</code> properties of the resource with a new
		property with the given value.
	@param rdf The RDF data model.
	@param resource The resource for which the first property should be set.
	@param value The first element of the list.
	*/
	public static void setFirst(final RDF rdf, final RDFResource resource, final RDFResource value)
	{
		RDFUtilities.setProperty(rdf, resource, RDF_NAMESPACE_URI, FIRST_PROPERTY_NAME, value); //set the first value
	}

	/**Replaces all <code>rdf:rest</code> properties of the resource with a new
		property with the given value.
	@param rdf The RDF data model.
	@param resource The resource for which the rest property should be set.
	@param value The resource, which should be a list, representing the rest
		of the elements of the list.
	*/
	public static void setRest(final RDF rdf, final RDFResource resource, final RDFResource value)
	{
		RDFUtilities.setProperty(rdf, resource, RDF_NAMESPACE_URI, REST_PROPERTY_NAME, value); //set the first value
	}

	/**Returns the number of elements in this list.
	@return The number of elements in this list.
	*/
	public int size()
	{
		int size=0;	//start out knowing about no elements
		RDFResource list=this;	//start with this list resource
		while(list!=null && !NIL_RESOURCE_URI.equals(list.getReferenceURI()))	//while we have a list and it's not the nil resource
		{
			if(getFirst(list)!=null)	//if there is an element
			{
				++size;	//show that we found another element
			}
			list=getRest(list);	//look at the rest of the list
		}
		return size;	//return whatever size we found
	}

	/**Appends the specified element to the end of the list.
	<p>This implementation calls <code>add(size(), o)</code>.<p>
	@param rdf The data model to use as a resource factory. G***should we eventually just allow a ResourceLocator interface parameter, or put this method in RDFUtilities?
	@param element The element to be appended to this list.
	@exception ClassCastException Thrown if the class of the specified element
		prevents it from being added to this set.
	@exception IllegalArgumentException Thrown some aspect of this element prevents
		it from being added to this collection.
	*/
	public void add(final RDF rdf, final RDFResource element)
	{
		add(rdf, size(), element);	//add the object to the end of the list
	}

	/**Inserts the specified element at the specified position in this
		list. Shifts the element currently at that position (if any) and
		any subsequent elements to the right (adds one to their indices).
	<p>As each list is the first element in the list, an element cannot
		be inserted before the first element in the list, at index zero.
		Instead, create a new list with the new element and this list as
		parameters.</p>
	@param rdf The data model to use as a resource factory. G***should we eventually just allow a ResourceLocator interface parameter, or put this method in RDFUtilities?
	@param index The index at which the specified element is to be inserted.
	@param element The element to be inserted.
	@exception IndexOutOfBoundsException Thrown if index is out of range
		<code>(index &le; 0 || index &gt; size())</code>.
	@see #create(RDF, URI, RDFResource, RDFResource)
	*/
	public void add(RDF rdf, final int index, final RDFResource element)
	{
		int nextIndex=1;	//start with the second element 
		RDFResource list=this;	//start with this list resource
		while(list!=null && !NIL_RESOURCE_URI.equals(list.getReferenceURI()))	//while we have a list and it's not the nil resource
		{
			final RDFResource oldRest=getRest(list);	//get the current rest of the list
			if(nextIndex==index)	//if the next element is the correct insertion index
			{
				final RDFResource newRest=new RDFListResource(rdf, element, oldRest);	//create a new element for this element and the rest of the elements
				setRest(rdf, list, newRest);	//set the rest of the list
				return;	//stop looking for the appropriate index
			}
			++nextIndex;	//show that we found another element
			list=oldRest;	//look at the rest of the list
		}
		throw new IndexOutOfBoundsException("Index: "+index+", Size: "+(nextIndex-1));	//if we make it to here, we will not have found a place to insert the element
	}

}