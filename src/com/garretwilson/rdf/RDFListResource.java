package com.garretwilson.rdf;

import java.net.URI;
import java.util.*;

/**Represents an RDF list resource.
<p>Manipulating an RDF list resource using its convenience methods may add
	anonymous resources that are are not known to the underlying RDF data model
	that produced this list resource. Similarly, multiple resources may be
	created to represent the nil resource, the special immutable resource defined
	by RDF.
@author Garret Wilson
*/
public class RDFListResource extends DefaultRDFResource	//G***fix implements List//G***del, Comparator
{

//TODO create RDF data model aware reference URI constructors

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
	@param first The first and only element of the list.
	*/
	public RDFListResource(final RDFResource first)
	{
		this((URI)null, first);	//create a list with no other elements
	}

	/**Constructs an anonymous RDF list resource with a single element.
		The rest of the list is set to the RDF nil resource.
	@param rdf The RDF data model to use as a factory for creating properties.
	@param first The first and only element of the list.
	*/
	public RDFListResource(final RDF rdf, final RDFResource first)
	{
		this(rdf, (URI)null, first);	//create a list with no other elements
	}

	/**Constructs an RDF list resource with a single element.
		The rest of the list is set to the RDF nil resource.
	@param newReferenceURI The reference URI for the new resource.
	@param first The first and only element of the list.
	*/
	public RDFListResource(final URI newReferenceURI, final RDFResource first)
	{
		this(newReferenceURI, first, createNil());	//create a list with no other elements
	}

	/**Constructs an RDF list resource with a single element.
		The rest of the list is set to the RDF nil resource.
	@param rdf The RDF data model to use as a factory for creating properties.
	@param newReferenceURI The reference URI for the new resource.
	@param first The first and only element of the list.
	*/
	public RDFListResource(final RDF rdf, final URI newReferenceURI, final RDFResource first)
	{
		this(rdf, newReferenceURI, first, createNil());	//create a list with no other elements
	}

	/**Constructs an anonymous RDF list resource with a current element and the
		rest of the list.
	@param rdf The RDF data model to use as a factory for creating properties.
	@param first The first element of the list.
	@param rest The list resource representing the rest of the list, or <code>null</code>
		if no rest should be specified.
	*/
	public RDFListResource(final RDF rdf, final RDFResource first, final RDFResource rest)
	{
		this(rdf, (URI)null, first, rest); //construct a list with an anonymous reference URI
	}

	/**Constructs an RDF list resource with a current element and the rest of the list.
	@param newReferenceURI The reference URI for the new resource.
	@param first The first element of the list.
	@param rest The list resource representing the rest of the list, or <code>null</code>
		if no rest should be specified.
	*/
	public RDFListResource(final URI newReferenceURI, final RDFResource first, final RDFResource rest)
	{
		this(null, newReferenceURI, first, rest);	//construct the list with no RDF data model
	}

	/**Constructs an RDF list resource with a current element and the rest of the list.
	@param rdf The RDF data model to use as a factory for creating properties.
	@param newReferenceURI The reference URI for the new resource.
	@param first The first element of the list.
	@param rest The list resource representing the rest of the list, or <code>null</code>
		if no rest should be specified.
	*/
	public RDFListResource(final RDF rdf, final URI newReferenceURI, final RDFResource first, final RDFResource rest)
	{
		super(newReferenceURI); //construct the list with the reference URI
		//TODO use the RDF data model to store local references to the properties we need, make the non-RDF constructors protected, and then copy the property information over when we construct a new list from this class 
		setFirst(first);	//set the first element
		if(rest!=null)	//if a rest is specified
			setRest(rest);	//set the rest of the list
	}

	/**Constructs an anonymous RDF list resource with the contents of a collection.
	@param rdf The RDF data model to use as a factory for creating properties.
	@param collection The collection with which to populate the list.
	*/
	public static RDFListResource create(final RDF rdf, final Collection collection)
	{
		return create(rdf, (URI)null, collection);	//create and return a list with an anonymous reference URI
	}

	/**Constructs an RDF list resource with the contents of a collection.
	@param rdf The RDF data model to use as a factory for creating properties.
	@param newReferenceURI The reference URI for the new resource.
	@param collection The collection with which to populate the list, each
		element of which is a <code>RDFResource</code>.
	*/
	public static RDFListResource create(final RDF rdf, final URI newReferenceURI, final Collection collection)
	{
			//TODO now that we've removed the requirement of passing an RDF data model, maybe this can be optimized---or maybe removed altogether
//G***del		final RDFResource nil=rdf.locateResource(RDF_NAMESPACE_URI, NIL_RESOURCE_LOCAL_NAME);	//get the nil resource
		final RDFListResource listResource;	//we'll create a list resource for the collection 
		if(collection.size()>0)	//if there are elements in the collection
		{
			final Iterator resourceIterator=collection.iterator();	//get an iterator to the elements of the collection
				//G***maybe throw an illegal argument exception if the elements in the collection are not resources
			listResource=new RDFListResource((RDFResource)resourceIterator.next());	//get the first resource in the collection
			RDFListResource list=listResource;	//we'll keep adding to the list as we go along
			while(resourceIterator.hasNext())	//while there are more elements in the collection
			{
				
				final RDFListResource nextList=new RDFListResource((RDFResource)resourceIterator.next());	//get the next resource in the collection
				setRest(list, nextList);	//add the next list to this one
				list=nextList;	//we'll use the next list the next time around 
			}
		}
		else	//if the collection is empty
		{
			listResource=createNil();	//use the nil list
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
		not a <code>RDFResource</code> (such as a <code>RDFLiteral</code>), which would
		indicate that an incorrect value has been stored for the property.
	*/
	public static RDFResource getFirst(final RDFResource resource) throws ClassCastException
	{
		return (RDFResource)resource.getPropertyValue(RDF_NAMESPACE_URI, FIRST_PROPERTY_NAME); //return the first element property
	}

	/**Retrieves the first element of this list. If this resource has more than one
		property of <code>rdf:first</code>, it is undefined which of those property
		values will be returned.
	@return The first element of this list, or <code>null</code> if there is
		no first element specified.
	@exception ClassCastException Thrown if the first element property value is
		not a <code>RDFResource</code> (such as a <code>RDFLiteral</code>), which would
		indicate that an incorrect value has been stored for the property.
	*/
	public RDFResource getFirst() throws ClassCastException
	{
		return getFirst(this); //return the first element property
	}

	/**Retrieves the rest of the list, which is usually another list.
		If the resource has more than one property of <code>rdf:rest</code>,
		it is undefined which of those property values will be returned.
	@param resource The list resource for which the rest property will be returned.
	@return The resource representing the rest of the list, or <code>null</code> if
		the resource has no rest of the list specified.
	@exception ClassCastException Thrown if the rest property value is
		not a <code>RDFResource</code> (such as a <code>RDFLiteral</code>), which would
		indicate that an incorrect value has been stored for the property.
	*/
	public static RDFResource getRest(final RDFResource resource) throws ClassCastException
	{
		return (RDFResource)resource.getPropertyValue(RDF_NAMESPACE_URI, REST_PROPERTY_NAME); //return the rest property
	}

	/**Retrieves the rest of this list, which is usually another list.
		If this resource has more than one property of <code>rdf:rest</code>,
		it is undefined which of those property values will be returned.
	@return The resource representing the rest of the list, or <code>null</code> if
		this resource has no rest of the list specified.
	@exception ClassCastException Thrown if the rest property value is
		not a <code>RDFResource</code> (such as a <code>RDFLiteral</code>), which would
		indicate that an incorrect value has been stored for the property.
	*/
	public RDFResource getRest() throws ClassCastException
	{
		return getRest(this); //return the rest property
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

	/**Replaces all <code>rdf:first</code> properties of the resource with a new
		property with the given value.
	@param resource The resource for which the first property should be set.
	@param value The first element of the list.
	*/
	public static void setFirst(final RDFResource resource, final RDFResource value)
	{
		setFirst(new RDF(), resource, value); //set the first value G***fix data model
	}

	/**Replaces all <code>rdf:first</code> properties of this list with a new
		property with the given value.
	@param value The first element of the list.
	*/
	public void setFirst(final RDFResource value)
	{
		setFirst(this, value); //set the first value
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

	/**Replaces all <code>rdf:rest</code> properties of the resource with a new
		property with the given value.
	@param resource The resource for which the rest property should be set.
	@param value The resource, which should be a list, representing the rest
		of the elements of the list.
	*/
	public static void setRest(final RDFResource resource, final RDFResource value)
	{
		setRest(new RDF(), resource, value);	//set the first value G***fix the data model
	}

	/**Replaces all <code>rdf:rest</code> properties of this list with a new
		property with the given value.
	@param value The resource, which should be a list, representing the rest
		of the elements of the list.
	*/
	public void setRest(final RDFResource value)
	{
		setRest(this, value);	//set the first value
	}

	/**@return An iterator over the elements in this list in proper sequence.*/
	public Iterator iterator()
	{
		return new RDFListIterator();	//create a new iterator over all the list elements
	}

	/**Returns the number of elements in this list.  If this list contains more
		than <code>Integer.MAX_VALUE</code> elements, returns
		<code>Integer.MAX_VALUE</code>.
	@return The number of elements in this list.
	*/
	public int size()
	{
		int size=0;	//start out knowing about no elements
		RDFResource list=this;	//start with this list resource
		while(list!=null && !RDFUtilities.isNil(list))	//while we have a list and it's not the nil resource
		{
			++size;	//show that we found another element
			list=getRest(list);	//look at the rest of the list
		}
		return size;	//return whatever size we found
	}

	/**Returns the <code>RDFObject</code> at the specified position in this list.
	@param index The index of the element to return.
	@return The element at the specified position in this list.
	@throws IndexOutOfBoundsException Thrown if the index is out of range
		(<var>index</var>&lt;0 || <var>index</var>&gt;=size()).
	@see RDFObject
	*/
	public Object get(int index)
	{
		int currentIndex=0;	//start out on the first index
		RDFResource list=this;	//start with this list resource
		while(list!=null && !RDFUtilities.isNil(list))	//while we have a list and it's not the nil resource
		{
			if(currentIndex==index)	///if this is the correct index
			{
				return getFirst(list);	//return the element at this index
			}
			list=getRest(list);	//look at the rest of the list
			++currentIndex;	//go to the next index
		}
		throw new IndexOutOfBoundsException("The index "+index+" must be >=0 and <"+currentIndex);	//show that we don't have an element for this index
	}

	/**Appends the specified element to the end of the list.
	<p>This implementation calls <code>add(size(), o)</code>.<p>
	@param element The element to be appended to this list.
	@exception ClassCastException Thrown if the class of the specified element
		prevents it from being added to this set.
	@exception IllegalArgumentException Thrown some aspect of this element prevents
		it from being added to this collection.
	*/
	public void add(final RDFResource element)
	{
		add(size(), element);	//add the object to the end of the list
	}

	/**Inserts the specified element at the specified position in this
		list. Shifts the element currently at that position (if any) and
		any subsequent elements to the right (adds one to their indices).
	<p>As each list is the first element in the list, an element cannot
		be inserted before the first element in the list, at index zero.
		Instead, create a new list with the new element and this list as
		parameters.</p>
	@param index The index at which the specified element is to be inserted.
	@param element The element to be inserted.
	@exception IndexOutOfBoundsException Thrown if index is out of range
		<code>(index &le; 0 || index &gt; size())</code>.
	@see #create(RDF, URI, RDFResource, RDFResource)
	*/
	public void add(final int index, final RDFResource element)
	{
		int currentIndex=0;	//start out on the first index
		RDFResource list=this;	//start with this list resource, which guarantees that the list will not be null the first time around
		do	//do post-checks, because we'll always have a list the first time around, as we're using this list as the first element
		{
			if(currentIndex==index)	///if this is the correct index
			{
				final URI oldReferenceURI=list.getReferenceURI();	//get the old reference URI of this list element (which might even be the nil URI)
				final RDFResource oldFirst=getFirst(list);	//get the current first of the list
				final RDFResource oldRest=getRest(list);	//get the current rest of the list
				list.setReferenceURI(null);	//effectively change the current node into an anonymous node
				setFirst(list, element);	//set this element as the value of this node
				setRest(list, new RDFListResource(oldReferenceURI, oldFirst, oldRest));	//create a new element that mimics the old one, and add it as the rest of the list
				return;	//we've inserted the element, so there's nothing more to do here 
			}
			if(!RDFUtilities.isNil(list))	//if we're not at the last element
			{
				list=getRest(list);	//look at the rest of the list
				++currentIndex;	//go to the next index
			}
			else	//if we reached the last element
			{
				list=null;	//don't bother even looking for another list---it would be incorred for the nil list to have a rest property, anyway
			}
		}
		while(list!=null);	//keep looking while we have a list and it's not the nil resource
		throw new IndexOutOfBoundsException("The index "+index+" must be >=0 and <"+currentIndex);	//show that we don't have an element for this index
	}

	/**@return A new instance of an RDF list resource representing the nil resource.*/
	protected static RDFListResource createNil()
	{
		return new RDFListResource(RDF_NAMESPACE_URI, NIL_RESOURCE_LOCAL_NAME);	//create a nil list resource
	}

	/**An iterator that allows iteration over the the elements in the list resource.
	<p>This implementation does not support <code>remove()</code>.</p>
	*/
	protected class RDFListIterator implements Iterator
	{
		/**The next list resource in the list.*/
		protected RDFResource nextList;
		
		/**Default constructor.*/
		public RDFListIterator()
		{
			nextList=RDFListResource.this;	//the first list element will be represented by our current list resource
		}

		/**@return <code>true</code> if the iterator has more elements.*/
		public boolean hasNext()
		{
				//we have a next list element if there is a next list node and it's not the nil resource
			return nextList!=null && !RDFUtilities.isNil(nextList);			
		}

		/**@return The next element in the iteration.
		@exception NoSuchElementException iteration has no more elements.
		*/
		public Object next()
		{
			if(hasNext())	//if we have a next element
			{
				final RDFResource nextResource=getFirst(nextList);	//get the element represented by the list
				nextList=getRest(nextList);	//show which list (if any) holds the rest of the list elements
				return nextResource;	//return the resource element we found 
			}
			else	//if there is no next element
			{
				throw new NoSuchElementException();	//show that there are no more elements
			}
		}

		/**Removes from the underlying collection the last element returned by the
			iterator (optional operation).  This method can be called only once per
			call to <code>next</code>.  The behavior of an iterator is unspecified if
			the underlying collection is modified while the iteration is in
			progress in any way other than by calling this method.
		@exception UnsupportedOperationException Thrown if the <code>remove</code>
			operation is not supported by this Iterator.
		@exception IllegalStateException Thrown if the <code>next</code> method has not
			yet been called, or the <code>remove</code> method has already
			been called after the last call to the <code>next</code>
			method.
		*/
		public void remove()
		{
			throw new UnsupportedOperationException();	//show that we don't yet support removing elements			
		}

	}

}