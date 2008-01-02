package com.garretwilson.rdf;

import java.lang.reflect.Array;
import java.net.URI;
import java.util.*;

import com.garretwilson.util.Debug;
import com.globalmentor.java.Objects;

import static com.garretwilson.rdf.RDFConstants.*;
import static com.garretwilson.rdf.RDFUtilities.*;

/**Represents an RDF list resource.
<p>Manipulating an RDF list resource using its convenience methods may add
	anonymous resources that are are not known to the underlying RDF data model
	that produced this list resource. Similarly, multiple resources may be
	created to represent the nil resource, the special immutable resource defined
	by RDF.</p>
@param <E> The type of elements contained in the list.
@author Garret Wilson
*/
public class RDFListResource<E extends RDFObject> extends TypedRDFResource implements List<E> //G***del, Comparator
{

	/**@return The namespace URI of the ontology defining the default type of this resource.*/
	public URI getDefaultTypeNamespaceURI() {return RDF_NAMESPACE_URI;}

	/**@return The local name of the default type of this resource.*/
	public String getDefaultTypeName() {return LIST_CLASS_NAME;}

	/**Default constructor for an empty list with the <code>rdf:nil</code> reference URI.*/
	public RDFListResource()
	{
		this((RDF)null);	//construct a list resource with no RDF data model
	}

	/**Constructs an RDF list resource with a reference URI.
	@param newReferenceURI The reference URI for the new resource.
	*/
	public RDFListResource(final URI newReferenceURI)
	{
		this((RDF)null, newReferenceURI); //construct the class with no data model 
	}

	/**Data model, reference URI, and optional namespace URI and local name constructor.
	@param rdf The RDF data model to use as a factory for creating properties.
	@param newReferenceURI The reference URI for the new resource.
	*/
	protected RDFListResource(final RDF rdf, final URI newReferenceURI)
	{
		this(rdf, newReferenceURI, null, null); //construct the class without a first or next resource
	}

	/**Constructs an anonymous RDF list resource with a single element.
		The rest of the list is set to the RDF nil resource.
	@param first The first and only element of the list.
	*/
	public RDFListResource(final E first)
	{
		this((URI)null, first);	//create a list with no other elements
	}

	/**Data model constructor.
	@param rdf The RDF data model to use as a factory for creating properties.
	*/
	public RDFListResource(final RDF rdf)
	{
		this(rdf, NIL_RESOURCE_URI); //construct the list with a nil reference URI
	}

	/**Constructs an anonymous RDF list resource with a single element.
		The rest of the list is set to the RDF nil resource.
	@param rdf The RDF data model to use as a factory for creating properties.
	@param first The first and only element of the list.
	*/
	public RDFListResource(final RDF rdf, final E first)
	{
		this(rdf, (URI)null, first);	//create a list with no other elements
	}

	/**Constructs an RDF list resource with a single element.
		The rest of the list is set to the RDF nil resource.
	@param newReferenceURI The reference URI for the new resource.
	@param first The first and only element of the list.
	*/
	public RDFListResource(final URI newReferenceURI, final E first)
	{
		this(newReferenceURI, first, new RDFListResource<E>(NIL_RESOURCE_URI));	//create a list with no other elements
	}

	/**Constructs an RDF list resource with a single element.
		The rest of the list is set to the RDF nil resource.
	@param rdf The RDF data model to use as a factory for creating properties.
	@param newReferenceURI The reference URI for the new resource.
	@param first The first and only element of the list.
	*/
	public RDFListResource(final RDF rdf, final URI newReferenceURI, final E first)
	{
		this(rdf, newReferenceURI, first, new RDFListResource<E>(rdf, NIL_RESOURCE_URI));	//create a list with no other elements
	}

	/**Constructs an anonymous RDF list resource with a current element and the
		rest of the list.
	@param rdf The RDF data model to use as a factory for creating properties.
	@param first The first element of the list.
	@param rest The list resource representing the rest of the list, or <code>null</code>
		if no rest should be specified.
	*/
	public RDFListResource(final RDF rdf, final E first, final RDFResource rest)
	{
		this(rdf, (URI)null, first, rest); //construct a list with an anonymous reference URI
	}

	/**Constructs an RDF list resource with a current element and the rest of the list.
	@param newReferenceURI The reference URI for the new resource.
	@param first The first element of the list.
	@param rest The list resource representing the rest of the list, or <code>null</code>
		if no rest should be specified.
	*/
	public RDFListResource(final URI newReferenceURI, final E first, final RDFResource rest)
	{
		this(null, newReferenceURI, first, rest);	//construct the list with no RDF data model
	}

	/**Constructs an RDF list resource with a current element and the rest of the list.
	@param rdf The RDF data model to use as a factory for creating properties.
	@param newReferenceURI The reference URI for the new resource.
	@param first The first element of the list, or <code>null</code> if there
		should be no first element.
	@param rest The list resource representing the rest of the list, or <code>null</code>
		if no rest should be specified.
	*/
	protected RDFListResource(final RDF rdf, final URI newReferenceURI, final E first, final RDFResource rest)
	{
		super(rdf, newReferenceURI); //construct the list with the reference URI and namespace URI and local name information
		if(first!=null)	//if there is a first element
			setFirst(first);	//set the first element
		if(rest!=null)	//if a rest is specified
			setRest(rest);	//set the rest of the list
	}

	/**Constructs an anonymous RDF list resource with the contents of a collection.
	@param rdf The RDF data model to use as a factory for creating properties.
	@param collection The collection with which to populate the list.
	*/
	public static <T extends RDFObject> RDFListResource<T> create(final RDF rdf, final Collection<? extends T> collection)
	{
		return create(rdf, (URI)null, collection);	//create and return a list with an anonymous reference URI
	}

	/**Constructs an RDF list resource with the contents of a collection.
	@param rdf The RDF data model to use as a factory for creating properties.
	@param newReferenceURI The reference URI for the new resource.
	@param collection The collection with which to populate the list, each
		element of which is an {@link RDFObject}.
	*/
	public static <T extends RDFObject> RDFListResource<T> create(final RDF rdf, final URI newReferenceURI, final Collection<? extends T> collection)
	{
			//TODO now that we've removed the requirement of passing an RDF data model, maybe this can be optimized---or maybe removed altogether
//G***del		final RDFResource nil=rdf.locateResource(RDF_NAMESPACE_URI, NIL_RESOURCE_LOCAL_NAME);	//get the nil resource
		final RDFListResource<T> listResource;	//we'll create a list resource for the collection 
		if(!collection.isEmpty())	//if there are elements in the collection
		{
			final Iterator<? extends T> resourceIterator=collection.iterator();	//get an iterator to the elements of the collection
			listResource=new RDFListResource<T>(rdf, resourceIterator.next());	//get the first resource in the collection
			RDFListResource<T> list=listResource;	//we'll keep adding to the list as we go along
			while(resourceIterator.hasNext())	//while there are more elements in the collection
			{
				
				final RDFListResource<T> nextList=new RDFListResource<T>(rdf, resourceIterator.next());	//get the next resource in the collection
				setRest(list, nextList);	//add the next list to this one
				list=nextList;	//we'll use the next list the next time around 
			}
		}
		else	//if the collection is empty
		{
			listResource=new RDFListResource<T>(rdf, NIL_RESOURCE_URI);	//use the nil list
		}
		return listResource;	//return whichever list resource we created or used
	}

	/**Retrieves the first element of the list. If this resource has more than one
		property of <code>rdf:first</code>, it is undefined which of those property
		values will be returned.
	@param resource The list resource for which the first element will be returned.
	@return The first element of the list, or <code>null</code> if the resource
		has no first element specified.
	*/
	public static <T extends RDFObject> T getFirst(final RDFResource resource)
	{
		return (T)resource.getPropertyValue(RDF_NAMESPACE_URI, FIRST_PROPERTY_NAME); //return the first element property
	}

	/**Retrieves the first element of this list. If this resource has more than one
		property of <code>rdf:first</code>, it is undefined which of those property
		values will be returned.
	@return The first element of this list, or <code>null</code> if there is
		no first element specified.
	*/
	@SuppressWarnings("unchecked")	//cast needed so that Sun JDK 1.6.0_03-b05 will know which type we want; not required for Eclipse 3.4M3
	public E getFirst()
	{
		return (E)getFirst(this); //return the first element property
	}

	/**Retrieves the rest of the list, which is usually another list.
		If the resource has more than one property of <code>rdf:rest</code>,
		it is undefined which of those property values will be returned.
	@param resource The list resource for which the rest property will be returned.
	@return The resource representing the rest of the list, or <code>null</code> if
		the resource has no rest of the list specified.
	*/
	public static RDFResource getRest(final RDFResource resource)
	{
		return asResource(resource.getPropertyValue(RDF_NAMESPACE_URI, REST_PROPERTY_NAME)); //return the rest property
	}

	/**Retrieves the rest of this list, which is usually another list.
		If this resource has more than one property of <code>rdf:rest</code>,
		it is undefined which of those property values will be returned.
	@return The resource representing the rest of the list, or <code>null</code> if
		this resource has no rest of the list specified.
	*/
	public RDFResource getRest()
	{
		return getRest(this); //return the rest property
	}

	/**Replaces all <code>rdf:first</code> properties of the resource with a new
		property with the given value.
	@param resource The resource for which the first property should be set.
	@param value The first element of the list, or <code>null</code> if the
		element should be removed.
	*/
	public static <T extends RDFObject> void setFirst(final RDFResource resource, final T value)
	{
		if(value!=null)	//if there is a value
		{
			resource.setProperty(RDF_NAMESPACE_URI, FIRST_PROPERTY_NAME, value); //set the first value
		}
		else	//if there is no value
		{
			resource.removeProperties(RDF_NAMESPACE_URI, FIRST_PROPERTY_NAME); //remove the first value		
		}		
	}

	/**Replaces all <code>rdf:first</code> properties of this list with a new
		property with the given value.
	@param value The first element of the list, or <code>null</code> if the
		element should be removed.
	*/
	public void setFirst(final E value)
	{
		setFirst(this, value); //set the first value
	}

	/**Replaces all <code>rdf:rest</code> properties of the resource with a new
		property with the given value.
	@param resource The resource for which the rest property should be set.
	@param value The resource, which should be a list, representing the rest
		of the elements of the list, or <code>null</code> if the rest should be
		removed.
	*/
	public static void setRest(final RDFResource resource, final RDFResource value)
	{
		if(value!=null)	//if there is a value
		{
			resource.setProperty(RDF_NAMESPACE_URI, REST_PROPERTY_NAME, value); //set the first value
		}
		else	//if there is no value
		{
			resource.removeProperties(RDF_NAMESPACE_URI, REST_PROPERTY_NAME); //remove the rest of the list		
		}
	}

	/**Replaces all <code>rdf:rest</code> properties of this list with a new
		property with the given value.
	@param value The resource, which should be a list, representing the rest
		of the elements of the list, or <code>null</code> if the rest should be
		removed.
	*/
	public void setRest(final RDFResource value)
	{
		setRest(this, value);	//set the first value
	}

	/**@return A new instance of an RDF list resource representing the nil resource.*/
	protected RDFListResource<E> createNil()
	{
		return new RDFListResource<E>(getRDF(), NIL_RESOURCE_URI);	//create a nil list resource
	}

	/**Returns the number of elements in this list.  If this list contains more
		than <code>Integer.MAX_VALUE</code> elements, returns
		<code>Integer.MAX_VALUE</code>.
	@return The number of elements in this list.
	*/
	public int size()	//TODO cache the size of the list if we can at all, because walking the list is very expensive; perhaps only cache the size if there are no non-anonymous list nodes within the list (even though if a data model was constructed from a nodeID serialization, other lists could reference an anonymous sublist)
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

	/**@return <code>true</code> if this list contains no elements.*/
	public boolean isEmpty()
	{
		return RDFUtilities.isNil(this);	//we're empty if we're the nil list		
	}

	/**Determines if this list contains the specified element.
	@param object The element whose presence in this list is to be tested.
	@return <code>true</code> if this list contains the specified element
	@exception ClassCastException Thrown if the specified element is not an {@link RDFObject}.
	@see #indexOf(Object)
	*/
	public boolean contains(final Object object)
	{
		return indexOf(object)>=0;	//if the object has a valid index, it's contained in the list
	}

	/**@return An iterator over the elements in this list in proper sequence.*/
	public Iterator<E> iterator()
	{
		return new RDFListIterator();	//create a new iterator over all the list elements
	}

	/**
	 * Returns a list iterator of the elements in this list (in proper
	 * sequence).
	 *
	 * @return a list iterator of the elements in this list (in proper
	 * 	       sequence).
	 */
	public ListIterator<E> listIterator()
	{
		return null;	//TODO implement listIterator(), and have iterator() call this
	}

	/**
	 * Returns a list iterator of the elements in this list (in proper
	 * sequence), starting at the specified position in this list.  The
	 * specified index indicates the first element that would be returned by
	 * an initial call to the <tt>next</tt> method.  An initial call to
	 * the <tt>previous</tt> method would return the element with the
	 * specified index minus one.
	 *
	 * @param index index of first element to be returned from the
	 *		    list iterator (by a call to the <tt>next</tt> method).
	 * @return a list iterator of the elements in this list (in proper
	 * 	       sequence), starting at the specified position in this list.
	 * @throws IndexOutOfBoundsException if the index is out of range (index
	 *         &lt; 0 || index &gt; size()).
	 */
	public ListIterator<E> listIterator(int index)
	{
		return null;	//TODO implement listIterator(), and have iterator() call this
	}

	/**@return an array containing all of the elements in this collection.*/
	public Object[] toArray()
	{
		return toArray(new RDFObject[size()]);	//create a new array of RDF objects, store the elements in it, and return the array TODO is this correct? check
	}

	/**Returns an array containing all of the elements in this list. 
	<p>If the list fits in the specified array with room to spare (i.e., the
		array has more elements than the list), the element in the array
		immediately following the end of the collection is set to
		<code>null</code>.</p>
	@param array The array into which the elements of this list are to be
		stored, if it is big enough; otherwise, a new array of the same runtime
		type is allocated for this purpose.
	@return An array containing the elements of this list.
	@exception ArrayStoreException Throw if the runtime type of the specified array
	 is not a supertype of the runtime type of every element in this list.
	@exception NullPointerException if the specified array is <code>null</code>.
	*/
	@SuppressWarnings("unchecked")
	public <T> T[] toArray(T[] array)
	{
		final int size=size();	//get our size
		if(array.length<size)	//if the given array is not large enough
		{
			array=(T[])Array.newInstance(array.getClass().getComponentType(), size);	//create a new array
		}
		int i=0;	//this will index into the array we're filling
		final Iterator<E> iterator=iterator();	//get an iterator to the elements
		while(iterator.hasNext())	//while there are more elements
		{
			try
			{
				array[i++]=(T)iterator.next();	//put the next object into the array and advance our index
			}
			catch(ClassCastException classCastException)	//if there is a problem casting to T
			{
				throw (ArrayStoreException)new ArrayStoreException(classCastException.getMessage()).initCause(classCastException);
			}
			
		}
		if(array.length>size)	//if we didn't fill the array
			array[size]=null;	//set the next element to null
		return array;	//return the array we filled
	}

	/**Returns the first contained resource with the specified
	 	reference URI.
	@param referenceURI The reference URI of the resource to retrieve.
	@return The first contained resource with the given reference URI, or
		<code>null</code> if the list does not contain a resource with the given
		reference URI.
	*/
	public RDFResource getResourceByReferenceURI(final URI uri)
	{
		int currentIndex=0;	//start out on the first index
		RDFResource list=this;	//start with this list resource
		while(list!=null && !RDFUtilities.isNil(list))	//while we have a list and it's not the nil resource
		{
			final RDFObject object=getFirst(list);	//get this resource
			if(object instanceof RDFResource)
			{
				final RDFResource resource=(RDFResource)object;	//get the resource
				if(uri.equals(resource.getURI()))	//if this resource has the correct reference URI
				{
					return resource;	//return the resource
				}
			}
			list=getRest(list);	//look at the rest of the list
			++currentIndex;	//go to the next index
		}
		return null;	//report that we couldn't find a resource containing the requested resource
	}

	/**Returns the <code>RDFObject</code> at the specified position in this list.
	@param index The index of the element to return.
	@return The element at the specified position in this list.
	@exception IndexOutOfBoundsException Thrown if the index is out of range
		(<var>index</var>&lt;0 || <var>index</var>&gt;=size()).
	@see RDFObject
	*/
	@SuppressWarnings("unchecked")	//cast needed so that Sun JDK 1.6.0_03-b05 will know which type we want; not required for Eclipse 3.4M3
	public E get(int index)
	{
		int currentIndex=0;	//start out on the first index
		RDFResource list=this;	//start with this list resource
		while(list!=null && !RDFUtilities.isNil(list))	//while we have a list and it's not the nil resource
		{
			if(currentIndex==index)	///if this is the correct index
			{
				return (E)getFirst(list);	//return the element at this index
			}
			list=getRest(list);	//look at the rest of the list
			++currentIndex;	//go to the next index
		}
		throw new IndexOutOfBoundsException("The index "+index+" must be >=0 and <"+currentIndex);	//show that we don't have an element for this index
	}

	/**Replaces the element at the specified position in this list with the
		specified element.
	@param index The index of element to replace.
	@param object The element to be stored at the specified position.
	@return the element previously at the specified position.
	@exception ClassCastException Thrown if the class of the specified element
		prevents it from being added to this list.
	@exception IllegalArgumentException if some aspect of the specified
		element prevents it from being added to this list.
	@exception IndexOutOfBoundsException Thrown if the index is out of range
		(index &lt; 0 || index &gt;= size()).
	*/
	@SuppressWarnings("unchecked")	//cast needed so that Sun JDK 1.6.0_03-b05 will know which type we want; not required for Eclipse 3.4M3
	public E set(final int index, final E object)
	{
		int currentIndex=0;	//start out on the first index
		RDFResource list=this;	//start with this list resource, which guarantees that the list will not be null the first time around
		while(list!=null && !RDFUtilities.isNil(list))	//while we have a list and it's not the nil resource
		{
			if(currentIndex==index)	///if this is the correct index
			{
				final E oldFirst=(E)getFirst(list);	//get the current first of the list
				setFirst(list, object);	//set this element as the value of this node
				return oldFirst;	//return the element that was previously at this index  
			}
			list=getRest(list);	//look at the rest of the list
			++currentIndex;	//go to the next index
		}
		throw new IndexOutOfBoundsException("The index "+index+" must be >=0 and <"+currentIndex);	//show that we don't have an element for this index		
	}

	/**Appends the specified element to the end of the list.
	<p>This implementation calls <code>add(size(), o)</code>.<p>
	@param element The element to be appended to this list.
	@return <code>true</code> if this collection changed as a result of the call.
	@exception ClassCastException Thrown if the class of the specified element
		prevents it from being added to this list.
	@exception IllegalArgumentException Thrown some aspect of this element
		prevents it from being added to this collection.
	*/
	public boolean add(final E object)
	{
		add(size(), object);	//add the object to the end of the list
		return true;	//show that we modified the list
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
	@exception ClassCastException Thrown if the class of the specified element
		prevents it from being added to this list.
	@exception IndexOutOfBoundsException Thrown if index is out of range
		<code>(index &le; 0 || index &gt; size())</code>.
	@see #create(RDF, URI, RDFObject, RDFResource)
	*/
	@SuppressWarnings("unchecked")	//cast needed so that Sun JDK 1.6.0_03-b05 will know which type we want; not required for Eclipse 3.4M3
	public void add(final int index, final E object)
	{
		int currentIndex=0;	//start out on the first index
		RDFResource list=this;	//start with this list resource, which guarantees that the list will not be null the first time around
		do	//do post-checks, because we'll always have a list the first time around, as we're using this list as the first element
		{
			if(currentIndex==index)	///if this is the correct index
			{
				final URI oldReferenceURI=list.getURI();	//get the old reference URI of this list element (which might even be the nil URI)
				final E oldFirst=(E)getFirst(list);	//get the current first of the list
				final RDFResource oldRest=getRest(list);	//get the current rest of the list
				list.setReferenceURI(null);	//effectively change the current node into an anonymous node
				setFirst(list, object);	//set this element as the value of this node
				setRest(list, new RDFListResource<E>(oldReferenceURI, oldFirst, oldRest));	//create a new element that mimics the old one, and add it as the rest of the list
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

	/**Removes a single instance of the specified element from this list,
		if it is present
	@param object The element to be removed from this list, if present.
	@return <code>true</code> if this list changed as a result of the call.
	@exception ClassCastException if the type of the specified element
		is incompatible with this list.
	*/
	public boolean remove(final Object object)
	{
//TODO del		final RDFObject rdfObject=(RDFObject)object;	//cast the object to an RDF object to make sure it's the correct type
		RDFResource list=this;	//start with this list resource
		while(list!=null && !RDFUtilities.isNil(list))	//while we have a list and it's not the nil resource
		{
			final RDFObject first=getFirst(list);	//get the first object
			final RDFResource rest=getRest(list);	//get the rest of the list
			if(Objects.equals(object, first))	//if this node contains the correct object
			{
				setFirst(list, getFirst(rest));	//transfer the element of the next node to this node
				setRest(list, getRest(rest));	//transfer the rest of the next node to this node					
				if(RDFUtilities.isNil(rest))	//if the rest of the list is the nil list, the empty list
				{
					list.setReferenceURI(rest.getURI());	//effectively change the current node into the empty list
				}
				return true;	//we've removed the current value by taking over the value of the next node and removing the next node from our list
			}
			list=rest;	//look at the rest of the list
		}
		return false;	//show that we didn't modify the list
	}

	/**Removes the element at the specified position in this list.
	@param index The index of the element to remove.
	@return The element previously at the specified position.
	@exception IndexOutOfBoundsException Thrown if the index is out of range
		(index &lt; 0 || index &gt;= size()).
	*/
	@SuppressWarnings("unchecked")	//cast needed so that Sun JDK 1.6.0_03-b05 will know which type we want; not required for Eclipse 3.4M3
	public E remove(final int index)
	{
		int currentIndex=0;	//start out on the first index
		RDFResource list=this;	//start with this list resource
		while(list!=null && !RDFUtilities.isNil(list))	//while we have a list and it's not the nil resource
		{
			final E first=(E)getFirst(list);	//get the first object
			final RDFResource rest=getRest(list);	//get the rest of the list
			if(currentIndex==index)	///if this is the correct index
			{
				setFirst(list, getFirst(rest));	//transfer the element of the next node to this node
				setRest(list, getRest(rest));	//transfer the rest of the next node to this node					
				if(RDFUtilities.isNil(rest))	//if the rest of the list is the nil list, the empty list
				{
					list.setReferenceURI(rest.getURI());	//effectively change the current node into the empty list
				}
				return first;	//we've removed the current value; return the old first value
			}
			list=rest;	//look at the rest of the list
			++currentIndex;	//go to the next index
		}
		throw new IndexOutOfBoundsException("The index "+index+" must be >=0 and <"+currentIndex);	//show that we don't have an element for this index		
	}

	/**Determines if this list contains all of the elements in the specified
		collection.
	@param collection The collection to be checked for containment in this list.
	@return <code>true</code> if this list contains all of the elements
		in the specified collection
	@exception ClassCastException Thrown if the types of one or more elements
		in the specified list are incompatible with this collection.
	@exception NullPointerException Thrown if the specified collection is
		<code>null</code>.
	@see #contains(Object)
	*/
	public boolean containsAll(final Collection collection)
	{
		final Iterator iterator=collection.iterator();	//get an iterator to the collection
		while(iterator.hasNext())	//while there are more elements in the collection
		{
			if(!contains(iterator.next()))	//if we don't contain the next element
			{
				return false;	//show that there at least one element we don't contain
			}
		}
		return true;	//we checked all elements, and we have them all
	}

	/**Adds all of the elements in the specified collection to this list.
	@param collection The elements to be inserted into this list.
	@return <code>true</code> if this list changed as a result of the call.
	@exception ClassCastException Thrown if the class of an element of the specified
		collection prevents it from being added to this list.
	@exception IllegalArgumentException some aspect of an element of the specified
		collection prevents it from being added to this list.
	@see #add(Object)
	*/
	public boolean addAll(final Collection<? extends E> collection)
	{
		boolean modified=false;	//we haven't modified the list, yet
    for(E resource:collection)   //look at each resource in the collection
		{
			if(add(resource))	//add the next element to our list; if the operation modified the list
			{
				modified=true;	//show that we modified the list
			}
		}
		return modified;	//show whether we modified this list
	}

	/**
	 * Inserts all of the elements in the specified collection into this
	 * list at the specified position (optional operation).  Shifts the
	 * element currently at that position (if any) and any subsequent
	 * elements to the right (increases their indices).  The new elements
	 * will appear in this list in the order that they are returned by the
	 * specified collection's iterator.  The behavior of this operation is
	 * unspecified if the specified collection is modified while the
	 * operation is in progress.  (Note that this will occur if the specified
	 * collection is this list, and it's nonempty.)
	 *
	 * @param index index at which to insert first element from the specified
	 *	            collection.
	 * @param c elements to be inserted into this list.
	 * @return <tt>true</tt> if this list changed as a result of the call.
	 * 
	 * @throws UnsupportedOperationException if the <tt>addAll</tt> method is
	 *		  not supported by this list.
	 * @throws ClassCastException if the class of one of elements of the
	 * 		  specified collection prevents it from being added to this
	 * 		  list.
	 * @throws NullPointerException if the specified collection contains one
	 *           or more null elements and this list does not support null
	 *           elements, or if the specified collection is <tt>null</tt>.
	 * @throws IllegalArgumentException if some aspect of one of elements of
	 *		  the specified collection prevents it from being added to
	 *		  this list.
	 * @throws IndexOutOfBoundsException if the index is out of range (index
	 *		  &lt; 0 || index &gt; size()).
	 */
	public boolean addAll(final int index, final Collection<? extends E> collection)	//TODO implement addAll(int, Collection)
	{
		throw new UnsupportedOperationException("RDFListResource does not yet support addAll(int, Collection)");		
	}

	/**Removes all this collection's elements that are also contained in the
		specified collection.
	@param collection The elements to be removed from this list.
	@return <code>true</code> if this list changed as a result of the call.
	@exception ClassCastException Thrown if the class of an element of the specified
		collection prevents it from being added to this list.
	@exception NullPointerException Thrown if the specified collection is <code>null</code>.
	@see #remove(Object)
	@see #contains(Object)
	*/
	public boolean removeAll(final Collection<?> collection)
	{
		boolean modified=false;	//we haven't modified the list, yet
    for(Object object:collection)   //look at each object in the collection
		{
			if(remove(object))	//remove the next element from our list; if the operation modified the list
			{
				modified=true;	//show that we modified the list
			}
		}
		return modified;	//show whether we modified this list		
	}

	/**Retains only the elements in this collection that are contained in the
		specified collection.
	<p>This method is currently unsupported.</p>
	@param collection The elements to be retained in this list.
	@return <code>true</code> if this collection changed as a result of the call.
	@exception UnsupportedOperationException Thrown if the <code>retainAll</code>
		method is not supported by this list.
	@exception ClassCastException Thrown if the class of an element of the specified
		collection prevents it from being added to this list.
	@exception NullPointerException Thrown if the specified collection is <code>null</code>
	@see #remove(Object)
	@see #contains(Object)
	*/
	public boolean retainAll(final Collection<?> collection)	//TODO add retainAll() support
	{
		throw new UnsupportedOperationException("RDFListResource does not yet support retainAll()");
	}

	/**Returns the index in this list of the first occurrence of the specified
		element, or -1 if this list does not contain this element.
	@param object The element to search for.
	@return The index in this list of the first occurrence of the specified
		element, or -1 if this list does not contain this element.
	@exception ClassCastException Thrown if the type of the specified element
		is incompatible with this list.
	*/
	public int indexOf(final Object object)
	{
		int index=0;	//start out on the first index
//TODO del		final RDFObject rdfObject=(RDFObject)object;	//cast the object to an RDF object to make sure it's the correct type
		RDFResource list=this;	//start with this list resource
		while(list!=null && !RDFUtilities.isNil(list))	//while we have a list and it's not the nil resource
		{
			final RDFObject first=getFirst(list);	//get the first object
			if(Objects.equals(object, first))	//if the objects are equal (taking into account nulls)
			{
				return index;	//show the index at which we found the object 
			}
			list=getRest(list);	//look at the rest of the list
			++index;	//go to the next index
		}
		return -1;	//show that we couldn't find the specified object
	}

	/**Returns the index in this list of the last occurrence of the specified
		element, or -1 if this list does not contain this element.
	@param object The element to search for.
	@return The index in this list of the last occurrence of the specified
		element, or -1 if this list does not contain this element.
	@exception ClassCastException Thrown if the type of the specified element
		is incompatible with this list.
	*/
	public int lastIndexOf(final Object object)
	{
		int lastIndexOf=-1;	//start off not knowing the last index of the object
		int index=0;	//start out on the first index
//TODO del		final RDFObject rdfObject=(RDFObject)object;	//cast the object to an RDF object to make sure it's the correct type
		RDFResource list=this;	//start with this list resource
		while(list!=null && !RDFUtilities.isNil(list))	//while we have a list and it's not the nil resource
		{
			final RDFObject first=getFirst(list);	//get the first object
			if(Objects.equals(object, first))	//if the objects are equal (taking into account nulls)
			{
				lastIndexOf=index;	//keep track of the last index at which we found the object 
			}
			list=getRest(list);	//look at the rest of the list
			++index;	//go to the next index
		}
		return lastIndexOf;	//return the last index at which we found the object, or -1 if we couldn't find it a tall 		
	}

	/**Removes all of the elements from this collection.*/
	public void clear()
	{
		setFirst(null);	//remove our first element
		setRest(null);	//remove the rest of the list
		setReferenceURI(NIL_RESOURCE_URI);	//set our reference URI to the nil reference URI
	}

	/**
	 * Returns a view of the portion of this list between the specified
	 * <tt>fromIndex</tt>, inclusive, and <tt>toIndex</tt>, exclusive.  (If
	 * <tt>fromIndex</tt> and <tt>toIndex</tt> are equal, the returned list is
	 * empty.)  The returned list is backed by this list, so non-structural
	 * changes in the returned list are reflected in this list, and vice-versa.
	 * The returned list supports all of the optional list operations supported
	 * by this list.<p>
	 *
	 * This method eliminates the need for explicit range operations (of
	 * the sort that commonly exist for arrays).   Any operation that expects
	 * a list can be used as a range operation by passing a subList view
	 * instead of a whole list.  For example, the following idiom
	 * removes a range of elements from a list:
	 * <pre>
	 *	    list.subList(from, to).clear();
	 * </pre>
	 * Similar idioms may be constructed for <tt>indexOf</tt> and
	 * <tt>lastIndexOf</tt>, and all of the algorithms in the
	 * <tt>Collections</tt> class can be applied to a subList.<p>
	 *
	 * The semantics of the list returned by this method become undefined if
	 * the backing list (i.e., this list) is <i>structurally modified</i> in
	 * any way other than via the returned list.  (Structural modifications are
	 * those that change the size of this list, or otherwise perturb it in such
	 * a fashion that iterations in progress may yield incorrect results.)
	 *
	 * @param fromIndex low endpoint (inclusive) of the subList.
	 * @param toIndex high endpoint (exclusive) of the subList.
	 * @return a view of the specified range within this list.
	 * 
	 * @throws IndexOutOfBoundsException for an illegal endpoint index value
	 *     (fromIndex &lt; 0 || toIndex &gt; size || fromIndex &gt; toIndex).
	 */
	public List<E> subList(final int fromIndex, final int toIndex)
	{
		return null;	//TODO implement subList()
	}

	/**An iterator that allows iteration over the the elements in the list resource.
	<p>This implementation does not support <code>remove()</code>.</p>
	*/
	protected class RDFListIterator implements Iterator<E>
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
				//we have a next list element if there is a next list node and it's not the nil resource;
				//make sure it has a next element as well, in case the list is corrupt, as that is what the next() method will use
			return nextList!=null && !RDFUtilities.isNil(nextList) && getFirst(nextList)!=null;			
		}

		/**@return The next element in the iteration.
		@exception NoSuchElementException iteration has no more elements.
		*/
		@SuppressWarnings("unchecked")	//cast needed so that Sun JDK 1.6.0_03-b05 will know which type we want; not required for Eclipse 3.4M3
		public E next()
		{
			if(nextList!=null && !RDFUtilities.isNil(nextList))	//if we have a next list that isn't the nil resource
			{
				final E nextResource=(E)getFirst(nextList);	//get the element represented by the list
				if(nextResource!=null)	//if there is a next resource
				{
					nextList=getRest(nextList);	//show which list (if any) holds the rest of the list elements
					return nextResource;	//return the resource element we found
				}
			}
			throw new NoSuchElementException();	//show that there are no more elements
		}

		/**Removes from the underlying collection the last element returned by the iterator.
		This method can be called only once per call to {@link #next()}. 
		The behavior of an iterator is unspecified if the underlying collection is modified while the iteration is in progress in any way other than by calling this method.
		This implementation always throws an {@link UnsupportedOperationException}.
		@exception UnsupportedOperationException Thrown if the {@link #remove()} operation is not supported by this Iterator.
		@exception IllegalStateException Thrown if the {@link #next()} method has not yet been called,
			or the {@link #remove()} method has already been called after the last call to the {@link #next()} method.
		*/
		public void remove()
		{
			throw new UnsupportedOperationException();	//show that we don't yet support removing elements			
		}

	}

}