package com.garretwilson.model;

import java.net.URI;

import com.garretwilson.util.Debug;

/**Represents the default implementation of a resource.
	This class provides compare functionality that sorts according to the reference
	URI, if available.
@author Garret Wilson
*/
public class DefaultResource implements Resource, Comparable<Resource>
{

	/**The resource identifier URI, or <code>null</code> if the identifier is not known.*/
	private URI referenceURI;

		/**@return The resource identifier URI, or <code>null</code> if the identifier is not known.*/
		public URI getReferenceURI() {return referenceURI;}

		/**Sets the reference URI of the resource.
		@param uri The new reference URI, or <code>null</code> if the identifier is not known.
		*/
		public void setReferenceURI(final URI uri) {referenceURI=uri;}

	/**@return The unique identifier of the object.*/
//TODO del when not needed	public URI getID() {return getReferenceURI();}

	/**Default constructor that allows the reference URI to be set later.*/
	protected DefaultResource()
	{
		this(null);	//construct the class without a reference URI
//G***del		referenceURI=null; //set the reference URI to null for now
	}

	/**Constructs a resource with a reference URI.
	@param referenceURI The reference URI for the new resource.
	*/
	protected DefaultResource(final URI referenceURI)
	{
		this.referenceURI=referenceURI; //set the reference URI
	}

	/**If <code>object</code> is another <code>Resource</code>, compares the
		resource reference URIs.
		Otherwise, compares the objects using the superclass functionality.
	@param object The object with which to compare this  resource; should be
		another resource.
	@return <code>true<code> if this resource equals that specified in
		<code>object</code>.
	@see #getReferenceURI
	*/
	public boolean equals(final Object object)	//G***do we really want to compare with a non-URI?
	{
		assert !(object instanceof URI) : "DefaultResource no longer allows equals(URI)";	//G***del when not needed
		assert !(object instanceof String) : "DefaultResource no longer allows equals(String)";	//G***del when not needed

		if(getReferenceURI()!=null)	//if we have a reference URI
		{
			if(object instanceof Resource)	//if we're being compared with another resource
			{
				final URI otherReferenceURI=((Resource)object).getReferenceURI();	//get the other reference URI
				if(otherReferenceURI!=null)	//if we can compare URIs
					return getReferenceURI().equals(otherReferenceURI);	//see if the URIs match
			}
		}
		return super.equals(object);	//if we're being compared with anything else or have a null reference URI, use the default compare
	}

	/**@return A hashcode value composed from the reference URI.*/
	public int hashCode()
	{
			//return the hash code of the reference URI unless there is no reference ID;
			//  in that case, return the default hash code
		return getReferenceURI()!=null ? getReferenceURI().hashCode() : super.hashCode();
	}

	/**Compares this object to another object.
	<p>This method determines order based upon the reference URI of the resource,
		if any; otherwise, the string versions of the resources are compared.</p>
	@param resource The resource with which to compare this resouce.
	@return A negative integer, zero, or a positive integer as this resource
		reference URI is less than, equal to, or greater than the reference URI of
		the specified resource, respectively.
	@see #getReferenceURI
	*/
	public int compareTo(final Resource resource)	//TODO comparing different things may result in circular comparisons
	{
		if(getReferenceURI()!=null && resource.getReferenceURI()!=null)	//if both resources have reference URIs
		{
			return getReferenceURI().compareTo(resource.getReferenceURI()); //compare reference URIs
		}
		else	//if one of the two resources doesn't have a reference URI
			return toString().compareTo(resource.toString());	//compare strings
	}

	/**Returns a string representation of the resource.
	<p>This version returns the reference URI if there is one; otherwise, the
		default string representation of the object is returned.</p>
	@return A string representation of the resource.
	*/
	public String toString()
	{
		return getReferenceURI()!=null ? getReferenceURI().toString() : super.toString();	//return the reference URI, if available
	}
}