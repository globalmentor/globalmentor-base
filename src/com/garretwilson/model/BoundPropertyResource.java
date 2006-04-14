package com.garretwilson.model;

import java.net.URI;

import com.garretwilson.beans.BoundPropertyObject;

/**Represents the default implementation of a resource that recognizes bound properties.
This class provides compare functionality that sorts according to the reference URI, if available.
@author Garret Wilson
*/
public class BoundPropertyResource extends BoundPropertyObject implements Resource, Comparable<Resource>
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
	protected BoundPropertyResource()
	{
		this(null);	//construct the class without a reference URI
//G***del		referenceURI=null; //set the reference URI to null for now
	}

	/**Constructs a resource with a reference URI.
	@param referenceURI The reference URI for the new resource.
	*/
	protected BoundPropertyResource(final URI referenceURI)
	{
		this.referenceURI=referenceURI; //set the reference URI
	}

	/**Compares the resource reference URIs.
	If neither object has a reference URI, the default identity comparison is performed.
	@param object The object with which to compare this  resource.
	@return <code>true<code> if this resource equals that specified in <code>object</code>.
	@see #getReferenceURI()
	*/
	public boolean equals(final Object object)
	{
		if(object instanceof Resource)	//if we're being compared with another resource
		{
			final URI referenceURI=getReferenceURI();	//get the reference URI
			if(referenceURI!=null)	//if this resource has a reference URI
			{
				return referenceURI.equals(((Resource)object).getReferenceURI());	//compare reference URIs
			}
			else	//if this resource has no reference URI
			{
				return super.equals(object);	//compare normally
			}
		}
		else	//if the object is not a resource
		{
			return false;	//we can't compare this object to a non-resource object
		}
	}

	/**@return A hashcode value composed from the reference URI, if available.*/
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