package com.garretwilson.net;

import static com.garretwilson.text.CharacterConstants.*;

import java.net.URI;

/**Represents the default implementation of a resource.
This class provides compare functionality that sorts according to the reference URI, if available.
@author Garret Wilson
*/
public class DefaultResource implements Resource, Comparable<Resource>
{

	/**The resource identifier URI, or <code>null</code> if the identifier is not known.*/
	private URI uri;

		/**@return The resource identifier URI, or <code>null</code> if the identifier is not known.*/
		public URI getURI() {return uri;}

		/**Sets the URI of the resource.
		@param uri The new URI, or <code>null</code> if the identifier is not known.
		*/
		public void setURI(final URI uri) {this.uri=uri;}

	/**Default constructor that allows the URI to be set later.*/
	protected DefaultResource()
	{
		this(null);	//construct the class without a URI
	}

	/**URI constructor.
	@param uri The URI for the new resource.
	*/
	protected DefaultResource(final URI uri)
	{
		this.uri=uri; //set the reference URI
	}

	/**Compares the resource URIs.
	If neither object has a reference URI, the default identity comparison is performed.
	@param object The object with which to compare this  resource.
	@return <code>true<code> if this resource equals that specified in <code>object</code>.
	@see #getURI()
	*/
	public boolean equals(final Object object)
	{
		if(object instanceof Resource)	//if we're being compared with another resource
		{
			final URI uri=getURI();	//get the reference URI
			if(uri!=null)	//if this resource has a reference URI
			{
				return uri.equals(((Resource)object).getURI());	//compare reference URIs
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
		return getURI()!=null ? getURI().hashCode() : super.hashCode();
	}

	/**Compares this object to another object.
	<p>This method determines order based upon the reference URI of the resource,
		if any; otherwise, the string versions of the resources are compared.</p>
	@param resource The resource with which to compare this resouce.
	@return A negative integer, zero, or a positive integer as this resource
		reference URI is less than, equal to, or greater than the reference URI of
		the specified resource, respectively.
	@see #getURI()
	*/
	public int compareTo(final Resource resource)	//TODO comparing different things may result in circular comparisons
	{
		if(getURI()!=null && resource.getURI()!=null)	//if both resources have reference URIs
		{
			return getURI().compareTo(resource.getURI()); //compare reference URIs
		}
		else	//if one of the two resources doesn't have a reference URI
			return toString().compareTo(resource.toString());	//compare strings
	}

	/**Returns a string representation of the resource.
	This version returns the URI, if there is one, between double angle quotation marks; otherwise the default string representation of the object is returned.
	@return A string representation of the resource.
	*/
	public String toString()
	{
		final URI uri=getURI();	//get the URI, if any
		return uri!=null ? new StringBuilder().append(LEFT_POINTING_DOUBLE_ANGLE_QUOTATION_MARK_CHAR).append(uri).append(RIGHT_POINTING_DOUBLE_ANGLE_QUOTATION_MARK_CHAR).toString() : super.toString();	//return the URI, if available
	}

}