package com.garretwilson.model;

import java.net.URI;
import com.garretwilson.io.*;

/**A model that keeps track of a base URI and knows how to access input streams
	based upon URIs.
@author Garret Wilson
*/
public abstract class URIAccessibleModel extends DefaultURIAccessible implements Model
{

	/**Whether the object has been modified; the default is not modified.*/
	private boolean modified=false;

		/**@return Whether the object been modified.*/
		public boolean isModified() {return modified;}

		/**Sets whether the object has been modified.
		This is a bound property.
		@param newModified The new modification status.
		*/
		public void setModified(final boolean newModified)
		{
			final boolean oldModified=modified; //get the old modified value
			if(oldModified!=newModified)  //if the value is really changing
			{
				modified=newModified; //update the value
					//show that the modified property has changed
				firePropertyChange(MODIFIED_PROPERTY, Boolean.valueOf(oldModified), Boolean.valueOf(newModified));
			}
		}

	/**Default constructor.*/
	public URIAccessibleModel()
	{
		super();
	}

	/**Base URI constructor.
	@param baseURI The base URI, or <code>null</code> if unknown.
	*/
	public URIAccessibleModel(final URI baseURI)
	{
		super(baseURI);
	}

	/**URI input stream locator constructor.
	@param uriInputStreamable The implementation to use for accessing a URI for
		input, or <code>null</code> if the default implementation should be used.
	*/
	public URIAccessibleModel(final URIInputStreamable uriInputStreamable)
	{
		super(uriInputStreamable);
	}

	/**URI output stream locator constructor.
	@param uriOutputStreamable The implementation to use for accessing a URI for
		output, or <code>null</code> if the default implementation should be used.
	*/
	public URIAccessibleModel(final URIOutputStreamable uriOutputStreamable)
	{
		super(uriOutputStreamable);
	}

	/**Base URI and input stream locator constructor.
	@param baseURI The base URI, or <code>null</code> if unknown.
	@param uriInputStreamable The implementation to use for accessing a URI for
		input, or <code>null</code> if the default implementation should be used.
	*/
	public URIAccessibleModel(final URI baseURI, final URIInputStreamable uriInputStreamable)
	{
		super(baseURI, uriInputStreamable);
	}

	/**Base URI and output stream locator constructor.
	@param baseURI The base URI, or <code>null</code> if unknown.
	@param uriOutputStreamable The implementation to use for accessing a URI for
		output, or <code>null</code> if the default implementation should be used.
	*/
	public URIAccessibleModel(final URI baseURI, final URIOutputStreamable uriOutputStreamable)
	{
		super(baseURI, uriOutputStreamable);
	}

	/**Full constructor.
	@param baseURI The base URI, or <code>null</code> if unknown.
	@param uriInputStreamable The implementation to use for accessing a URI for
		input, or <code>null</code> if the default implementation should be used.
	@param uriOutputStreamable The implementation to use for accessing a URI for
		output, or <code>null</code> if the default implementation should be used.
	*/
	public URIAccessibleModel(final URI baseURI, final URIInputStreamable uriInputStreamable, final URIOutputStreamable uriOutputStreamable)
	{
		super(baseURI, uriInputStreamable, uriOutputStreamable);
	}
}
