package com.garretwilson.model;

import java.io.*;
import java.net.URI;
import com.garretwilson.io.*;

/**A model that keeps track of a base URI and knows how to access input streams
	based upon URIs.
<p>Bound properties:</p>
<dl>
	<dt><code>URIAccessible.BASE_URI_PROPERTY</code> (<code>URI</code>)</dt>
	<dd>Indicates that the base URI of the model.</dd>
	<dt><code>URIAccessible.URI_INPUT_STREAMABLE_PROPERTY</code> (<code>URIInputStreamable</code>)</dt>
	<dd>Indicates that model's source of input streams.</dd>
</dl>
@author Garret Wilson
@see URIAccessible#BASE_URI_PROPERTY
@see URIAccessible#URI_INPUT_STREAMABLE_PROPERTY
*/
public abstract class URIAccessibleModel extends DefaultModel implements URIAccessible
{

	/**The base URI of the model, or <code>null</code> if unknown.*/
	private URI baseURI;
	
		/**@return The base URI of the model, or <code>null</code> if unknown.*/
		public URI getBaseURI() {return baseURI;}

		/**Sets the base URI.
		This is a bound property.
		@param newBaseURI The base URI of the model, or <code>null</code> if unknown.
		*/
		protected void setBaseURI(final URI newBaseURI)
		{
			final URI oldBaseURI=baseURI; //get the old value
			if(oldBaseURI!=newBaseURI)  //if the value is really changing
			{
				baseURI=newBaseURI; //update the value
					//show that the value has changed
				firePropertyChange(BASE_URI_PROPERTY, oldBaseURI, newBaseURI);
			}			
		}

	/**The implementation to use for retrieving an input stream to a URI.*/
	private URIInputStreamable uriInputStreamable;

		/**@return The non-<code>null</code> implementation to use for retrieving an input stream to a URI.*/
		public URIInputStreamable getURIInputStreamable() {return uriInputStreamable;}
		
		/**Sets the implementation to use for retrieving an input stream to a URI.
		This is a bound property.
		@param uriInputStreamable The implementation to use for accessing a URI for
			input, or <code>null</code> if the default implementation should be used.
		*/
		protected void setURIInputStreamable(final URIInputStreamable uriInputStreamable)
		{
			final URIInputStreamable newURIInputStreamable=uriInputStreamable!=null ? uriInputStreamable : this;	//use our default URI input streamable if they passed null
			final URIInputStreamable oldURIInputStreamable=this.uriInputStreamable; //get the old value
			if(oldURIInputStreamable!=newURIInputStreamable)  //if the value is really changing
			{
				this.uriInputStreamable=newURIInputStreamable; //update the value
					//show that the value has changed
				firePropertyChange(URI_INPUT_STREAMABLE_PROPERTY, oldURIInputStreamable, newURIInputStreamable);
			}			
		}

	/**Default constructor.*/
	public URIAccessibleModel()
	{
		this((URI)null);
	}

	/**Base URI constructor.
	@param baseURI The base URI of the model, or <code>null</code> if unknown.
	*/
	public URIAccessibleModel(final URI baseURI)
	{
		this(baseURI, null);
	}

	/**URI input stream locator constructor.
	@param uriInputStreamable The implementation to use for accessing a URI for
		input, or <code>null</code> if the default implementation should be used.
	*/
	public URIAccessibleModel(final URIInputStreamable uriInputStreamable)
	{
		this(null, uriInputStreamable);
	}

	/**Full constructor.
	@param baseURI The base URI of the model, or <code>null</code> if unknown.
	@param uriInputStreamable The implementation to use for accessing a URI for
		input, or <code>null</code> if the default implementation should be used.
	 */
	public URIAccessibleModel(final URI baseURI, final URIInputStreamable uriInputStreamable)
	{
		this.baseURI=baseURI;	//save the base URI
		this.uriInputStreamable=uriInputStreamable!=null ? uriInputStreamable : this;	//save the URI input stream locator, using our default if one was not given
	}

	/**Returns an input stream for the given URI.
	The calling class has the responsibility for closing the input stream.
	@param uri A URI to a resource.
	@return An input stream to the contents of the resource represented by the given URI.
	@exception IOException Thrown if an I/O error occurred.
	*/
	public InputStream getInputStream(final URI uri) throws IOException
	{
		return uri.toURL().openConnection().getInputStream();	//TODO this is used a lot---put in some generic location
	}
}
