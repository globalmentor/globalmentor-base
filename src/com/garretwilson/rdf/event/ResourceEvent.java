package com.garretwilson.rdf.event;

import java.net.URI;
import java.util.EventObject;
import com.garretwilson.rdf.Resource;

/**An event related to a resource.
@author Garret Wilson
*/
public class ResourceEvent extends EventObject
{

	/**The previous resource reference URI, or <code>null</code> if not applicable.*/
	private final URI oldResourceURI;

		/**@return The previous resource reference URI, or <code>null</code> if not applicable.*/
		public URI getOldResourceURI() {return oldResourceURI;}

	/**A description of the old resource, or <code>null</code> if not applicable.*/
	private final Resource oldResource;

		/**@return A description of the old resource, or <code>null</code> if not applicable.*/
		public Resource getOldResource() {return resource;}

	/**The resource reference URI, or <code>null</code> if not applicable.*/
	private final URI resourceURI;

		/**@return The resource reference URI, or <code>null</code> if not applicable.*/
		public URI getResourceURI() {return resourceURI;}


	/**A description of the resource, or <code>null</code> if there is no description.*/
	private final Resource resource;

		/**@return A description of the resource, or <code>null</code> if there is no description.*/
		public Resource getResource() {return resource;}

	/**Constructor that specifies the resource reference URI.
	@param source The object on which the event initially occurred.
	@param resourceURI The reference URI of the resource.
	*/
  public ResourceEvent(final Object source, final URI resourceURI)
  {
		this(source, null, null, resourceURI, null);	//construct the class with the resource reference URI
  }

	/**Constructor that specifies both an old and a current resource reference URI.
	@param source The object on which the event initially occurred.
	@param oldResourceURI The old reference URI of the resource.
	@param newResourceURI The new reference URI of the resource.
	*/
	public ResourceEvent(final Object source, final URI oldResourceURI, final URI newResourceURI)
	{
		this(source, oldResourceURI, null, newResourceURI, null);	//construct the class with old and new URIs
	}

	/**Constructor that specifies the resource description.
	@param source The object on which the event initially occurred.
	@param resource A description of the resource, or <code>null</code> if
		there is no description.
	*/
  public ResourceEvent(final Object source, final Resource resource)
  {
  	this(source, null, null, resource.getReferenceURI(), resource);	//construct the class with a resource and a reference URI
  }

	/**Constructor that specifies both an old and a current resource reference URI.
	@param source The object on which the event initially occurred.
	@param oldResourceURI The old reference URI of the resource, or
		<code>null</code> if there is no old resource referene URI.
	@param The old resource, or <code>null</code> if there is no old resource.
	@param resourceURI The current reference URI of the resource.
	@param The current resource, or <code>null</code> if there is no resource.
	*/
	protected ResourceEvent(final Object source, final URI oldResourceURI, final Resource oldResource, final URI resourceURI, final Resource resource)
	{
		super(source);  //construct the parent class
		this.oldResourceURI=oldResourceURI;	//save the old resource URI
		this.oldResource=oldResource;	//save the old resource
		this.resourceURI=resourceURI;	//save the resource URI
		this.resource=resource; //save the resource description
	}

}