package com.garretwilson.rdf.event;

import java.util.EventObject;
import com.garretwilson.rdf.RDFResource;

/**An event related to a resource.
@author Garret Wilson
*/
public class ResourceEvent extends EventObject
{

	/***Convenience method to return the resource URI.
	@return The reference URI, the source cast to a URI.
	*/
	public String getReferenceURI() {return (String)getSource();}

	/**A description of the resource, or <code>null</code> if there is no description.*/
	private final RDFResource resource;

		/**@return A description of the resource, or <code>null</code> if there is no description.*/
		public RDFResource getResource() {return resource;}

	/**Constructor that specifies the resource URI.
	@param referenceURI the reference URI of the resource.
	*/
  public ResourceEvent(final String referenceURI)
  {
		this(referenceURI, null); //construct the event with no description
  }

	/**Constructor that specifies the resource URI and resource description.
	@param referenceURI the reference URI of the resource.
	@param rdfResource A description of the resource, or <code>null</code> if
		there is no description.
	*/
  public ResourceEvent(final String referenceURI, final RDFResource rdfResource)
  {
		super(referenceURI);  //construct the parent class
		resource=rdfResource; //save the resource description
  }

}