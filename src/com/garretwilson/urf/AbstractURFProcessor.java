package com.garretwilson.urf;

import java.net.URI;
import java.util.*;
import static java.util.Collections.*;

import static com.garretwilson.lang.ObjectUtilities.*;

import com.garretwilson.io.ParseIOException;
import com.garretwilson.net.DefaultResource;
import com.garretwilson.net.Resource;
import static com.garretwilson.urf.URF.*;

import com.garretwilson.util.Debug;
import com.garretwilson.util.IdentityHashSet;

/**Base class for URF processors.
Each instance of an URF processor maintains an internal URF data model throughout its lifetime that is continually updated with every new URF processing that occurs.
<p>The URF processor maintains URF data in two separate formats: the URF data model {@link URF}, as well as a list of assertions used to create	the data model.
The URF data model may be replaced and its members modified, but these actions will not update the list of URF assertions.
The URF assertions are only generated by the URF processor itself as it parses URF serializations, and are available to give information on the parser actions.</p>
<p>An URF processor takes the following steps:</p>
<ol>
	<li>All typed resources are created and stored in the URF data model and in statements.</li>
	<li>After processing is ended, all untyped subjects and predicates that have not yet been created are created, preferably by using any type that has become available through processing the URF instance.</li>
	<li>All statements are processed, connecting the resources and their properties.</li>
</ol>
<p>This class is not thread-safe.</p>
@author Garret Wilson
*/
public abstract class AbstractURFProcessor
{

	/**The URF data model that is filled by the processor.*/
	private URF urf;

		/**@return The URF data model being constructed by the URF processor.*/
		public URF getURF() {return urf;}

		/**Sets the URF data model.
		The list of assertions is reset.
		@param urf The URF data model to use.
		@exception NullPointerException if the given URF data model is <code>null</code>.
		*/
		public void setURF(final URF urf)
		{
			this.urf=checkInstance(urf, "URF cannot be null."); //set the URF data model
			assertions.clear();  //clear the set of assertions
		}

	/**The base URI of the URF being processed, or <code>null</code> if no base URI was specified.*/
//TODO fix	private URI baseURI=URI.create("base:/");	//TODO use a constant

		/**Sets the base URI for the RDF being processed.
		@param newBaseURI The base URI of the RDF being processed, or <code>null</code> if no base URI was specified.
		*/
/*TODO fix
		public void setBaseURI(final URI newBaseURI)
		{
			baseURI=newBaseURI; //set the base URI
		}
*/

		/**@return The base URI of the RDF being processed, or "online:" if no
			base URI is known.
		*/
/*TODO fix
		public URI getBaseURI()
		{
			return baseURI!=null ? baseURI : URI.create("online:/");	//return the base URI if we know it	//TODO use a constant
		}
*/

	/**The map of resource proxies keyed to URIs.*/
	private final Map<URI, ResourceProxy> uriResourceProxyMap=new HashMap<URI, ResourceProxy>();

	/**The map of resource proxies keyed to labels.*/
	private final Map<String, ResourceProxy> labelResourceProxyMap=new HashMap<String, ResourceProxy>();

		/**Retrieves a resource proxy to represent a resource with the given label.
		If such a proxy already exists, it will be returned; otherwise, a new one will be created.
		If no label is given, a new resource proxy will be created and returned.
		@param label The label used to identify the resource, or <code>null</code> if no label is known.
		@return A resource proxy to represent the identified resource.
		*/
		protected ResourceProxy getResourceProxy(final String label) throws ParseIOException
		{
			return getResourceProxy(label, null);	//get a resource proxy with no URI			
		}

		/**Retrieves a resource proxy to represent a resource with the given URI.
		If such a proxy already exists, it will be returned; otherwise, a new one will be created.
		If no URI is given, a new resource proxy will be created and returned.
		@param resourceURI The URIs of the resource the proxy should represent, or <code>null</code> if a resource URI is not known.
		@return A resource proxy to represent the identified resource.
		*/
		protected ResourceProxy getResourceProxy(final URI resourceURI) throws ParseIOException
		{
			return getResourceProxy(null, resourceURI);	//get a resource proxy with no label
		}

		/**Retrieves a resource proxy to represent a resource with the given label and/or URI.
		If such a proxy already exists, it will be returned; otherwise, a new one will be created.
		If neither a label nor a URI is given, a new resource proxy will be created and returned.
		@param label The label used to identify the resource, or <code>null</code> if no label is known.
		@param resourceURI The URIs of the resource the proxy should represent, or <code>null</code> if a resource URI is not known.
		@return A resource proxy to represent the identified resource.
		@exception ParseIOException if the same label has been used resources with different URIs.
		*/
		protected ResourceProxy getResourceProxy(final String label, final URI resourceURI) throws ParseIOException
		{
Debug.trace("getting proxy for label", label, "resource URI", resourceURI);
			final Set<ResourceProxy> equivalentResourceProxies=new IdentityHashSet<ResourceProxy>();	//create a set in which to store the resource proxies
			if(label!=null)	//if we were given a label
			{
				final ResourceProxy resourceProxy=labelResourceProxyMap.get(label);	//see if there is a proxy associated with the label
				if(resourceProxy!=null)	//if there is a label resource proxy
				{
Debug.trace("found existing resource proxy for label", label, resourceProxy);
					equivalentResourceProxies.add(resourceProxy);	//add this resource proxy
				}
			}
			if(resourceURI!=null)	//if we were given a URI
			{
				final ResourceProxy resourceProxy=uriResourceProxyMap.get(resourceURI);	//see if there is a resource proxy associated with the URI
				if(resourceProxy!=null)	//if there is a URI resource proxy
				{
					equivalentResourceProxies.add(resourceProxy);	//add this resource proxy
				}
			}
//Debug.trace("got initial proxies");
			if(equivalentResourceProxies.isEmpty())	//if we found no resource proxies
			{
				equivalentResourceProxies.add(new ResourceProxy(resourceURI));	//put a new resource proxy in the map, using the given resource URI if any
			}
			else	//if there is at least one resource proxy
			{
				for(final ResourceProxy equivalentResourceProxy:equivalentResourceProxies)	//for each equivalent resource proxy
				{
//Debug.trace("existing proxy:", equivalentResourceProxy.getURI());
					gatherEquivalentResourceProxies(equivalentResourceProxy.getEquivalentResourceProxies(), equivalentResourceProxies);	//gather all equivalent resource proxies
				}
			}
//Debug.trace("ready to infom proxies; we have", equivalentResourceProxies.size());
			URI equivalentURI=resourceURI;	//if no URI was passed, see if one of the resource proxies has a URI
			for(final ResourceProxy equivalentResourceProxy:equivalentResourceProxies)	//for each equivalent resource proxy
			{
//Debug.trace("informing proxy:", equivalentResourceProxy.getURI());
				equivalentResourceProxy.getEquivalentResourceProxies().addAll(equivalentResourceProxies);	//make sure each equivalent resource proxy knows about all the others
				if(equivalentURI==null)	//if we don't know of any URI, yet,
				{
					equivalentURI=equivalentResourceProxy.getURI();	//get the URI of this resource proxy, in case it knows of a resource URI
				}
			}
			if(equivalentURI!=null)	//if we know of a resource URI, make sure all the equivalent resource proxies know about that URI
			{
				//Debug.trace("double-checking proxy:", equivalentResourceProxy.getURI());
				for(final ResourceProxy equivalentResourceProxy:equivalentResourceProxies)	//for each equivalent resource proxy
				{
					final URI existingURI=equivalentResourceProxy.getURI();	//see what URI this resource proxy has
					if(existingURI!=null && !equivalentURI.equals(existingURI))	//if this equivalent resource proxy has a different URI
					{
						throw new ParseIOException("Two resources with the same label have different URIs: "+equivalentURI+" and "+existingURI);
					}
					equivalentResourceProxy.setURI(equivalentURI);	//make sure this resource knows about the URI
				}
			}
//Debug.trace("ready to return proxies");
			final ResourceProxy resourceProxy=equivalentResourceProxies.iterator().next();	//get one of (any of) the equivalent resource proxies
//Debug.trace("we'll return proxy with URI:", resourceProxy.getURI());
			if(label!=null)	//if a label was given
			{
				labelResourceProxyMap.put(label, resourceProxy);	//associate the resource proxy with the label
			}
			if(equivalentURI!=null)	//if we know of a URI
			{
				uriResourceProxyMap.put(equivalentURI, resourceProxy);	//associate this proxy with the URI
			}
			return equivalentResourceProxies.iterator().next();	//return one of (any of) the equivalent resource proxies
		}

	//TODO comment
	protected static void gatherEquivalentResourceProxies(final Set<ResourceProxy> resourceProxies, final Set<ResourceProxy> equivalentResourceProxies)
	{
		for(final ResourceProxy resourceProxy:resourceProxies)	//look at each resource proxy
		{
			if(equivalentResourceProxies.add(resourceProxy))	//add this resource proxy; if we don't yet have this resource proxy
			{
				gatherEquivalentResourceProxies(resourceProxy.getEquivalentResourceProxies(), equivalentResourceProxies);	//add this resource proxy's equivalent resource proxies
			}
		}
	}
		
	/**The map of URF resources keyed to resource proxies.*/ 
	private final Map<ResourceProxy, URFResource> proxiedURFResourceMap=new HashMap<ResourceProxy, URFResource>();

		/**Retrieves a resource associated with the given resource proxy, if any.
		@param resourceProxy The object standing in for the resource.
		@return The resource associated with the given resource proxy, or <code>null</code> if there are no resources associated with the given resource proxy.
		*/
		protected URFResource getProxiedURFResource(final ResourceProxy resourceProxy)
		{
			return proxiedURFResourceMap.get(resourceProxy); //get any resource associated with the resource proxy
		}

		/**Stores a reference to a resource keyed to a resource proxy.
		@param resourceProxy The object standing in for the resource.
		@param resource The resource the proxy represents.
		*/
		protected void putProxiedURFResource(final ResourceProxy resourceProxy, final URFResource resource)
		{
			proxiedURFResourceMap.put(resourceProxy, resource);	//store the resource keyed to the resource proxy
		}

	/**The next number to use when generating node IDs.*/
//TODO fix	private long nextNodeIDTag=1;

	/**@return A unique node ID appropriate for a new node.*/
/*TODO fix
	protected String generateNodeID()
	{
		return AbstractURFProcessor.class.getName()+JavaConstants.PACKAGE_SEPARATOR+"nodeID"+(nextNodeIDTag++);	//use the next node ID tag and increments TODO use a constant
	}
*/

	/**The set of all assertions used to create the resources.*/
	private final Set<Assertion> assertions=new HashSet<Assertion>();

	/**@return A read-only iterable of all statements collected and processed by the processor.*/
	public Iterable<Assertion> getAssertions()
	{
		return unmodifiableSet(assertions);  //create an unmodifiable set for the assertion set and return it
	}

	/**Adds an assertion to the set of assertions.
	If an equivalent assertion already exists in the list, no action occurs.
	@param assertion The assertion to add.
	*/
	protected void addAssertion(final Assertion assertion)
	{
	  assertions.add(assertion); //add the assertion to the set
	}

	/**Clear all collected URF assertions.*/
	public void clearAssertions()
	{
		assertions.clear();	//clear the set of statements
	}

	/**Default constructor.*/
	public AbstractURFProcessor()
	{
		this(new URF());  //create an URF data model to use
	}

	/**Constructor that specifies an existing data model to continue filling.
	@param urf The URF data model to use.
	*/
	public AbstractURFProcessor(final URF urf)
	{
		setURF(urf);  //set the URF data model
	}

	/**Resets the processor by clearing all temporary references and maps, such as associations between resource proxies and URF resources.
	The set of assertions is left undisturbed.
	*/
	public void reset()
	{
		uriResourceProxyMap.clear();	//clear our map of resource proxies keyed to reference URIs
//TODO fix		nodeIDResourceProxyMap.clear();	//clear our map of resource proxies keyed to node IDs
		proxiedURFResourceMap.clear();	//clear our map of URF resources keyed to resource proxies
//TODO fix		nextNodeIDTag=1;	//reset our counter that keeps track of the next node ID to use
	}

	/**Iterates through all collected statements and, for any resources in the statement that are only proxies for URF resources,
	creates appropriate resources, using any provided types in other statements if possible.
	@see #ResourceProxy
	*/
	public void createResources()
	{
		createResources(null);	//create resources without keeping track of any resource in particular
	}

	/**Iterates through all collected assertions and, for any resources in the statement that are only proxies for URF resources,
	creates appropriate resources, using any provided types in other statements if possible.
	The final created URF resource of the given resource is returned.
	@param resource The resource, either an URF resource or a proxy, for which an unproxied URF resource should be returned, or <code>null</code> if no specific resource should be returned.
	@return The created resource from the given resource proxy, or the given resource itself if the resource is already an URF resource.
	@exception IllegalArgumentException if a resource was given that is not one of the resources in the list of URF statements.
	@see #ResourceProxy
	*/
	public URFResource createResources(final Resource resource)	//TODO maybe check to make sure any URFResource passed to us is really one of the ones in the list of statements
	{
		URFResource urfResource=resource instanceof URFResource ? (URFResource)resource : null;	//if the given resource is already an URF resource, there's nothing to unproxy
		for(final Assertion assertion:getAssertions())	//for each assertion
		{
			final Resource subject=assertion.getSubject();	//get the assertion subject
			if(subject instanceof ResourceProxy)	//if the subject is just a proxy
			{
					//unproxy the resource by either retrieving an already-created resource or creating a new one
				final URFResource subjectURFResource=unproxyURFResource((ResourceProxy)subject);
				assertion.setSubject(subjectURFResource);	//update the subject to the unproxied URF resource
				if(subject==resource)	//if the subject is the resource we are supposed to keep track of
				{
					urfResource=subjectURFResource;	//show that we now have an URF resource for the proxy
				}
			}
			final Resource predicate=assertion.getPredicate();	//get the assertion predicate
			if(predicate instanceof ResourceProxy)	//if the predicate is just a proxy
			{
					//unproxy the resource by either retrieving an already-created resource or creating a new one
				final URFResource predicateURFResource=unproxyURFResource((ResourceProxy)predicate);
				assertion.setPredicate(predicateURFResource); //update the predicate to the unproxied URF resource
				if(predicate==resource)	//if the predicate is the resource we are supposed to keep track of
				{
					urfResource=predicateURFResource;	//show that we now have an URF resource for the proxy
				}
			}
			final Resource object=assertion.getObject();	//get the assertion object
			if(object instanceof ResourceProxy)	//if the object is just a proxy
			{
					//unproxy the resource by either retrieving an already-created resource or creating a new one
				final URFResource objectURFResource=unproxyURFResource((ResourceProxy)object);
				assertion.setObject(objectURFResource);	//update the object to the unproxied URF resource
				if(object==resource)	//if the object is the resource we are supposed to keep track of
				{
					urfResource=objectURFResource;	//show that we now have an URF resource for the proxy
				}
			}
		}
		if(resource!=null && urfResource==null)	//if we were unable to find a suitable URF resource for a valid given resource, the given resource proxy must have not been in the list of assertions
		{
			throw new IllegalArgumentException("Resource "+resource+" unknown in list of statements.");
		}
		return urfResource;	//return the resource we unproxied if needed
	}

	/**For the given resource proxy, returns the existing URF resource the proxy represents or, if there is no target, creates a new URF resource for the proxy.
	If the latter, the assertions are iterated to determine an appropriate type for the new resource, if possible.
	@param resourceProxy The resource that represents the URF resource by reference URI or node ID.
	@return An URF resource representing the given proxy.
	@exception NullPointerException if the given resource proxy is <code>null</code>.
	*/
	protected URFResource unproxyURFResource(final ResourceProxy resourceProxy)
	{
		final URI resourceURI=resourceProxy.getURI();	//get the URI, if any, to use for the resource
		URFResource resource=null;	//we'll see if we alrady have an unproxied resource
		for(final ResourceProxy equiavalentResourceProxy:resourceProxy.getEquivalentResourceProxies())	//for each equivalent resource proxy
		{
			resource=getProxiedURFResource(resourceProxy);	//see if we already have a resource represented by the proxy
			if(resource!=null)	//if we found a resource
			{
				break;	//we already have an unproxied resource
			}
		}
		if(resource==null)	//if we have no such resource, create one; first, look for an appropriate type
		{
			final URF urf=getURF();	//get the URF data model
			for(final Assertion assertion:getAssertions())	//for each assertion
			{
					//if this is an assertion in the form, {resource proxy, urf:type, XXX}
				if(assertion.getSubject().equals(resourceProxy)	//if this assertion has this resource proxy as its subject
						&& TYPE_PROPERTY_URI.equals(assertion.getPredicate().getURI()))	//if this assertion has a predicate of urf:type
				{
						//TODO check for a contextual property
					final URFResource typeValueURFResource;	//we'll find a resource to use as the type value
					final Resource typeValueResource=assertion.getObject();	//get the type value
					if(typeValueResource instanceof URFResource)	//if the type value is already an URF resource
					{
						typeValueURFResource=(URFResource)typeValueResource;	//use the type value already in place
					}
					else if(typeValueResource instanceof ResourceProxy)	//if the type value is only resource proxy
					{
						typeValueURFResource=unproxyURFResource((ResourceProxy)typeValueResource);	//unproxy the type value (note that this will not replace the proxy in the assertion, but it will create the resource and associate it with the proxy so that when it does come time to replace the proxy, it will already be there)
					}
					else	//if we don't recognize the value
					{
						throw new AssertionError("Unrecognized assertion object type: "+typeValueResource.getClass());
					}
					final URI typeURI=typeValueURFResource.getURI();	//get the type URI
					if(typeURI!=null)	//if we know the type value
					{
						resource=urf.locateResource(resourceURI, typeURI);	//create this typed resource
						break;	//stop looking at assertions
					}
				}
			}
			if(resource==null)	//if we couldn't create a resource from an assertion that provided the type
			{
				resource=urf.locateResource(resourceURI);	//locate a default resource
			}
		}
		for(final ResourceProxy equiavalentResourceProxy:resourceProxy.getEquivalentResourceProxies())	//for each equivalent resource proxy
		{
			putProxiedURFResource(equiavalentResourceProxy, resource);	//associate the resource with the resource proxy so we won't have to go through all this the next time
		}
		return resource;	//return the resource that was either already the target of the proxy, or that we created and associated with the proxy
	}

	/**Processes all assertions by creating associations between resources that reflect the assertions contained in the assertions.*/
	public void processAssertions()
	{
//TODO del Debug.trace("ready to process assertions");
		for(final Assertion assertion:getAssertions())	//for each assertion
		{
		//TODO del Debug.trace("here's a assertion:", assertion);
			final Resource subject=assertion.getSubject();	//get the assertion subject
			final Resource predicate=assertion.getPredicate();	//get the assertion predicate
			final Resource object=assertion.getObject();	//get the assertion object
				//if the subject, predicate, and object of the assertion are URF resources
			if(subject instanceof URFResource && predicate instanceof URFResource && object instanceof URFResource)
			{
				final URFResource urfSubject=(URFResource)subject;	//cast the subject to an URF resource
				final URFResource urfPredicate=(URFResource)predicate;	//cast the predicate to an URF resource
				final URFResource urfObject=(URFResource)object;	//cast the object to an URF resource
//TODO fix to recognize contextual properties
				final URI predicateURI=urfPredicate.getURI();	//get the predicate URI
				if(predicateURI!=null)	//if there is a predicate URI TODO do we want to allow anonymous predicates?
				{
					//TODO del Debug.trace("ready to add predicate to subject:", urfSubject);
					urfSubject.addPropertyValue(urfPredicate.getURI(), urfObject);	//process this assertion by adding the predicate and object to the subject as a property
				}
			}
		}		
	}

	/**A class which represents an URF resource and that will eventually be replaced with an URF resource. 
	@author Garret Wilson
	*/
	protected static class ResourceProxy extends DefaultResource 
	{

		/**The set of all resource proxies, including this one, to which this resource proxy is equivalent.*/
		private final Set<ResourceProxy> equivalentResourceProxies=new IdentityHashSet<ResourceProxy>();

			/**@return The set of all resource proxies, including this one, to which this resource proxy is equivalent.*/
			public Set<ResourceProxy> getEquivalentResourceProxies() {return equivalentResourceProxies;}

		/**URI constructor.
		@param uri The URI for the new resource.
		This resource proxy will be added to the set of resource proxies.
		*/
		protected ResourceProxy(final URI uri)
		{
			getEquivalentResourceProxies().add(this);	//add ourselves to the set of equivalent resource proxies
		}

	}

	/**An URF assertion used for processing.
	@author Garret Wilson
	*/
	public static class Assertion
	{

		/**The assertion subject.*/
		private Resource subject;

			/**@return The assertion subject.*/
			public Resource getSubject() {return subject;}

			/**Sets the assertion subject.
			@param subject The assertion subject.
			@exception NullPointerException if the given subject is <code>null</code>.
			*/
			private void setSubject(final Resource subject) {this.subject=checkInstance(subject, "Subject cannot be null.");}

		/**The assertion predicate.*/
		private Resource predicate;

			/**@return The assertion predicate.*/
			public Resource getPredicate() {return predicate;}

			/**Sets the assertion predicate.
			@param predicate The assertion predicate.
			@exception NullPointerException if the given predicate is <code>null</code>.
			*/
			private void setPredicate(final Resource predicate) {this.predicate=checkInstance(predicate, "Predicate cannot be null.");}

		/**The assertion object.*/
		private Resource object;

			/**@return The assertion object.*/
			public Resource getObject() {return object;}

			/**Sets the assertion object.
			@param object The assertion object.
			@exception NullPointerException if the given object is <code>null</code>.
			*/
			void setObject(final Resource object) {this.object=checkInstance(object, "Object cannot be null.");}

		/**Creates a new assertion from a subject, predicate, and object resource.
		@param subject The subject of the assertion.
		@param predicate The predicate of the assertion.
		@param object The object of the assertion.
		@exception NullPointerException if the given subject, predicate, and/or object is <code>null</code>.
		*/
		public Assertion(final Resource subject, final Resource predicate, final Resource object)
		{
			setSubject(subject); //set the subject
			setPredicate(predicate); //set the predicate
			setObject(object); //set the object
		}

//TODO fix and improve all this
		
		/**@return A hash code value for the assertion.
		@see #getSubject()
		@see #getPredicate()
		@see #getObject()
		*/
		public int hashCode()
		{
			final StringBuffer stringBuffer=new StringBuffer();	//create a string buffer to store string versions of the parts of the assertion
			final Resource subject=getSubject();	//get the subject
			if(subject!=null)	//if there is a subject
			{
				stringBuffer.append(subject);	//append the subject
			}
			final Resource predicate=getPredicate();	//get the predictae
			if(getPredicate()!=null)	//if there is a predicate
			{
				stringBuffer.append(predicate);	//append the predicate
			}
			final Object object=getObject();	//get the object
			if(object!=null)	//if there is an object
			{
				stringBuffer.append(object);	//append the object
			}
			return stringBuffer.toString().hashCode();	//return the hash code of all three parts of the assertion as a string (StringBuffer apparently doesn't provide a specialized hashCode() implementation)
		}

		/**Compares assertions based upon subject, predicate, and object.
		@param object The object with which to compare this assertion; should be another assertion.
		@return <code>true<code> if the subjects, predicates, and objects of the two assertions are equal.
		*/
		public boolean equals(final Object object)
		{
			if(object instanceof Assertion)	//if the other object is a assertion
			{
				final Assertion assertion2=(Assertion)object;	//cast the object to a assertion
				return getSubject().equals(assertion2.getSubject())	//compare subjects
						&& getPredicate().equals(assertion2.getPredicate())	//compare predicates
						&& getObject().equals(assertion2.getObject());	//compare objects
			}
			return false;	//show that the assertions do not match
		}

		/**@return A string representation of the assertion in the form:
		"{subject, predicate, object}".
		*/
		public String toString()
		{
			final StringBuilder stringBuilder=new StringBuilder(); //create a new string builder
			stringBuilder.append('{');
			stringBuilder.append('[').append(getSubject()).append(']');
			stringBuilder.append(',').append(' ');
			stringBuilder.append('[').append(getPredicate()).append(']');
			stringBuilder.append(',').append(' ');
			stringBuilder.append('[').append(getObject()).append(']');  //[resource]
			stringBuilder.append('}');
			return stringBuilder.toString(); //return the string we just constructed
		}

	}
}