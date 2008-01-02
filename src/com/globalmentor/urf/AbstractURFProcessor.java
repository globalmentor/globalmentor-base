package com.globalmentor.urf;

import java.net.URI;
import java.util.*;
import static java.util.Collections.*;
import java.util.concurrent.atomic.AtomicLong;

import com.garretwilson.net.*;

import static com.globalmentor.java.Objects.*;
import static com.globalmentor.urf.URF.*;

import com.garretwilson.util.*;
import com.globalmentor.java.*;

/**Base class for URF processors.
Each instance of an URF processor maintains an internal URF data model throughout its lifetime that is continually updated with every new URF processing that occurs.
<p>The URF processor maintains URF data in two separate formats: the URF data model {@link URF}, as well as a list of assertions used to create	the data model.
The assertions are maintained in the order in which they were made, except that assertions with shorter scope chains are always presented before those with longer scope chains.
The URF data model may be replaced and its members modified, but these actions will not update the list of URF assertions.
The URF assertions are only generated by the URF processor itself as it parses URF serializations, and are available to give information on the parser actions.</p>
<p>An URF processor takes the following steps:</p>
<ol>
	<li>All typed resources are created and stored in the URF data model and in statements.</li>
	<li>After processing is ended, all untyped subjects and predicates that have not yet been created are created, preferably by using any type that has become available through processing the URF instance.</li>
	<li>All statements are processed, connecting the resources and their properties.</li>
</ol>
<p>This class is not thread-safe.</p>
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
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

	/**The map of resource proxies keyed to URIs.*/
	private final Map<URI, ResourceProxy> uriResourceProxyMap=new HashMap<URI, ResourceProxy>();

		/**Retrieves the resource proxy that represents a resource with the given URI.
		@param resourceURI The URI of the resource the proxy should represent.
		@return The resource proxy used to represent the identified resource, or <code>null</code> if there is no resource proxy for the given URI.
		@exception NullPointerException if the given resource URI is <code>null</code>.
		@exception DataException if the same label has been used resources with different URIs.
		*/
		protected ResourceProxy getResourceProxy(final URI resourceURI) throws DataException
		{
			return uriResourceProxyMap.get(checkInstance(resourceURI, "Resource URI cannot be null."));	//look up the resource proxy
		}

	/**The map of resource proxies keyed to labels.*/
	private final Map<String, ResourceProxy> labelResourceProxyMap=new HashMap<String, ResourceProxy>();

		/**Creates a new anonymous resource proxy.
		@return A new anonymous resource proxy.
		*/
		protected ResourceProxy createResourceProxy() throws DataException
		{
			return determineResourceProxy((URI)null);	//determine a new resource proxy with no URI
		}

		/**Retrieves the resource proxy that represents a resource with the given label.
		@param label The label used to identify the resource.
		@return The resource proxy used to represent the identified resource with the given label, or <code>null</code> if there is no resource proxy for the given label.
		@exception NullPointerException if the given label is <code>null</code>.
		@exception DataException if the same label has been used resources with different URIs.
		*/
		protected ResourceProxy getResourceProxy(final String label) throws DataException
		{
			return labelResourceProxyMap.get(checkInstance(label, "Label cannot be null."));	//look up the resource proxy
		}

		/**Retrieves a resource proxy to represent a resource with the given label.
		If such a proxy already exists, it will be returned; otherwise, a new one will be created.
		If no label is given, a new resource proxy will be created and returned.
		@param label The label used to identify the resource, or <code>null</code> if no label is known.
		@return A resource proxy to represent the identified resource.
		*/
		protected ResourceProxy determineResourceProxy(final String label) throws DataException
		{
			return determineResourceProxy(label, null);	//get a resource proxy with no URI
		}

		/**Retrieves a resource proxy to represent a resource with the given URI.
		If such a proxy already exists, it will be returned; otherwise, a new one will be created.
		If no URI is given, a new resource proxy will be created and returned.
		@param resourceURI The URI of the resource the proxy should represent, or <code>null</code> if a resource URI is not known.
		@return A resource proxy to represent the identified resource.
		@exception DataException if the same label has been used resources with different URIs.
		*/
		protected ResourceProxy determineResourceProxy(final URI resourceURI) throws DataException
		{
			return determineResourceProxy(null, resourceURI);	//get a resource proxy with no label
		}

		/**Retrieves a resource proxy to represent a resource with the given label and/or URI.
		If such a proxy already exists, it will be returned; otherwise, a new one will be created.
		If neither a label nor a URI is given, a new resource proxy will be created and returned.
		@param label The label used to identify the resource, or <code>null</code> if no label is known.
		@param resourceURI The URIs of the resource the proxy should represent, or <code>null</code> if a resource URI is not known.
		@return A resource proxy to represent the identified resource.
		@exception DataException if the same label has been used resources with different URIs.
		*/
		protected ResourceProxy determineResourceProxy(final String label, final URI resourceURI) throws DataException
		{
			final Set<ResourceProxy> equivalentResourceProxies=new IdentityHashSet<ResourceProxy>();	//create a set in which to store the resource proxies
			if(label!=null)	//if we were given a label
			{
				final ResourceProxy resourceProxy=labelResourceProxyMap.get(label);	//see if there is a proxy associated with the label
				if(resourceProxy!=null)	//if there is a label resource proxy
				{
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
			if(equivalentResourceProxies.isEmpty())	//if we found no resource proxies
			{
				equivalentResourceProxies.add(new ResourceProxy(resourceURI));	//put a new resource proxy in the map, using the given resource URI if any
			}
			else	//if there is at least one resource proxy
			{
				for(final ResourceProxy equivalentResourceProxy:equivalentResourceProxies)	//for each equivalent resource proxy
				{
					gatherEquivalentResourceProxies(equivalentResourceProxy.getEquivalentResourceProxies(), equivalentResourceProxies);	//gather all equivalent resource proxies
				}
			}
			URI equivalentURI=resourceURI;	//if no URI was passed, see if one of the resource proxies has a URI
			for(final ResourceProxy equivalentResourceProxy:equivalentResourceProxies)	//for each equivalent resource proxy
			{
				equivalentResourceProxy.getEquivalentResourceProxies().addAll(equivalentResourceProxies);	//make sure each equivalent resource proxy knows about all the others
				if(equivalentURI==null)	//if we don't know of any URI, yet,
				{
					equivalentURI=equivalentResourceProxy.getURI();	//get the URI of this resource proxy, in case it knows of a resource URI
				}
			}
			if(equivalentURI!=null)	//if we know of a resource URI, make sure all the equivalent resource proxies know about that URI
			{
				for(final ResourceProxy equivalentResourceProxy:equivalentResourceProxies)	//for each equivalent resource proxy
				{
					final URI existingURI=equivalentResourceProxy.getURI();	//see what URI this resource proxy has
					if(existingURI!=null && !equivalentURI.equals(existingURI))	//if this equivalent resource proxy has a different URI
					{
						throw new DataException("Two resources with the same label have different URIs: "+equivalentURI+" and "+existingURI);
					}
					equivalentResourceProxy.setURI(equivalentURI);	//make sure this resource knows about the URI
				}
			}
			final ResourceProxy resourceProxy=equivalentResourceProxies.iterator().next();	//get one of (any of) the equivalent resource proxies
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

	/**Recursively gathers all resource proxies that are linked to these proxies as equivalent proxies.
	@param resourceProxies The resource proxies to recursively search.
	@param equivalentResourceProxies The accumulated set of equivalent resource proxies.
	@see ResourceProxy#getEquivalentResourceProxies()
	*/
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

	/**The atomic variable used to generate assertion orders.*/
	private final AtomicLong assertionOrder=new AtomicLong(0);

		/**Generates a new assertion order unique to this JVM.
		@return A new assertion order unique to this JVM.
		*/
		protected long generateAssertionOrder()
		{
			return assertionOrder.getAndIncrement();	//atomically get the next counter value
		}

	/**The set of all assertions used to create the resources.*/
	private final Set<Assertion> assertions=new TreeSet<Assertion>();

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
		labelResourceProxyMap.clear();	//clear our map of resource proxies keyed to labels
		proxiedURFResourceMap.clear();	//clear our map of URF resources keyed to resource proxies
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
			//TODO don't forget to go through all the resource proxies that were not used in any assertions
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
			final NameValuePair<Resource, Resource>[] scopeChain=assertion.getScopeChain();	//get the assertion scope chain
			for(int i=scopeChain.length-1; i>=0; --i)	//for each property value pair in the scope chain
			{
				final NameValuePair<Resource, Resource> scopePair=scopeChain[i];	//get this scope property value pair
				boolean replace=false;	//we'll determine if we need to replace this scope property/value pair
				Resource scopeProperty=scopePair.getName();	//get the property of this element of the scope chain
				if(scopeProperty instanceof ResourceProxy)	//if this scope property is just a proxy
				{
					replace=true;	//indicate that we'll need to replace the scope pair
					final URFResource scopePropertyURFResource=unproxyURFResource((ResourceProxy)scopeProperty);	//unproxy the resource by either retrieving an already-created resource or creating a new one
					if(scopeProperty==resource)	//if the scope property value is the resource we are supposed to keep track of
					{
						urfResource=scopePropertyURFResource;	//show that we now have an URF resource for the proxy
					}
					scopeProperty=scopePropertyURFResource;	//update the scope property
				}
				Resource scopePropertyValue=scopePair.getValue();	//get the property value of this element of the scope chain
				if(scopePropertyValue instanceof ResourceProxy)	//if this scope property value is just a proxy
				{
					replace=true;	//indicate that we'll need to replace the scope pair
					final URFResource scopePropertyValueURFResource=unproxyURFResource((ResourceProxy)scopePropertyValue);	//unproxy the resource by either retrieving an already-created resource or creating a new one
					if(scopePropertyValue==resource)	//if the scope property value is the resource we are supposed to keep track of
					{
						urfResource=scopePropertyValueURFResource;	//show that we now have an URF resource for the proxy
					}
					scopePropertyValue=scopePropertyValueURFResource;	//update the scope property value
				}
				if(replace)	//if we should replace the scope pair
				{
					scopeChain[i]=new NameValuePair<Resource, Resource>(scopeProperty, scopePropertyValue);	//update the scopy property and scope property value to the unproxied URF resource versions
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
		if(resource==null)	//if we have no such resource, create one; first, look for appropriate types
		{
			final List<URI> typeURIs=new ArrayList<URI>();	//create a list to hold the type URIs
			for(final Assertion assertion:getAssertions())	//for each assertion
			{
				final Resource subject=assertion.getSubject();	//get the assertion subject
					//if this is an assertion in the form, {resource proxy, urf:type, XXX}
				if(subject.equals(resourceProxy)	//if this assertion has this resource proxy as its subject,
						&& TYPE_PROPERTY_URI.equals(assertion.getPredicate().getURI())	//and this assertion has a predicate of urf:type,
						&& assertion.getScopeChain().length==0)	//and this is not a scoped property
				{
					final Resource typeValueResource=assertion.getObject();	//get the type value
					if(typeValueResource!=subject)	//if the resource doesn't have itself for the type (otherwise, that would cause recursive unproxying) TODO improve to detect circular types; check URIs as wel
					{
						final URFResource typeValueURFResource;	//we'll find a resource to use as the type value
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
						typeURIs.add(typeValueURFResource.getURI());	//add this type to our list
					}
				}
			}
			final URF urf=getURF();	//get the URF data model
			resource=urf.locateResource(resourceURI, typeURIs.toArray(new URI[typeURIs.size()]));	//locate a resource for the given URI and type URIs, if any
		}
		for(final ResourceProxy equiavalentResourceProxy:resourceProxy.getEquivalentResourceProxies())	//for each equivalent resource proxy
		{
			putProxiedURFResource(equiavalentResourceProxy, resource);	//associate the resource with the resource proxy so we won't have to go through all this the next time
		}
		return resource;	//return the resource that was either already the target of the proxy, or that we created and associated with the proxy
	}

	/**Processes all assertions by creating associations between resources that reflect the assertions contained in the assertions.
	All resources should first have been created using {@link #createResources()} or in equivalent method.
	@exception IllegalStateException if a resource that has not been unproxied is encountered in an assertion.
	@exception IllegalStateException if an element on some scope chain that does not exist is reached.
	*/
	public void processAssertions()
	{
		for(final Assertion assertion:getAssertions())	//for each assertion
		{
			try
			{
				final URFResource subject=(URFResource)assertion.getSubject();	//get the assertion subject as an URF resource
				final URFResource predicate=(URFResource)assertion.getPredicate();	//get the assertion predicate as an URF resource
				final URFResource object=(URFResource)assertion.getObject();	//get the assertion object as an URF resource
				final URI predicateURI=predicate.getURI();	//get the predicate URI
				if(predicateURI!=null)	//if there is a predicate URI TODO do we want to allow anonymous predicates? if not, add an IllegalStateException
				{
					URFScope scope=subject;	//we'll determine which scope to use; start with the subject
					for(final NameValuePair<Resource, Resource> scopePair:assertion.getScopeChain())	//look at each property value pair (if any) on the scope chain
					{
						final URFResource scopeProperty=(URFResource)scopePair.getName();	//get the scope property
						final URFResource scopePropertyValue=(URFResource)scopePair.getValue();	//get the scope value
						final URI scopePropertyURI=scopeProperty.getURI();	//get the scope property URI
						if(scopePropertyURI!=null)	//if there is a scope property URI TODO do we want to allow anonymous predicates? if not, add an IllegalStateException
						{
							scope=scope.getScope(scopePropertyURI, scopePropertyValue);	//get the next scope in the chain
							if(scope==null)	//if we couldn't find the appropriate scope
							{
								throw new IllegalStateException("Could not find scope for "+scopePair);	//indicate that a scope could not be found
							}
						}
					}
					scope.addPropertyValue(predicateURI, object);	//process this assertion by adding the predicate and object to the scope as a property
				}
			}
			catch(final ClassCastException classCastException)	//if we have trouble casting a resource to an URF resource, a resource hasn't been unproxied
			{
				throw new IllegalStateException("Resource has not yet been unproxied for assertion.", classCastException);
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
	public class Assertion implements Comparable<Assertion>
	{

		/**The order in which this assertion was created.*/
		private final long order;

			/**@return The order in which this assertion was created.*/
			public long getOrder() {return order;}

		/**The chain of scope, each element representing a property and value to serve as scope for the subsequent property and value.*/
		private final NameValuePair<Resource, Resource>[] scopeChain;

			/**@return The chain of scope, each element representing a property and value to serve as scope for the subsequent property and value.*/
			public NameValuePair<Resource, Resource>[] getScopeChain() {return scopeChain;}

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
		@param scopeChain The chain of scope, each element representing a property and value to serve as scope for the subsequent property and value.
		@exception NullPointerException if the given subject, predicate, object, and/or scope chain is <code>null</code>.
		*/
		public Assertion(final Resource subject, final Resource predicate, final Resource object, final NameValuePair<Resource, Resource>... scopeChain)
		{
			order=generateAssertionOrder();	//set the order of this assertion
			setSubject(subject); //set the subject
			setPredicate(predicate); //set the predicate
			setObject(object); //set the object
			this.scopeChain=checkInstance(scopeChain, "Scope chain cannot be null.");
		}

		/**@return A hash code value for the assertion.*/
		public int hashCode()
		{
			return Objects.hashCode(getSubject(), getPredicate(), getObject(), getScopeChain());	//hash and return the subject, predicate, object, and scope chain
		}

		/**Compares assertions based upon subject, predicate, and object.
		@param object The object with which to compare this assertion; should be another assertion.
		@return <code>true<code> if the subjects, predicates, objects, and scope chains of the two assertions are equal.
		*/
		public boolean equals(final Object object)
		{
			if(object instanceof Assertion)	//if the other object is a assertion
			{
				final Assertion assertion2=(Assertion)object;	//cast the object to a assertion
				return getSubject().equals(assertion2.getSubject())	//compare subjects
						&& getPredicate().equals(assertion2.getPredicate())	//compare predicates
						&& getObject().equals(assertion2.getObject())	//compare objects
						&& Arrays.equals(getScopeChain(), assertion2.getScopeChain());	//compare scope chains
			}
			return false;	//show that the assertions do not match
		}

		/**Compares this object with the specified object for order.
		Returns a negative integer, zero, or a positive integer as this object is less than, equal to, or greater than the specified object.
		This implementation sorts assertions with smaller scope chains first to ensure that preconditions can be fulfilled.
		This implementation then sorts on assertion order.
		@param assertion The object to be compared.
		@return A negative integer, zero, or a positive integer as this object is less than, equal to, or greater than the specified object.
		*/
    public int compareTo(final Assertion assertion)
    {
    	int result=getScopeChain().length-assertion.getScopeChain().length;	//compare scope chain lengths
    	if(result==0)	//if scope chain lengths are equal
    	{
    		result=LongUtilities.compare(getOrder(), assertion.getOrder());	//compare orders
    	}
    	return result;	//return the result of the comparison
    }

		/**@return A string representation of the assertion in the form: "{<var>subject</var>; <var>scope</var>, <var>scope</var>...; <var>predicate</var>; <var>object</var>}".*/
		public String toString()
		{
			final StringBuilder stringBuilder=new StringBuilder(); //create a new string builder
			stringBuilder.append('{').append(DefaultResource.toString(getSubject())).append(';').append(' ');	//{subject;
			stringBuilder.append('(');	//begin scopes
			int scopeCount=0;	//keep track of the number of scopes
			for(final NameValuePair<Resource, Resource> scope:getScopeChain())	//for each scope in the scope chain
			{
				if(scopeCount>0)	//if this isn't the first scope
				{
					stringBuilder.append(',').append(' ');	//separate scopes
				}
				stringBuilder.append(DefaultResource.toString(scope.getName())).append('=').append(DefaultResource.toString(scope.getValue()));	//property=value
				++scopeCount;	//indicate we appended another scope
			}
			stringBuilder.append(')').append(';').append(' ');	//end scopes
			stringBuilder.append(DefaultResource.toString(getPredicate())).append(';').append(' ');	//predicate;
			stringBuilder.append(DefaultResource.toString(getObject())).append('}');	//object}
			return stringBuilder.toString(); //return the string we just constructed
		}

	}
}