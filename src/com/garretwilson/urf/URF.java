package com.garretwilson.urf;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.*;
import java.util.concurrent.atomic.AtomicLong;

import static java.util.Collections.*;

import static com.garretwilson.lang.ObjectUtilities.*;
import static com.garretwilson.net.URIConstants.*;
import static com.garretwilson.net.URIUtilities.*;
import static com.garretwilson.urf.URF.URI_NAMESPACE_URI;
import static com.garretwilson.urf.URF.asNumber;

import com.garretwilson.io.ParseIOException;
import com.garretwilson.lang.LongUtilities;
import com.garretwilson.lang.NumberUtilities;
import com.garretwilson.net.Resource;
import com.garretwilson.rdf.RDFResource;
import com.garretwilson.util.*;

/**An URF data model.
<p>The data model should be used to create resources, as it keeps a list of
	registered resource factories based upon resource type namespaces.</p>
<p>The RDF data model itself is a resource factory for the default RDF resources
	such as bags, but registering a resource factory for the RDF namespace will
	override this default behavior.</p>
<p>The RDF data model itself is a typed literal factory for the default RDF
	datatypes (currently XMLLiteral), but registering a resource factory for the
	RDF namespace will override this default behavior.</p>
<p>The RDF data model by default registers a factories that handle the following
	datatype namespaces:</p>
<dl>
	<dt>XML Schema</dt> <dd>http://www.w3.org/2001/XMLSchema</dd>
</dl>
@author Garret Wilson
*/
public class URF 
{

	/**The recommended prefix to the URF namespace.*/
	public final static String URF_NAMESPACE_PREFIX="urf";
	/**The URI to the URF namespace.*/
	public final static URI URF_NAMESPACE_URI=URI.create("http://urf.name/urf");
	/**The URI to the URF index namespace.*/
//TODO del when not needed	public final static URI URF_INDEX_NAMESPACE_URI=URI.create("urn:urf:index");
	/**The base to the URF lexical namespace.*/
	private final static String URF_LEXICAL_NAMESPACE_BASE="info:lexical/";
	/**The base URI to the URF lexical namespace.*/
	public final static URI URF_LEXICAL_NAMESPACE_BASE_URI=URI.create(URF_LEXICAL_NAMESPACE_BASE);
	
		//URF classes 
	/**The URI of the URF <code>Array</code> class.*/ 
	public final static URI ARRAY_CLASS_URI=URF_NAMESPACE_URI.resolve("#Array");
	/**The URI of the URF <code>Boolean)</code> class.*/ 
	public final static URI BOOLEAN_CLASS_URI=URF_NAMESPACE_URI.resolve("#Boolean");
	/**The URI of the URF <code>Integer</code> class.*/ 
	public final static URI INTEGER_CLASS_URI=URF_NAMESPACE_URI.resolve("#Integer");
	/**The URI of the URF <code>Number</code> class.*/ 
	public final static URI NUMBER_CLASS_URI=URF_NAMESPACE_URI.resolve("#Number");
	/**The URI of the URF <code>Real</code> class.*/ 
	public final static URI REAL_CLASS_URI=URF_NAMESPACE_URI.resolve("#Real");
	/**The URI of the URF <code>String</code> class.*/ 
	public final static URI STRING_CLASS_URI=URF_NAMESPACE_URI.resolve("#String");
	/**The URI of the URF <code>URI</code> class.*/ 
	public final static URI URI_CLASS_URI=URF_NAMESPACE_URI.resolve("#URI");
		//URF properties
	/**The URI of the URF <code>order</code> property.*/ 
	public final static URI ORDER_PROPERTY_URI=URF_NAMESPACE_URI.resolve("#order");
	/**The URI of the URF <code>type</code> property.*/ 
	public final static URI TYPE_PROPERTY_URI=URF_NAMESPACE_URI.resolve("#type");

		//URF lexical namespaces
	/**The Boolean lexical namespace URI.*/
	public final static URI BOOLEAN_NAMESPACE_URI=createLexicalNamespaceURI(BOOLEAN_CLASS_URI);
		/**The lexical form of the Boolean value <code>false</code>.*/
		public final static String BOOLEAN_FALSE_LEXICAL_FORM=Boolean.FALSE.toString();
		/**The lexical form of the Boolean value <code>true</code>.*/
		public final static String BOOLEAN_TRUE_LEXICAL_FORM=Boolean.TRUE.toString();
		/**The URI of the boolean value <code>false</code>.*/
		public final static URI BOOLEAN_FALSE_URI=createLexicalURI(BOOLEAN_CLASS_URI, BOOLEAN_FALSE_LEXICAL_FORM);
		/**The URI of the boolean value <code>true</code>.*/
		public final static URI BOOLEAN_TRUE_URI=createLexicalURI(BOOLEAN_CLASS_URI, BOOLEAN_TRUE_LEXICAL_FORM);
	/**The integer lexical namespace URI.*/
	public final static URI INTEGER_NAMESPACE_URI=createLexicalNamespaceURI(INTEGER_CLASS_URI);
	/**The real lexical namespace URI.*/
	public final static URI REAL_NAMESPACE_URI=createLexicalNamespaceURI(REAL_CLASS_URI);
	/**The URI lexical namespace URI.*/
	public final static URI URI_NAMESPACE_URI=createLexicalNamespaceURI(URI_CLASS_URI);

	/**The atomic variable used to generate scope creation orders.*/
	private final static AtomicLong scopeCreationOrder=new AtomicLong(0);

		/**Generates a new scope creation order unique to this JVM.
		@return A new scope creation order unique to this JVM.
		*/
		public static long generateScopeCreationOrder()
		{
			return scopeCreationOrder.getAndDecrement();	//atomically get the next counter value
		}	

	/**Retrieves the namespace from the given URI.
	The namespace is the URI with no fragment.
	If the URI has no fragment, it is considered to have no local name and therefore no namespace, as the URI is the namespace URI itself.
	@param uri The URI from which a namespace should be retrieved.
	@return The namespace represented by the given URI, or <code>null</code> if the URI has no fragment and therefore is not in a namespace.
	@exception NullPointerException if the given URI is <code>null</code>.
	*/
	public static URI getNamespaceURI(final URI uri)
	{
		return uri.getFragment()!=null ? removeFragment(uri) : null;	//if there is a fragment, remove it; otherwise, report that there is no namespace		
	}

	/**Retrieves the local name from the given URI.
	The local name is the decoded fragment of the URI
	@param uri The URI from which a local name should be retrieved.
	@return The decoded local name represented by the given URI, or <code>null</code> if the given URI has no local name.
	@exception NullPointerException if the given URI is <code>null</code>.
	*/
	public static String getLocalName(final URI uri)
	{
		return uri.getFragment();	//get the URI's fragment
	}

	/**Creates a URI in the array namespace for the given index.
	@param index The index for which the index URI should be created.
	@return A URI in the index namespace for the specified index
	*/
	public static URI createIndexURI(final long index)
	{
		return createLexicalURI(INTEGER_CLASS_URI, Long.toString(index));	//create an integer URI from the index
//TODO del when works		return resolveFragment(URF_INDEX_NAMESPACE_URI, Long.toString(index));	//create a string from the index and resolve it as a fragment to the index URI
	}

	/**Determines whether the given URI is in a lexical namespace.
	This method returns <code>false</code> for lexical namespaces themselves (i.e. a lexical namespace URI with no fragment).
	@param uri The URI to check for being in a lexical namespace.
	@return <code>true</code> if the URI is is in a lexical namespace.
	@exception NullPointerException if the given URI is <code>null</code>.
	@see #isLexicalNamespaceURI(URI)
	*/
	public static boolean isLexicalURI(final URI uri)
	{
		final URI namespaceURI=getNamespaceURI(uri);	//get the namespace of the URI, if any
		return namespaceURI!=null && isLexicalNamespaceURI(namespaceURI);	//see if there is a namespace URI that is a lexical namespace URI
	}

	/**Determines whether the given URI is in a lexical namespace with the given type.
	This method returns <code>false</code> for lexical namespaces themselves (i.e. a lexical namespace URI with no fragment).
	@param uri The URI to check for being in a lexical namespace with the given lexical type
	@param lexicalTypeURI The URI of the type of the resource.
	@return <code>true</code> if the URI is is in a lexical namespace with the given lexical type.
	@exception NullPointerException if the given URI and/or lexical type URI is <code>null</code>.
	@see #isLexicalURI(URI)
	@see #isLexicalTypeURI(URI)
	*/
	public static boolean isLexicalTypeURI(final URI uri, final URI lexicalTypeURI)
	{
		return isLexicalURI(uri) && lexicalTypeURI.equals(getLexicalTypeURI(lexicalTypeURI));	//see if the URI is a lexical URI with the given lexical type
	}
	
	/**Determines whether the given URI the URI of a lexical namespace.
	This method returns <code>false</code> for URIs that are not namespaces (i.e. URIs with local names).
	@param uri The URI to check for being that of a lexical namespace
	@return <code>true</code> if the URI is that of a lexical namespace.
	@exception NullPointerException if the given URI is <code>null</code>.
	*/
	public static boolean isLexicalNamespaceURI(final URI uri)
	{
		return getLocalName(uri)==null && uri.toString().startsWith(URF_LEXICAL_NAMESPACE_BASE);	//see if this is a namespace URI that starts with the lexical namespace base URI
	}

	/**Retrieves the type URI of a URI in a lexical namespace.
	This method throws an exception for lexical namespaces themselves (i.e. a lexical namespace URI with no fragment).
	@param lexicalURI A URI URI in a lexical namespace.
	@return The type URI of the namespace of the lexical URI.
	@exception IllegalArgumentException if the given URI is not in a lexical namespace.
	@exception IllegalArgumentException if the given URI's lexical namespace URI does not have a correctly encoded type URI.
	@see #getLexicalNamespaceTypeURI(URI)
	*/
	public static URI getLexicalTypeURI(final URI lexicalURI)
	{
		final URI namespaceURI=getNamespaceURI(lexicalURI);	//get the namespace of the URI
		if(namespaceURI==null)	//if this URI has no namespace
		{
			throw new IllegalArgumentException("URI "+lexicalURI+" is not in any namespace.");
		}
		final String lexicalNamespaceURIString=namespaceURI.toString();	//get the string version of the namespace URI
		if(!lexicalNamespaceURIString.startsWith(URF_LEXICAL_NAMESPACE_BASE))	//if this URI doesn't start with the lexical namespace base URI
		{
			throw new IllegalArgumentException("URI "+lexicalURI+" is not a lexical namespace URI or a URI in a lexical namespace.");
		}
		return URI.create(decode(lexicalNamespaceURIString.substring(URF_LEXICAL_NAMESPACE_BASE.length())));	//retrieve the type substring and decode it
	}

	/**Retrieves the type URI of a lexical namespace URI.
	This method throws an exception if the given URI has a local name.
	@param namespaceURI The URI of a lexical namespace.
	@return The type URI of the lexical namespace.
	@exception IllegalArgumentException if the given URI is not a lexical namespace.
	@exception IllegalArgumentException if the given URI's lexical namespace URI does not have a correctly encoded type URI.
	*/
	public static URI getLexicalNamespaceTypeURI(final URI namespaceURI)
	{
		if(getLocalName(namespaceURI)!=null)	//if the given URI has a local name
		{
			throw new IllegalArgumentException("URI "+namespaceURI+" is not a namespace URI.");			
		}
		final String lexicalNamespaceURIString=namespaceURI.toString();	//get the string version of the namespace URI
		if(!lexicalNamespaceURIString.startsWith(URF_LEXICAL_NAMESPACE_BASE))	//if this URI doesn't start with the lexical namespace base URI
		{
			throw new IllegalArgumentException("URI "+namespaceURI+" is not a lexical namespace URI.");
		}
		return URI.create(decode(lexicalNamespaceURIString.substring(URF_LEXICAL_NAMESPACE_BASE.length())));	//retrieve the type substring and decode it
	}
	
	/**Creates a lexical namespace URI for the given resource type.
	@param typeURI The URI of the type of the resource.
	@return The lexical namespace for the specified type.
	@exception NullPointerException if the given type URI is <code>null</code>.
	*/
	public static URI createLexicalNamespaceURI(final URI typeURI)
	{
		return URI.create(URF_LEXICAL_NAMESPACE_BASE_URI.toString()+encodeURI(typeURI.toString()));	//encode the type and append it to the lexical namespace base URI
	}

	/**Creates a URI in a lexical namespace for the given resource type and lexical form.
	@param typeURI The URI of the type of the resource.
	@param lexicalForm The canonical lexical form of the resource.
	@return A URI in the lexical namespace for the specified type of a resource based upon its lexical form.
	@exception NullPointerException if the given type URI and/or lexical form is <code>null</code>.
	*/
	public static URI createLexicalURI(final URI typeURI, final String lexicalForm)
	{
		return URI.create(URF_LEXICAL_NAMESPACE_BASE_URI.toString()+encodeURI(typeURI.toString())+FRAGMENT_SEPARATOR+encodeURI(lexicalForm));	//encode the type, append it to the lexical namespace base URI, and append the fragment of the encoded lexical form
	}

	/**Determines the number represented by the given resource.
	@param resource The resource which is expected to represent a number, or <code>null</code>.
	@return The number represented by the given resource, or <code>null</code> if the resource does not represent a number.
	@exception IllegalArgumentException if the given resource represents a number that does not have the correct syntax.
	*/
	public static Number asNumber(final Resource resource)
	{
		return resource!=null ? asNumber(resource.getURI()) : null;	//if a resource was given, see if its URI represents a URI
	}

	/**Determines the number represented by the given URI.
	@param resourceURI The URI which is expected to represent a number, or <code>null</code>.
	@return The number represented by the given URI, or <code>null</code> if the URI does not represent a number.
	@exception IllegalArgumentException if the given URI represents a number that does not have the correct syntax.
	@see #INTEGER_CLASS_URI
	@see #INTEGER_NAMESPACE_URI
	@see #REAL_CLASS_URI
	@see #REAL_NAMESPACE_URI
	*/
	public static Number asNumber(final URI resourceURI)
	{
		if(resourceURI!=null)	//if a URI was given
		{
			final String localName=getLocalName(resourceURI);	//retrieve the URI local name, if any
			if(localName!=null)	//if there is a local name
			{
				final URI namespaceURI=getNamespaceURI(resourceURI);	//get the namespace of the URI
				if(INTEGER_NAMESPACE_URI.equals(namespaceURI))	//if this is an integer
				{
					return Long.parseLong(localName);	//parse a long from the local name
				}
				else if(REAL_NAMESPACE_URI.equals(namespaceURI))	//if this is an real
				{
					return Double.parseDouble(localName);	//parse a double from the local name
				}
			}
		}
		return null;	//no number could be found
	}
	
	/**Determines the URI represented by the given resource.
	@param resource The resource which is expected to represent a URI, or <code>null</code>.
	@return The URI represented by the given resource, or <code>null</code> if the resource does not represent a URI.
	@exception IllegalArgumentException if the given resource represents a URI that does not have the correct syntax.
	*/
	public static URI asURI(final Resource resource)
	{
		return resource!=null ? asURI(resource.getURI()) : null;	//if a resource was given, see if its URI represents a URI
	}

	/**Determines the URI represented by the given URI.
	@param resourceURI The URI which is expected to represent a URI, or <code>null</code>.
	@return The URI represented by the given URI, or <code>null</code> if the URI does not represent a URI.
	@exception IllegalArgumentException if the given URI represents a URI that does not have the correct syntax.
	@see #URI_CLASS_URI
	@see #URI_NAMESPACE_URI
	*/
	public static URI asURI(final URI resourceURI)
	{
		if(resourceURI!=null)	//if a URI was given
		{
			final String localName=getLocalName(resourceURI);	//retrieve the URI local name, if any
			if(localName!=null)	//if there is a local name
			{
				if(URI_NAMESPACE_URI.equals(getNamespaceURI(resourceURI)))	//if this is a URI
				{
					return URI.create(localName);	//create a URI from the local name
				}
			}
		}
		return null;	//no URI could be found
	}

	/**Comparator for sorting resources in by their property counts, from few to many.*/
	public final static Comparator<URFResource> RESOURCE_PROPERTY_COUNT_COMPARATOR=new Comparator<URFResource>()
			{
		
				/**Compares its two arguments for order.
				Returns a negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second.
				This implementation compares by property count.
				@param resource1 The first object to be compared.
				@param resource2 The second object to be compared.
				@return A negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second.
				*/
				public int compare(final URFResource resource1, final URFResource resource2)
				{
					int result=LongUtilities.compare(resource1.getPropertyCount(), resource2.getPropertyCount());	//compare property counts
					if(result==0)	//if property counts are the same
					{
						result=LongUtilities.compare(resource1.getCreationOrder(), resource2.getCreationOrder());	//compare creation order
					}
					return result;	//return the result of the comparison
				}
			};
	
	/**A map of resource factories, keyed to namespace URIs.*/
	private final Map<URI, URFResourceFactory> namespaceURIResourceFactoryMap=new HashMap<URI, URFResourceFactory>();

		/**Registers a resource factory to be used to create resources with a type from the specified namespace.
		If a resource factory is already registered for this namespace, it will be replaced.
		@param typeNamespaceURI The namespace of the resource type for which this factory should be used to create objects.
		@param factory The resource factory that will be used to create resources of types from this namespace.
		*/
		public void registerResourceFactory(final URI typeNamespaceURI, final URFResourceFactory factory)
		{
			namespaceURIResourceFactoryMap.put(typeNamespaceURI, factory);
		}

		/**Removes the resource factory being used to create resources with a type from the specified namespace.
		If there is no resource factory registered for this namespace, no action will be taken.
		@param typeNamespaceURI The namespace of the resource type for which this factory should be used to create objects.
		*/
		public void unregisterResourceFactory(final URI typeNamespaceURI)
		{
			namespaceURIResourceFactoryMap.remove(typeNamespaceURI);
		}

		/**Retrieves a resource factory to be used for creating resources with a type from the specified namespace URI.
		@param typeNamespaceURI The namespace of the type for which a resource factory should be returned.
		@return The factory registered for this type namespace, or <code>null</code> if there is no factory registered for this type namespace.
		*/
		protected URFResourceFactory getResourceFactory(final URI typeNamespaceURI)
		{
			return namespaceURIResourceFactoryMap.get(typeNamespaceURI);  //return any factory registered for this namespace
		}

	/**The set of all resources, identified and anonymous, using identity rather than equality for equivalence.*/
	private final IdentityHashSet<URFResource> resourceSet=new IdentityHashSet<URFResource>();

	/**The map of all identified resources, keyed to resource URIs.*/
	private final Map<URI, URFResource> resourceMap=new HashMap<URI, URFResource>();

	/**@return A read-only set of the URIs of all named resources in this data model.*/
	public Set<URI> getResourceURIs()
	{
		return unmodifiableSet(resourceMap.keySet());	//return the set of keys to the resource map
	}
	
	/**Adds a resource to the data model.
	All property value resources are recursively added to the model.
	@param resource The resource to add.
	@exception NullPointerException if the given resource is <code>null</code>.
	*/
	public void addResource(final URFResource resource)	//TODO fix to add scoped resource values
	{
		addResource(resource, new IdentityHashSet<URFResource>());	//add this resource, using an identity hash map to determine which resources have been added
	}

	/**Adds a resource to the data model if it hasn't been added already.
	All property value resources are recursively added to the model.
	If the resource
	@param resource The resource to add.
	@param addedResources The set of resources added to the set to prevent infinite recursion in self-connected graphs.
	@exception NullPointerException if the given resource and or added resources set is <code>null</code>.
	@see #addPropertyValues(URFScope, Set)
	*/
	protected void addResource(final URFResource resource, final Set<URFResource> addedResources)
	{
		if(!addedResources.contains(resource))	//if we haven't already added this resource
		{
			resourceSet.add(resource);	//add the resource to our set
			final URI resourceURI=resource.getURI();	//get the resource's URI, if any
			if(resourceURI!=null)	//if this is not an anonymous resource
				resourceMap.put(resourceURI, resource);  //store the resource in the map
			addedResources.add(resource);	//indicate that we added this resource
			addPropertyValues(resource, addedResources);	//add all property values from the resource recursively
		}
	}

	/**Adds all the property values of a particular scope to the data model if they hasn't been added already.
	The property values of each property value's scope are also recursively added to the model.
	@param scope The scope the property values of which to add.
	@param addedResources The set of resources added to the set to prevent infinite recursion in self-connected graphs.
	@exception NullPointerException if the given resource and or added resources set is <code>null</code>.
	*/
	protected void addPropertyValues(final URFScope scope, final Set<URFResource> addedResources)
	{
		for(final URFProperty property:scope.getProperties())	//for each property in the scope
		{
			addResource(property.getValue());	//add this property value resource
			addPropertyValues(property.getScope(), addedResources);	//add all property values from the property-value's scope recursively
		}
	}

	/**Retrieves an identified resource from the data model using its URI.
	@param resourceURI The URI of the resource to retrieve.
	@return The resource, or <code>null</code> if no matching resource was found.
	@exception NullPointerException if the given URI is <code>null</code>.
	*/
	public URFResource getResource(final URI resourceURI)
	{
//G***del Debug.trace("getting resource with URI: ", resourceURI);
//G***del Debug.traceStack(); //G***del
		return resourceMap.get(checkInstance(resourceURI, "Resource URI cannot be null.")); //retrieve the resource
	}

	/**@return The number of resources in this data model.*/
	public int getResourceCount()
	{
		return resourceSet.size();  //return the size of the resource set
	}

	/**Returns a read-only iterable of all resources in the data model.
	@return A read-only iterable of resources in the data model.
	*/
	public Iterable<URFResource> getResources()
	{
		return unmodifiableSet(resourceSet); //return an unmodifiable iterable to the set of all resources
	}

	/**Returns a read-only iterable of all resources in the data model, sorted by the given comparator.
	@param comparator The object that determines how the resources will be sorted.
	@return A read-only iterable of resources in the data model.
	@exception NullPointerException if the given comparator is <code>null</code>.
	*/
/*TODO del
	public Iterable<URFResource> getResources(final Comparator<URFResource> comparator)
	{
		final List<URFResource> resourceList=new ArrayList<URFResource>(resourceSet);	//create a list of all the resources
		sort(resourceList, comparator)
		
		return unmodifiableSet(resourceSet); //return an unmodifiable iterable to the set of all resources
	}
*/

	/**@return A read-only iterable of resources appropriate for appearing at the root of a hierarchy, such as a TURF or XMURF representation.*/
/*TODO del if not needed
	public Iterable<URFResource> getRootResources()
	{
		return getRootResources(null);	//return an unsorted iterable to the root resources 
	}
*/
	
	/**Returns a read-only iterable of resources appropriate for appearing at the root of a hierarchy, such as a TURF or XMURF representation.
	The resources are sorted using the optional comparator.
	@param comparator The object that determines how the resources will be sorted, or <code>null</code> if the resources should not be sorted.
	@return A read-only iterable of root resources sorted by the optional comparator.
	*/
/*TODO del
	public Iterable<URFResource> getRootResources(final Comparator<URFResource> comparator)
	{
			//create a set in which to place the root resources, making the set sorted if we have a comparator
	///TODO fix		final Set<RDFResource> rootResourceSet=comparator!=null ? (Set<RDFResource>)new TreeSet<RDFResource>(comparator) : (Set<RDFResource>)new HashSet<RDFResource>();	 		
		final Set<URFResource> rootResourceSet=new HashSet<URFResource>();	//TODO fix comparing once we decide what type of comparator to use---should it include just resources, or all RDF objects?
		for(final URFResource resource:getResources())	//look at all resouces
		{
			if(isRootResource(resource))	//if this is a root resource
			{
				rootResourceSet.add(resource);	//add the resource to the set of root resources
			}
		}
		return unmodifiableCollection(rootResourceSet); //return an unmodifiable set of root resources
	}
*/

	/**Determines if the given resource is appropriate for appearing at the root of a hierarchy.
	This should be determined, among other things, by whether the resource in question is a property and whether or not there are references to the resource.
	This implementation considers root resources to be those that have a URI and have at least one property, along with those that have labels. 
	@param resource The resource which might be a root resource.
	@return <code>true</code> if this resource is one of the resources that should be presented at the root of a hierarchy.
	*/
/*TODO del
	public boolean isRootResource(final URFResource resource)
	{
		if(resource)

//Debug.trace("is root resource?", resource);
		final URI referenceURI=resource.getURI(); //get the resource URI, if any
		
//Debug.trace("referenceURI:", referenceURI);
//TODO fix		final RDFLiteral label=RDFSUtilities.getLabel(resource);	//see if this resource has a label
//TODO eventually we'll probably have to determine if something is actually a property---i.e. this doesn't work: if(resource.getReferenceURI()!=null || resource.getPropertyCount()>0)	//only show resources that have URIs or have properties, thereby not showing property resources and literals at the root
*/
/*TODO fix
final Iterator<URFProperty> propertyIterator=resource.getProperties().iterator();
if(referenceURI!=null && propertyIterator.hasNext())
{
	Debug.trace("property:", propertyIterator.next());
}
*/
/*TODO del
//TODO fix to check if resources witih lexical URIs have more types than their lexical type; fix properties routines to remove property value context list from the map if all property value contexts are removed
//TODO fix property isEmpty() and property count methods		
			//if this is not an anonymous resource and this resource actually has properties
		return (referenceURI!=null && resource.getProperties().iterator().hasNext());	//TODO fix; this is very inefficient
//TODO fix		return (referenceURI!=null && resource.getPropertyCount()>0)
//TODO fix					|| label!=null;	//if a resource is labeled, it's probably important enough to show at the top of the hierarchy as well 
	}
*/

	/**Retreives a resource from the data model based upon a URI.
	If no such resource exists, or no resource URI was given, a resource will be created and added to the data model.
	If the given resource URI is in a lexical namespace, its lexical type will be used.
	@param resourceURI The URI of the resource to retrieve, or <code>null</code> if the resource should have no URI.
	@return A resource with the given URI.
	@exception NullPointerException if the given URI is <code>null</code>.
	*/
	public URFResource locateResource(final URI resourceURI)
	{
		final URI lexicalTypeURI=isLexicalURI(resourceURI) ? getLexicalTypeURI(resourceURI) : null;	//get the lexical type, if we can
		return locateResource(resourceURI, lexicalTypeURI);	//locate a resource with whatever type we determined, if any
	}

	/**Retrieves a resource from the data model based upon the URI of the resource and an optional type URI.
	If no such resource exists, or no resource URI was given, a resource will be created and added to the data model.
	The given type URI, if any, will be used to locate a resource factory to create the resource, and that type URI will be added as a type property.
	If the resource already exists, no checks are performed to ensure that the existing resource is of the requested type.
	@param resourceURI The URI of the resource to retrieve, or <code>null</code> if the resource should have no URI.
	@param typeURI The URI of the type, or <code>null</code> if the type is not known.
	@return A resource with the given URI.
	*/
	public URFResource locateResource(final URI resourceURI, final URI typeURI)
	{
		URFResource resource=resourceURI!=null ? getResource(resourceURI) : null;  //retrieve a resource from the data model if a resource URI was given
		if(resource==null)  //if no such resource exists
		{
			resource=createResource(resourceURI, typeURI);  //create a new resource of the given type from the given URI and store the resource in the data model
		}
		return resource;  //return the resource we either found or created
	}

	/**Creates an anonymous resource and stores it in this data model.
	@return An anonymous resource.
	*/
	public URFResource createResource()
	{
		return createResource(null);  //create a resource with no URI URI
	}

	/**Creates a general resource with the specified URI and stores it in this data model.
	@param resourceURI The URI of the resource to create, or <code>null</code> if the resource created should be anonymous.
	@return A resource with the given URI.
	*/
	public URFResource createResource(final URI resourceURI)
	{
		return createResource(resourceURI, null);	//create and return a resource without a type
	}

	/**Creates a resource with the provided URI and type URI.
	The given type URI will be used to attempt to locate a resource factory to create the resource.
	A type property with the given type URI will be added to the resource.
	The created resource will be stored in this data model.
	@param resourceURI The URI of the resource to create, or <code>null</code> if the resource created should be anonymous.
	@param typeURI The URI of the type, or <code>null</code> if the type is not known.
	@return The resource created with this URI, with the given type added if a type was given.
	*/
	public URFResource createResource(final URI resourceURI, final URI typeURI)
	{
		URFResource resource=null; //start by assuming that no factory is registered for this type namespace, or the registered factory can't create a resource
		if(typeURI!=null)	//if we know the type
		{
			final String typeLocalName=typeURI.getFragment();	//get the local name of the type
			if(typeLocalName!=null)	//if there is a local name
			{
				final URI typeNamespaceURI=removeFragment(typeURI);	//get the namespace URI
				final URFResourceFactory resourceFactory=getResourceFactory(typeNamespaceURI); //get a resource factory for this namespace
				if(resourceFactory!=null) //if we have a factory
				{
					resource=resourceFactory.createResource(resourceURI, typeURI); //try to create a resource from this factory
				}
/*TODO del if not needed
				if(resource==null)  //if we haven't created a resource, see if this is an RDF resource
				{
					if(NIL_RESOURCE_URI.equals(referenceURI))	//if we are creating the nil resource
					{
						resource=new RDFListResource(this, NIL_RESOURCE_URI);	//create the nil resource with the special RDF nil URI
					}
				}
*/
			}
		}
		if(resource==null)  //if we didn't create a resource from a factory
		{
		  resource=new DefaultURFResource(resourceURI);  //create a new resource from the given URI
			addResource(resource);  //store the resource in the data model
		}
		if(typeURI!=null)	//if we were given a type
		{
//TODO fix			RDFUtilities.addType(resource, typeNamespaceURI, typeLocalName); //add the type property
		}
		return resource;  //return the resource we created
	}

	/**Attempts to create a resource with the provided reference URI
	The given type URI will be used to attempt to locate a resource factory to create the resource.
	<p>The created resource, if any, will be added to this RDF data model, but
		no type will be added to the resource.</p>
	<p>This method knows how to create the following RDF-defined resources:</p>
	<ul>
		<li>Reference URI <code>rdf:nil</code> (<code>RDFListResource</code>)</li>
		<li>Type <code>rdf:Alt</code>  (<code>RDFAltResource</code>)</li>
		<li>Type <code>rdf:Bag</code>  (<code>RDFBagResource</code>)</li>
		<li>Type <code>rdf:Seq</code>  (<code>RDFSequenceResource</code>)</li>
	</ul>
	@param referenceURI The reference URI of the resource to create, or
		<code>null</code> if the resource created should be represented by a blank node.
	@param typeURI The URI of the type, or <code>null</code> if the type is not known.
	@return The resource created with this reference URI, or <code>null</code>
		if the resource could not be created from a resource factory or a suitable
		resource factory could not be found.
	*/
/*TODO fix
	public RDFResource createTypedResourceFromFactory(final URI referenceURI, final URI typeURI)
	{
		return createTypedResourceFromFactory(referenceURI, typeURI!=null ? getNamespaceURI(typeURI) : null, typeURI!=null ? getLocalName(typeURI) : null);	//create a typed resource after breaking out the namespace URI and the local name of the type
	}
*/

	/**Attempts to create a resource with the provided reference URI
		The given type namespace URI and type local name will be used to
		attempt to locate a resource factory to create the resource.
	<p>The created resource, if any, will be added to this RDF data model, but
		no type will be added to the resource.</p>
	<p>This method knows how to create the following RDF-defined resources:</p>
	<ul>
		<li>Reference URI <code>rdf:nil</code> (<code>RDFListResource</code>)</li>
		<li>Type <code>rdf:Alt</code>  (<code>RDFAltResource</code>)</li>
		<li>Type <code>rdf:Bag</code>  (<code>RDFBagResource</code>)</li>
		<li>Type <code>rdf:Seq</code>  (<code>RDFSequenceResource</code>)</li>
	</ul>
	@param referenceURI The reference URI of the resource to create, or
		<code>null</code> if the resource created should be represented by a blank node.
	@param typeNamespaceURI The XML namespace used in the serialization of the
		type URI, or <code>null</code> if the type is not known.
	@param typeLocalName The XML local name used in the serialization of the type
		URI, or <code>null</code> if the type is not known.
	@return The resource created with this reference URI, or <code>null</code>
		if the resource could not be created from a resource factory or a suitable
		resource factory could not be found.
	*/
/*TODO fix
	public RDFResource createTypedResourceFromFactory(final URI referenceURI, final URI typeNamespaceURI, final String typeLocalName)
	{
		RDFResource resource=null; //start by assuming that no factory is registered for this type namespace, or the registered factory can't create a resource
		final RDFResourceFactory resourceFactory=getResourceFactory(typeNamespaceURI); //get a resource factory for this namespace
		if(resourceFactory!=null) //if we have a factory
		{
			resource=resourceFactory.createResource(referenceURI, typeNamespaceURI, typeLocalName); //try to create a resource from this factory
		}
		if(resource==null)  //if we haven't created a resource, see if this is an RDF resource
		{
			if(NIL_RESOURCE_URI.equals(referenceURI))	//if we are creating the nil resource
			{
				resource=new RDFListResource(this, NIL_RESOURCE_URI);	//create the nil resource with the special RDF nil URI
			}
			else if(RDF_NAMESPACE_URI.equals(typeNamespaceURI)) //if this resource is an RDF resource
			{
				if(ALT_CLASS_NAME.equals(typeLocalName)) //<rdf:Alt>
				{
					//G***fix for alt
				}
				else if(BAG_CLASS_NAME.equals(typeLocalName))  //<rdf:Bag>
				{
					resource=new RDFBagResource(this, referenceURI);  //create a bag resource
				}
				else if(SEQ_CLASS_NAME.equals(typeLocalName))  //<rdf:Seq>
				{
					resource=new RDFSequenceResource(this, referenceURI);  //create a sequence resource
				}
				else if(LIST_CLASS_NAME.equals(typeLocalName))  //<rdf:Seq>
				{
					resource=new RDFListResource(this, referenceURI);  //create a list resource
				}
			}
		}
		if(resource!=null)  //if we found a resource
		{
			if(resource.getRDF()!=this)	//if a resource was created that isn't associated with this data model
			{
				resource.setRDF(this);	//associate the resource with this data model TODO create an import() method that will recursively set the data models of the resource and all properties and property values
			}
			addResource(resource);  //store the resource in the data model
		}
		return resource;  //return the resource we created
	}
*/

	/**Default constructor.*/
	public URF()
	{
	}

	/**Looks at all the scopes in the URF data model and recursively gathers which scopes reference which other resources.
	Circular references are correctly handled.
	The returned map and the associated sets use identity rather than equality to store resources, as some resources may be anonymous.
	@param referenceMap A map that associates, for each resource, a set of all scopes that reference that resource.
	@return The map of resources and associated referring scopes.
	*/
	public CollectionMap<URFResource, URFScope, Set<URFScope>> getReferences()
	{
		final CollectionMap<URFResource, URFScope, Set<URFScope>> referenceMap=new IdentityHashSetMap<URFResource, URFScope>(new IdentityHashMap<URFResource, Set<URFScope>>());	//create a new map in which to store reference sets
		final Set<URFScope> referrerScopeSet=new IdentityHashSet<URFScope>();	//create a set of referring scopes to prevent circular references
		for(final URFResource resource:getResources())	//for each resource in this data model
		{
			getReferences(resource, referenceMap, referrerScopeSet);	//gather all references to this resource
		}
		return referenceMap;	//return the map we populated
	}

	/**Looks at the scope and all its properties and recursively gathers which scopes reference which other resources.
	Circular references are correctly handled.
	The returned map and the associated sets use identity rather than equality to store resources, as some resources may be anonymous.
	@param scope The scope for which references should be gathered for the scope and all child scopes and resources that are property values of this resource's properties, and so on.
	@param referenceMap A map that associates, for each resource, a set of all scopes that reference that resource value.
	@param referrerScopeSet The set of referrers the properties and scopes of which have been traversed, the checking of which prevents circular reference problems.
	@return The map of resources and associated referring scopes.
	*/
	protected CollectionMap<URFResource, URFScope, Set<URFScope>> getReferences(final URFScope scope, final CollectionMap<URFResource, URFScope, Set<URFScope>> referenceMap, final Set<URFScope> referrerScopeSet)
	{
		if(!referrerScopeSet.contains(scope))	//if we haven't checked this scope before
		{
			referrerScopeSet.add(scope);	//show that we've now checked this scope (in case one of the scope's own properties, subproperties, or child scopes reference this resource)
			for(final URFProperty property:scope.getProperties())	//for each property in the scope
			{
				final URFResource value=property.getValue();	//get the property value
				referenceMap.addItem(value, scope);	//note that this scope references this value
				getReferences(value, referenceMap, referrerScopeSet);	//get all references that the value makes
				getReferences(property.getScope(), referenceMap, referrerScopeSet);	//get all references that the scope makes
			}
		}
		return referenceMap;	//return the map that was provided, which now holds sets of references to resources
	}

}