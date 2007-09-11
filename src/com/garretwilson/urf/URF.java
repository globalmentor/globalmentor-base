package com.garretwilson.urf;

import java.net.URI;
import java.util.*;
import static java.util.Collections.*;

import static com.garretwilson.lang.ObjectUtilities.*;
import static com.garretwilson.net.URIConstants.*;
import static com.garretwilson.net.URIUtilities.*;

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
	public final static String URF_NAMESPACE_PREFIX="URF";
	/**The URI to the URF namespace.*/
	public final static URI URF_NAMESPACE_URI=URI.create("http://urf.name/urf");
	/**The URI to the URF index namespace.*/
	public final static URI URF_INDEX_NAMESPACE_URI=URI.create("urn:urf:index");
	/**The base to the URF lexical namespace.*/
	private final static String URF_LEX_NAMESPACE_BASE="urn:urf:lex:";
	/**The base URI to the URF lexical namespace.*/
	public final static URI URF_LEX_NAMESPACE_BASE_URI=URI.create(URF_LEX_NAMESPACE_BASE);
	
		//URF classes 
	/**The URI of the <code>(&lt;:urf:#Array&gt;)</code> class.*/ 
	public final static URI ARRAY_CLASS_URI=URF_NAMESPACE_URI.resolve("#Array");
	/**The URI of the <code>(&lt;:urf:#Boolean&gt;)</code> class.*/ 
	public final static URI BOOLEAN_CLASS_URI=URF_NAMESPACE_URI.resolve("#Boolean");
	/**The URI of the <code>(&lt;:urf:#Integer&gt;)</code> class.*/ 
	public final static URI INTEGER_CLASS_URI=URF_NAMESPACE_URI.resolve("#Integer");
	/**The URI of the <code>(&lt;:urf:#Number&gt;)</code> class.*/ 
	public final static URI NUMBER_CLASS_URI=URF_NAMESPACE_URI.resolve("#Number");
	/**The URI of the <code>(&lt;:urf:#Real&gt;)</code> class.*/ 
	public final static URI REAL_CLASS_URI=URF_NAMESPACE_URI.resolve("#Real");
	/**The URI of the <code>(&lt;:urf:#String&gt;)</code> class.*/ 
	public final static URI STRING_CLASS_URI=URF_NAMESPACE_URI.resolve("#String");
	/**The URI of the <code>(&lt;:urf:#URI&gt;)</code> class.*/ 
	public final static URI URI_CLASS_URI=URF_NAMESPACE_URI.resolve("#URI");
		//URF properties
	/**The URI of the <code>(&lt;:urf:#type&gt;)</code> property.*/ 
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
	/**The URI lexical namespace URI.*/
	public final static URI URI_NAMESPACE_URI=createLexicalNamespaceURI(URI_CLASS_URI);

	/**Creates a URI in the array namespace for the given index.
	@param index The index for which the index URI should be created.
	@return A URI in the index namespace for the specified index
	*/
	public static URI createIndexURI(final long index)
	{
		return resolveFragment(URF_INDEX_NAMESPACE_URI, Long.toString(index));	//create a string from the index and resolve it as a fragment to the index URI
	}

	/**Determines whether the given URI is in a lexical namespace.
	@param uri The URI to check for being in a lexical namespace
	@return <code>true</code> if the URI is the URI of a lexical namespace or is in a lexical namespace.
	@exception NullPointerException if the given URI is <code>null</code>.
	*/
	public static boolean isLexicalNamespaceURI(final URI uri)
	{
		return uri.toString().startsWith(URF_LEX_NAMESPACE_BASE);	//see if this URI starts with the lexical namespace base URI
	}

	/**Retrieves the type URI of a URI in a lexical namespace.
	@param uri The URI of a lexical namspace or a URI in a lexical namespace.
	@return The type URI of the lexical namespace.
	@exception IllegalArgumentException if the given URI is not in a lexical namespace.
	@exception IllegalArgumentException if the given URI's lexical namespace URI does not have a correctly encoded type URI.
	*/
	public static URI getLexicalNamespaceTypeURI(final URI uri)
	{
		final String lexicalNamespaceURIString=removeFragment(uri).toString();	//remove the URI's fragment
		if(!lexicalNamespaceURIString.startsWith(URF_LEX_NAMESPACE_BASE))	//if this URI doesn't start with the lexical namespace base URI
		{
			throw new IllegalArgumentException("URI "+uri+" is not a lexical namespace URI or a URI in a lexical namespace.");
		}
		return URI.create(decode(lexicalNamespaceURIString.substring(URF_LEX_NAMESPACE_BASE.length())));	//retrieve the type substring and decode it
	}

	/**Creates a lexical namespace URI for the given resource type.
	@param typeURI The URI of the type of the resource.
	@return The lexical namespace for the specified type.
	@exception NullPointerException if the given type URI is <code>null</code>.
	*/
	public static URI createLexicalNamespaceURI(final URI typeURI)
	{
		return URI.create(URF_LEX_NAMESPACE_BASE_URI.toString()+encodeURI(typeURI.toString()));	//encode the type and append it to the lexical namespace base URI
	}

	/**Creates a URI in a lexical namespace for the given resource type and lexical form.
	@param typeURI The URI of the type of the resource.
	@param lexicalForm The canonical lexical form of the resource.
	@return A URI in the lexical namespace for the specified type of a resource based upon its lexical form.
	@exception NullPointerException if the given type URI and/or lexical form is <code>null</code>.
	*/
	public static URI createLexicalURI(final URI typeURI, final String lexicalForm)
	{
		return URI.create(URF_LEX_NAMESPACE_BASE_URI.toString()+encodeURI(typeURI.toString())+FRAGMENT_SEPARATOR+encodeURI(lexicalForm));	//encode the type, append it to the lexical namespace base URI, and append the fragment of the encoded lexical form
	}

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

	/**Adds a resource to the data model.
	If the resource is already in the model, no action occurs. 
	@param resource The resource to add.
	*/
	public void addResource(final URFResource resource)
	{
//G***del Debug.trace("putting resource with URI: ", resource.getReferenceURI());
//G***del Debug.traceStack(); //G***del

		resourceSet.add(resource);	//add the resource to our set
		final URI resourceURI=resource.getURI();	//get the resource's URI, if any
		if(resourceURI!=null)	//if this is not an anonymous resource
			resourceMap.put(resourceURI, resource);  //store the resource in the map
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

	/**@return A read-only iterable of resources.*/
	public Iterable<URFResource> getResources()
	{
		return unmodifiableSet(resourceSet); //return an unmodifiable iterable to the set of all resources
	}

	/**@return A read-only iterable of resources appropriate for appearing at the root of a hierarchy.*/
/*TODO del if not needed
	public Iterable<URFResource> getRootResources()
	{
		return getRootResources(null);	//return an unsorted iterable to the root resources 
	}
*/

	/**Returns a read-only iterable of resources appropriate for appearing at the root of a hierarchy.
	The resources are sorted using the optional comparator.
	@param comparator The object that determines how the resources will be sorted, 	or <code>null</code> if the resources should not be sorted.
	@return A read-only iterable of root resources sorted by the optional comparator.
	*/
/*TODO del if not needed
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
	<p>This should be determined, among other things, by whether the resource in question is a property and whether or not there are references to the resource.
	This implementation considers root resources to be those that have a URI and have at least one property, along with those that have labels.</p> 
	@param resource The resource which might be a root resource.
	@return <code>true</code> if this resource is one of the resources that should be presented at the root of a hierarchy.
	*/
/*TODO del if not needed
	public boolean isRootResource(final URFResource resource)
	{
		final URI referenceURI=resource.getURI(); //get the resource URI
		final RDFLiteral label=RDFSUtilities.getLabel(resource);	//see if this resource has a label
//TODO eventually we'll probably have to determine if something is actually a property---i.e. this doesn't work: if(resource.getReferenceURI()!=null || resource.getPropertyCount()>0)	//only show resources that have URIs or have properties, thereby not showing property resources and literals at the root
			//if this is not a blank node and this resource actually has properties (even properties such as type identifiers are resources, but they don't have properties)
		return (referenceURI!=null && resource.getPropertyCount()>0)
					|| label!=null;	//if a resource is labeled, it's probably important enough to show at the top of the hierarchy as well 
		
	}
*/

	/**Retreives a resource from the data model based upon a URI.
	If no such resource exists, or no resource URI was given, a resource will be created and added to the data model.
	@param resourceURI The URI of the resource to retrieve, or <code>null</code> if the resource should have no URI.
	@return A resource with the given URI.
	@exception NullPointerException if the given URI is <code>null</code>.
	*/
	public URFResource locateResource(final URI resourceURI)
	{
		return locateResource(resourceURI, null);	//locate a resource without knowing its type
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

	/**Looks at all the resources in the RDF data model and recursively gathers
		which resources reference which other resources.
	<p>Circular references are correctly handled.</p>
	@return A map that associates, for each resource, a set of all resources that
		reference the that resource. Both the map and the associated set use
		identity rather than equality to store resources, as some resources may
		be anonymous.
	*/
/*TODO fix
	public Map<RDFResource, Set<RDFResource>> getReferences()
	{
		final Map<RDFResource, Set<RDFResource>> referenceMap=new IdentityHashMap<RDFResource, Set<RDFResource>>();	//create a new map in which to store reference sets
		return getReferences(referenceMap);	//gather all reference sets, place them in the reference map, and return the map
		
	}
*/

	/**Looks at all the resources in the RDF data model and recursively gathers
		which resources reference which other resources.
	<p>Circular references are correctly handled.</p>
	@param referenceMap A map that associates, for each resource, a set of all
		resources that reference the that resource.
	@return The map of resources and associated referring resources. The
		associated set will use identity rather than equality to store resources,
		as some resources may be anonymous.
	*/
/*TODO fix
	public Map<RDFResource, Set<RDFResource>> getReferences(final Map<RDFResource, Set<RDFResource>> referenceMap)
	{
		final Set<RDFResource> referringResourceSet=new IdentityHashSet<RDFResource>();	//create a set of referring resources to prevent endless following of circular references
		for(final RDFResource resource:getResources())	//for each resource in this data model
		{
			getReferences(resource, referenceMap, referringResourceSet);	//gather all references to this resource
		}
		return referenceMap;	//return the map we populated
	}
*/

	/**Looks at the resources and all its properties and recursively gathers
		which resources reference which other resources.
	<p>Circular references are correctly handled.</p>
	@param resource The resource for which references should be gathered for the
		resource and all resources that are property values of this resource's
		properties, and so on.
	@return A map that associates, for each resource, a set of all resources that
		reference the that resource. Both the map and the associated set use
		identity rather than equality to store resources, as some resources may
		be anonymous.
	*/
/*TODO fix
	public static Map<RDFResource, Set<RDFResource>> getReferences(final RDFResource resource)
	{
		return getReferences(resource, new IdentityHashMap<RDFResource, Set<RDFResource>>());	//create a new identity hash map and use it to retrieve references to the given resources
	}
*/

	/**Looks at the resources and all its properties and recursively gathers
		which resources reference which other resources.
	<p>Circular references are correctly handled.</p>
	@param resource The resource for which references should be gathered for the
		resource and all resources that are property values of this resource's
		properties, and so on.
	@param referenceMap A map that associates, for each resource, a set of all
		resources that reference the that resource.
	@return The map of resources and associated referring resources. The
		associated set will use identity rather than equality to store resources,
		as some resources may be anonymous.
	*/
/*TODO fix
	public static Map<RDFResource, Set<RDFResource>> getReferences(final RDFResource resource, final Map<RDFResource, Set<RDFResource>> referenceMap)
	{
		return getReferences(resource, referenceMap, new IdentityHashSet<RDFResource>());	//gather references, showing that we haven't looked at any referring resources, yet
	}
*/

	/**Looks at the resources and all its properties and recursively gathers
		which resources reference which other resources.
	<p>Circular references are correctly handled.</p>
	@param resource The resource for which references should be gathered for the
		resource and all resources that are property values of this resource's
		properties, and so on.
	@param referenceMap A map that associates, for each resource, a set of all
		resources that reference the that resource.
	@param referrerResourceSet The set of referrers the properties of which have
		been traversed, the checking of which prevents circular reference problems.
	@return The map of resources and associated referring resources. The
		associated set will use identity rather than equality to store resources,
		as some resources may be anonymous.
	*/
/*TODO fix
	protected static Map<RDFResource, Set<RDFResource>> getReferences(final RDFResource resource, final Map<RDFResource, Set<RDFResource>> referenceMap, final Set<RDFResource> referrerResourceSet)
	{
		if(!referrerResourceSet.contains(resource))	//if we haven't checked this resource before
		{
			referrerResourceSet.add(resource);	//show that we've now checked this resource (in case one of the resource's own properties or subproperties reference this resource)
			final Iterator propertyIterator=resource.getPropertyIterator();	//get an iterator to this resource's properties
			while(propertyIterator.hasNext())	//while there are more properties
			{
				final RDFPropertyValuePair property=(RDFPropertyValuePair)propertyIterator.next();	//get the next property
				final RDFObject valueObject=property.getPropertyValue();	//get the value of the property
				if(valueObject instanceof RDFResource)	//if the value is a resource
				{
					final RDFResource valueResource=(RDFResource)valueObject;	//cast the object value to a resource
					Set<RDFResource> referenceSet=referenceMap.get(valueResource);	//get the set of references to the object resource
					if(referenceSet==null)	//if this is the first reference we've gathered for the object resource
					{
						referenceSet=new IdentityHashSet<RDFResource>();	//create a new set to keep track of references to the object resource
						referenceMap.put(valueResource, referenceSet);	//store the set in the map, keyed to the object resource
					}
					referenceSet.add(resource);	//show that this resource is another referrer to the object resource of this property
					getReferences(valueResource, referenceMap, referrerResourceSet);	//gather resources to the object resource
				}
			}
		}
		return referenceMap;	//return the map that was provided, which now holds sets of references to resources
	}
*/

}