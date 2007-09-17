package com.garretwilson.urf;

import java.net.URI;
import java.util.*;
import java.util.concurrent.atomic.AtomicLong;
import static java.util.Collections.*;

import static com.garretwilson.lang.ObjectUtilities.*;

import com.garretwilson.io.ParseIOException;
import com.garretwilson.lang.LongUtilities;
import com.garretwilson.net.Resource;
import static com.garretwilson.net.URIConstants.*;
import static com.garretwilson.net.URIUtilities.*;
import com.garretwilson.util.*;

/**An URF data model.
This data model keeps track of all resources that are being created as a linked group, such as parsed from a TURF interchange document,
and are thought of as a separate universe of descriptions.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) <http://www.urf.name/> specification and processing Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>.
Written by Garret Wilson <http://www.garretwilson.com/>."
Any redistribution of source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public class URF 
{

	/**The recommended prefix to the URF namespace.*/
	public final static String URF_NAMESPACE_PREFIX="urf";
	/**The URI to the URF namespace.*/
	public final static URI URF_NAMESPACE_URI=URI.create("http://urf.name/urf");
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
			return scopeCreationOrder.getAndIncrement();	//atomically get the next counter value
		}

	/**Creates a resource URI from a given namespace URI and a local name.
	If the namespace URI is a hierarchical URI that ends with a path separator, the local name is encoded and appended to the URI.
	Otherwise, the local name is encoded and added as a fragment.
	@param namespaceURI The URI of the namespace.
	@param localName The unencoded local name of the resource.
	@return A URI constructed from the given namespace URI and local name.
	@exception NullPointerException if the given namespace URI and/or local name is <code>null</code>.
	@exception IllegalArgumentException if the given namespace URI has a fragment.
	*/
	public static URI createURI(final URI namespaceURI, final String localName)
	{
//Debug.trace("creating URI from namespace", namespaceURI, "and local name", localName);
		if(namespaceURI.getRawFragment()!=null)	//if the supposed namespace URI has a fragment (namespaces can't have fragments)
		{
			throw new IllegalArgumentException("Invalid namespace URI: "+namespaceURI);
		}
		final String namespaceURIString=namespaceURI.toString();	//get the string form of the namespace
//Debug.trace("namespace URI string", namespaceURIString);

		final String encodedLocalName=encodeURI(localName);	//encode the local name
//Debug.trace("encoded local name", encodedLocalName);

		final int namespaceURIStringLength=namespaceURIString.length();	//get the length of the namespace URI string
		if(namespaceURIStringLength>0 && namespaceURIString.charAt(namespaceURIStringLength-1)==PATH_SEPARATOR)	//if the string ends with a path separator
		{
//Debug.trace("ready to append local name");
			return URI.create(namespaceURIString+encodedLocalName);	//append the encoded name to the URI
		}
		else	//if the string ends with any other character
		{
//Debug.trace("ready to add local name as fragment");
			return resolveFragment(namespaceURI, encodedLocalName);	//add the local name as a fragment
		}
	}

	/**Retrieves the namespace from the given URI.
	The namespace is the URI with no fragment, if there is a fragment; or the parent collection of a hierarchical URI that is not itself a collection.
	If the URI has no fragment or no non-collection name, it is considered to have no local name and therefore no namespace, as the URI is the namespace URI itself.
	@param uri The URI from which a namespace should be retrieved.
	@return The namespace represented by the given URI, or <code>null</code> if the URI has no fragment and therefore is not in a namespace.
	@exception NullPointerException if the given URI is <code>null</code>.
	*/
	public static URI getNamespaceURI(final URI uri)
	{
		if(uri.getRawFragment()!=null)	//if the URI has a fragment
		{
			return removeFragment(uri);	//remove the fragment to get the namespace
		}
		else	//check for a path-based namespace
		{
			final String rawPath=uri.getRawPath();	//get the raw path
			if(rawPath!=null)	//if there is a raw path
			{
				final int rawPathLength=rawPath.length();	//get the length of the raw path
				if(rawPathLength>0 && rawPath.charAt(rawPathLength-1)!=PATH_SEPARATOR)	//if there is a raw path that isn't a collection
				{
					return getCurrentLevel(uri);	//return the base level of the URI without the local name
				}
			}
		}
		return null;	//indicate that this URI has no namespace
	}

	/**Retrieves the local name from the given URI.
	The local name is the decoded fragment of the URI, if there is a fragment; or the decoded name of a hierarchical URI that is not a collection.
	If the URI has no fragment or no non-collection name, it is considered to have no local name.
	@param uri The URI from which a local name should be retrieved.
	@return The decoded local name represented by the given URI, or <code>null</code> if the given URI has no local name.
	@exception NullPointerException if the given URI is <code>null</code>.
	*/
	public static String getLocalName(final URI uri)
	{
		final String fragment=uri.getFragment();	//get the URI fragment
		if(fragment!=null)	//if there is a fragment
		{
			return fragment;	//return the fragment
		}
		else	//if there is no fragment
		{
			final String rawPath=uri.getRawPath();	//get the raw path
			if(rawPath!=null)	//if there is a raw path
			{
				final int rawPathLength=rawPath.length();	//get the length of the raw path
				if(rawPathLength>0 && rawPath.charAt(rawPathLength-1)!=PATH_SEPARATOR)	//if there is a raw path that isn't a collection
				{
					return getName(rawPath);	//return the name from the raw path
				}
			}
		}
		return null;	//indicate that this URI has no local name
	}

	/**Creates a URI in the array namespace for the given index.
	@param index The index for which the index URI should be created.
	@return A URI in the index namespace for the specified index
	*/
	public static URI createIndexURI(final long index)
	{
		return createLexicalURI(INTEGER_CLASS_URI, Long.toString(index));	//create an integer URI from the index
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
			final String typeLocalName=getLocalName(typeURI);	//get the local name of the type
			if(typeLocalName!=null)	//if there is a local name
			{
				final URI typeNamespaceURI=getNamespaceURI(typeURI);	//get the namespace URI
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