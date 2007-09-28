package com.garretwilson.urf;

import java.io.*;
import java.net.URI;
import java.util.*;
import static java.util.Collections.*;
import java.util.concurrent.atomic.AtomicLong;

import javax.mail.internet.ContentType;

import com.garretwilson.io.ContentTypeUtilities;
import static com.garretwilson.lang.BooleanUtilities.*;
import static com.garretwilson.lang.CharacterUtilities.*;
import com.garretwilson.lang.ClassUtilities;
import com.garretwilson.lang.LongUtilities;
import static com.garretwilson.lang.ObjectUtilities.*;
import com.garretwilson.net.*;
import static com.garretwilson.net.URIConstants.*;
import static com.garretwilson.net.URIUtilities.*;
import static com.garretwilson.text.CharacterEncodingConstants.*;
import com.garretwilson.urf.content.*;
import com.garretwilson.util.*;

/**An URF data model.
This data model keeps track of all resources that are being created as a linked group, such as parsed from a TURF interchange document,
and are thought of as a separate universe of descriptions.
This implementation by default registers the following namespace factories for the following namespaces:
<dl>
	<dt>{@value #URF_NAMESPACE_URI}</dt> <dd>{@link #DEFAULT_URF_RESOURCE_FACTORY}</dd>
	<dt>{@value Content#CONTENT_NAMESPACE_URI}</dt> <dd>{@link Content#DEFAULT_CONTENT_RESOURCE_FACTORY}</dd>
</dl>
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
	
		//classes 
	/**The URI of the URF <code>Array</code> class.*/ 
	public final static URI ARRAY_CLASS_URI=createResourceURI(URF_NAMESPACE_URI, "Array");
	/**The URI of the URF <code>Binary</code> class.*/ 
	public final static URI BINARY_CLASS_URI=createResourceURI(URF_NAMESPACE_URI, "Binary");
	/**The URI of the URF <code>Boolean</code> class.*/ 
	public final static URI BOOLEAN_CLASS_URI=createResourceURI(URF_NAMESPACE_URI, "Boolean");
	/**The URI of the URF <code>Character</code> class.*/ 
	public final static URI CHARACTER_CLASS_URI=createResourceURI(URF_NAMESPACE_URI, "Character");
	/**The URI of the URF <code>Integer</code> class.*/ 
	public final static URI INTEGER_CLASS_URI=createResourceURI(URF_NAMESPACE_URI, "Integer");
	/**The URI of the URF <code>Number</code> class.*/ 
	public final static URI NUMBER_CLASS_URI=createResourceURI(URF_NAMESPACE_URI, "Number");
	/**The URI of the URF <code>Ordinal</code> class.*/ 
	public final static URI ORDINAL_CLASS_URI=createResourceURI(URF_NAMESPACE_URI, "Ordinal");
	/**The URI of the URF <code>Real</code> class.*/ 
	public final static URI REAL_CLASS_URI=createResourceURI(URF_NAMESPACE_URI, "Real");
	/**The URI of the URF <code>Resource</code> class.*/ 
	public final static URI RESOURCE_CLASS_URI=createResourceURI(URF_NAMESPACE_URI, "Resource");
	/**The URI of the URF <code>Set</code> class.*/ 
	public final static URI SET_CLASS_URI=createResourceURI(URF_NAMESPACE_URI, "Set");
	/**The URI of the URF <code>String</code> class.*/ 
	public final static URI STRING_CLASS_URI=createResourceURI(URF_NAMESPACE_URI, "String");
	/**The URI of the URF <code>URI</code> class.*/ 
	public final static URI URI_CLASS_URI=createResourceURI(URF_NAMESPACE_URI, "URI");
		//properties
	/**The URI of the property indicating an element of a container such as a set.*/ 
	public final static URI ELEMENT_PROPERTY_URI=createResourceURI(URF_NAMESPACE_URI, "element");
	/**The name of a resource, which may differ from that indicated by the URI, if any.*/
	public final static URI NAME_PROPERTY_URI=createResourceURI(URF_NAMESPACE_URI, "name");
	/**The URI of the URF order property.*/ 
	public final static URI ORDER_PROPERTY_URI=createResourceURI(URF_NAMESPACE_URI, "order");
	/**The URI of the URF type property.*/ 
	public final static URI TYPE_PROPERTY_URI=createResourceURI(URF_NAMESPACE_URI, "type");

		//lexical namespaces
	/**The binary lexical namespace URI.*/
	public final static URI BINARY_NAMESPACE_URI=createLexicalNamespaceURI(BINARY_CLASS_URI);
	/**The boolean lexical namespace URI.*/
	public final static URI BOOLEAN_NAMESPACE_URI=createLexicalNamespaceURI(BOOLEAN_CLASS_URI);
		/**The lexical form of the boolean value <code>false</code>.*/
		public final static String BOOLEAN_FALSE_LEXICAL_FORM=Boolean.FALSE.toString();
		/**The lexical form of the boolean value <code>true</code>.*/
		public final static String BOOLEAN_TRUE_LEXICAL_FORM=Boolean.TRUE.toString();
		/**The URI of the boolean value <code>false</code>.*/
		public final static URI BOOLEAN_FALSE_URI=createLexicalURI(BOOLEAN_CLASS_URI, BOOLEAN_FALSE_LEXICAL_FORM);
		/**The URI of the boolean value <code>true</code>.*/
		public final static URI BOOLEAN_TRUE_URI=createLexicalURI(BOOLEAN_CLASS_URI, BOOLEAN_TRUE_LEXICAL_FORM);
	/**The character lexical namespace URI.*/
	public final static URI CHARACTER_NAMESPACE_URI=createLexicalNamespaceURI(CHARACTER_CLASS_URI);
	/**The integer lexical namespace URI.*/
	public final static URI INTEGER_NAMESPACE_URI=createLexicalNamespaceURI(INTEGER_CLASS_URI);
		/**The URI of the integer value <code>0</code>.*/
		public final static URI INTEGER_0_URI=createLexicalURI(INTEGER_CLASS_URI, Long.toString(0));
	/**The ordinal lexical namespace URI.*/
	public final static URI ORDINAL_NAMESPACE_URI=createLexicalNamespaceURI(ORDINAL_CLASS_URI);
		/**The URI of the ordinal value <code>0</code>.*/
		public final static URI ORDINAL_0_URI=createLexicalURI(ORDINAL_CLASS_URI, Long.toString(0));
	/**The real lexical namespace URI.*/
	public final static URI REAL_NAMESPACE_URI=createLexicalNamespaceURI(REAL_CLASS_URI);
	/**The string lexical namespace URI.*/
	public final static URI STRING_NAMESPACE_URI=createLexicalNamespaceURI(STRING_CLASS_URI);
		/**The URI of the empty string "".*/
		public final static URI EMPTY_STRING_URI=createLexicalURI(STRING_CLASS_URI, "");
	/**The URI lexical namespace URI.*/
	public final static URI URI_NAMESPACE_URI=createLexicalNamespaceURI(URI_CLASS_URI);

	/**The shared empty array of resources.*/
	public final static URFResource[] NO_RESOURCES=new URFResource[0];
	
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
	public static URI createResourceURI(final URI namespaceURI, final String localName)
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

	/**Creates a URI to represent URF binary data.
	@param binary The binary value to represent.
	@return A URI representing the given URF binary data.
	@exception NullPointerException if the given binary data is <code>null</code>.
	@see #BINARY_CLASS_URI
	*/
	public static URI createBinaryURI(final byte[] binary)
	{
		return createLexicalURI(BINARY_CLASS_URI, Base64.encodeBytes(binary, Base64.URL_SAFE&Base64.DONT_BREAK_LINES));	//encode the binary data and create a URI from base64url form
	}

	/**Creates a URI to represent an URF string.
	@param string The string value to represent.
	@return A URI representing the given URF string.
	@exception NullPointerException if the given string is <code>null</code>.
	@see #STRING_CLASS_URI
	*/
	public static URI createStringURI(final String string)
	{
		return string.isEmpty() ? EMPTY_STRING_URI : createLexicalURI(STRING_CLASS_URI, string);	//create a string URI, using the pre-made empty string URI if we can
	}

	/**Creates a URI to represent an URF integer.
	@param integer The integer value to represent.
	@return A URI representing the given URF integer.
	@see #INTEGER_CLASS_URI
	*/
	public static URI createIntegerURI(final long integer)
	{
		return integer==0 ? INTEGER_0_URI : createLexicalURI(INTEGER_CLASS_URI, Long.toString(integer));	//create an integer URI, using the pre-made zero integer URI if we can
	}

	/**Creates a URI to represent an URF ordinal.
	@param ordinal The ordinal value to represent.
	@return A URI representing the given URF ordinal.
	@exception IllegalArgumentException if the given ordinal is negative.
	@see #ORDINAL_CLASS_URI
	*/
	public static URI createOrdinalURI(final long ordinal)
	{
		return ordinal==0 ? ORDINAL_0_URI : createLexicalURI(ORDINAL_CLASS_URI, Long.toString(LongUtilities.checkMinimum(ordinal, 0)));	//create an ordinal URI, using the pre-made zero ordinal URI if we can and making sure that the value is not less than zero
	}

	/**Returns an array containing the URIs of the given resources.
	@param resources The resources of which URIs should be returned.
	@return The URIs of the given resources.
	@exception NullPointerException if one of the given resources is <code>null</code>.
	*/
	public URI[] getURIs(final URFResource... resources)
	{
		final int resourceCount=resources.length;	//find out how many resources there are
		final URI[] uris=new URI[resourceCount];	//create a URI array of appropriate length
		for(int i=0; i<resourceCount; ++i)	//for each resource
		{
			uris[i]=resources[i].getURI();	//get this resource's URI
		}
		return uris;	//return the resource URIs
	}

	/**Determines an object to represent the given URI, if possible 
	This method cna return objects for the resources in the following namespaces:
	<dl>
		<dt>{@value #BINARY_NAMESPACE_URI}</dt> <dd><code>byte[]</code></dd>
		<dt>{@value #BOOLEAN_NAMESPACE_URI}</dt> <dd>{@link Boolean}</dd>
		<dt>{@value #INTEGER_NAMESPACE_URI}</dt> <dd>{@link Long}</dd>
		<dt>{@value #ORDINAL_NAMESPACE_URI}</dt> <dd>{@link Long}</dd>
		<dt>{@value #REAL_NAMESPACE_URI}</dt> <dd>{@link Real}</dd>
		<dt>{@value #STRING_NAMESPACE_URI}</dt> <dd>{@link String}</dd>
		<dt>{@value #URI_NAMESPACE_URI}</dt> <dd>{@link URI}</dd>
	</dl>
	@param resourceURI The URI to represent as an object, or <code>null</code>.
	@return An object representing the resource represented by the given URI, or <code>null</code> if the URI does not represent a known object.
	@exception IllegalArgumentException if the given URI represents an object but does not have the correct syntax for that object.
	*/
	public static Object asObject(final URI resourceURI)
	{
		if(resourceURI!=null)	//if a resource URI was given
		{
			final URI namespaceURI=getNamespaceURI(resourceURI);	//get the URI namespace
			if(namespaceURI!=null)	//if this URI has a namespace
			{
				if(BINARY_NAMESPACE_URI.equals(namespaceURI))	//binary
				{
					return asBinary(resourceURI);	//return an array of bytes
				}
				else if(BOOLEAN_NAMESPACE_URI.equals(namespaceURI))	//boolean
				{
					return asBoolean(resourceURI);	//return a boolean
				}
				else if(CHARACTER_NAMESPACE_URI.equals(namespaceURI))	//character
				{
					return asCharacter(resourceURI);	//create a character value resource
				}
				else if(INTEGER_NAMESPACE_URI.equals(namespaceURI))	//integer
				{
					return asInteger(resourceURI);	//create a long value resource
				}
				else if(ORDINAL_NAMESPACE_URI.equals(namespaceURI))	//ordinal
				{
					return asOrdinal(resourceURI);	//create a long value resource
				}
				else if(REAL_NAMESPACE_URI.equals(namespaceURI))	//real
				{
					return asReal(resourceURI);	//create a double value resource
				}
				else if(STRING_NAMESPACE_URI.equals(namespaceURI))	//string
				{
					return asString(resourceURI);	//create a string value resource
				}
				else if(URI_NAMESPACE_URI.equals(namespaceURI))	//URI
				{
					return asURI(resourceURI);	//create a URI value resource
				}
			}
		}
		return null;	//we can't represent this URI as an object
	}
	
	/**Determines the array object, if any, represented by the given resource.
	@param resource The resource which is expected to represent an array, or <code>null</code>.
	@return The array object represented by the given resource, or <code>null</code> if the resource is not an instance of {@link URFArrayResource}.
	*/
	@SuppressWarnings("unchecked")	//we must trust that they asked for the correct generic type; a class cast exception will be thrown later if the incorrect generic type was requested
	public static <T extends URFResource> URFArrayResource<T> asArrayInstance(final Resource resource)
	{
		return resource instanceof URFArrayResource ? (URFArrayResource<T>)resource : null;	//if an array was given, return it with the requested generic type
	}

	/**Determines the binary data represented by the given resource.
	@param resource The resource which is expected to represent binary data, or <code>null</code>.
	@return The binary data represented by the given resource, or <code>null</code> if the resource does not represent binary data.
	@exception IllegalArgumentException if the given resource represents binary data that does not have the correct syntax.
	*/
	public static byte[] asBinary(final Resource resource)
	{
		return resource!=null ? asBinary(resource.getURI()) : null;	//if a resource was given, see if its URI represents binary data
	}

	/**Determines the binary data represented by the given URI.
	@param resourceURI The URI which is expected to represent binary data, or <code>null</code>.
	@return The binary data represented by the given URI, or <code>null</code> if the URI does not represent binary data.
	@exception IllegalArgumentException if the given URI represents binary data that does not have the correct syntax.
	@see #BINARY_CLASS_URI
	@see #BINARY_NAMESPACE_URI
	*/
	public static byte[] asBinary(final URI resourceURI)
	{
		if(resourceURI!=null && BINARY_NAMESPACE_URI.equals(getNamespaceURI(resourceURI)))	//if a binary URI was given
		{
			final String base64urlString=getLocalName(resourceURI);	//get the base64url-encoded binary data from the local name
			try
			{
				return Base64.decode(base64urlString.getBytes(UTF_8), 0, base64urlString.length(), Base64.URL_SAFE&Base64.DONT_BREAK_LINES);	//decode and return the data
			}
			catch(final UnsupportedEncodingException unsupportedEncodingException)	//the UTF-8 encoding should always be supported
			{
				throw new AssertionError(unsupportedEncodingException);
			}
		}
		return null;	//no boolean could be found
	}

	/**Determines the boolean represented by the given resource.
	@param resource The resource which is expected to represent a boolean, or <code>null</code>.
	@return The boolean represented by the given resource, or <code>null</code> if the resource does not represent a boolean.
	@exception IllegalArgumentException if the given resource represents a boolean that does not have the correct syntax.
	*/
	public static Boolean asBoolean(final Resource resource)
	{
		return resource!=null ? asBoolean(resource.getURI()) : null;	//if a resource was given, see if its URI represents a boolean
	}

	/**Determines the boolean represented by the given URI.
	@param resourceURI The URI which is expected to represent a boolean , or <code>null</code>.
	@return The boolean represented by the given URI, or <code>null</code> if the URI does not represent a boolean.
	@exception IllegalArgumentException if the given URI represents a boolean that does not have the correct syntax.
	@see #BOOLEAN_CLASS_URI
	@see #BOOLEAN_NAMESPACE_URI
	*/
	public static Boolean asBoolean(final URI resourceURI)
	{
		if(resourceURI!=null && BOOLEAN_NAMESPACE_URI.equals(getNamespaceURI(resourceURI)))	//if a boolean URI was given
		{
			return parseBoolean(getLocalName(resourceURI));	//create a boolean from the local name
		}
		return null;	//no boolean could be found
	}

	/**Determines the character represented by the given resource.
	@param resource The resource which is expected to represent a character, or <code>null</code>.
	@return The character represented by the given resource, or <code>null</code> if the resource does not represent a character.
	@exception IllegalArgumentException if the given resource represents a character that does not have the correct syntax.
	*/
	public static Character asCharacter(final Resource resource)
	{
		return resource!=null ? asCharacter(resource.getURI()) : null;	//if a resource was given, see if its URI represents a character
	}

	/**Determines the character represented by the given URI.
	@param resourceURI The URI which is expected to represent a character , or <code>null</code>.
	@return The character represented by the given URI, or <code>null</code> if the URI does not represent a character.
	@exception IllegalArgumentException if the given URI represents a character that does not have the correct syntax.
	@see #CHARACTER_CLASS_URI
	@see #CHARACTER_NAMESPACE_URI
	*/
	public static Character asCharacter(final URI resourceURI)
	{
		if(resourceURI!=null && CHARACTER_NAMESPACE_URI.equals(getNamespaceURI(resourceURI)))	//if a character URI was given
		{
			return parseCharacter(getLocalName(resourceURI));	//create a character from the local name
		}
		return null;	//no boolean could be found
	}

	/**Determines the Java class represented by the given resource.
	A resource represents a Java class if it has a valid <code>info:java/</code> URI with class identifier fragment.
	@param resource The resource which is expected to represent a Java class, or <code>null</code>.
	@return The Java class represented by the given resource, or <code>null</code> if the resource does not represent a Java class.
	@exception IllegalArgumentException if the given resource represents a Java class that does not have the correct syntax.
	@exception ClassNotFoundException if the class represented by the given resource could not be found.
	@see ClassUtilities#asClass(URI)
	*/
	public static Class<?> asClass(final Resource resource) throws ClassNotFoundException
	{
		return resource!=null ? ClassUtilities.asClass(resource.getURI()) : null;	//if a resource was given, see if its URI represents a Java class
	}
	
	/**Determines the Internet media type represented by the given resource.
	A resource represents an Internet media type if it has a valid <code>info:media/</code> URI.
	@param resource The resource which is expected to represent an Internet media type, or <code>null</code>.
	@return The Internet media type represented by the given resource, or <code>null</code> if the resource does not represent an Internet media type.
	@exception IllegalArgumentException if the given resource represents an Internet media type that does not have the correct syntax.
	@see ContentTypeUtilities#asMediaType(URI)
	*/
	public static ContentType asMediaType(final Resource resource)
	{
		return resource!=null ? ContentTypeUtilities.asMediaType(resource.getURI()) : null;	//if a resource was given, see if its URI represents an Internet media type
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
	@see #ORDINAL_CLASS_URI
	@see #ORDINAL_NAMESPACE_URI
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
					return Long.valueOf(Long.parseLong(localName));	//parse a long from the local name
				}
				else if(ORDINAL_NAMESPACE_URI.equals(namespaceURI))	//if this is an ordinal
				{
					return Long.valueOf(Long.parseLong(localName));	//parse a long from the local name
				}
				else if(REAL_NAMESPACE_URI.equals(namespaceURI))	//if this is an real
				{
					return Double.valueOf(Double.parseDouble(localName));	//parse a double from the local name
				}
			}
		}
		return null;	//no number could be found
	}

	/**Determines the integer represented by the given URI.
	@param resourceURI The URI which is expected to represent an integer, or <code>null</code>.
	@return The integer represented by the given URI, or <code>null</code> if the URI does not represent an integer.
	@exception IllegalArgumentException if the given URI represents an integer that does not have the correct syntax.
	@see #INTEGER_CLASS_URI
	@see #INTEGER_NAMESPACE_URI
	*/
	public static Long asInteger(final URI resourceURI)
	{
		if(resourceURI!=null && INTEGER_NAMESPACE_URI.equals(getNamespaceURI(resourceURI)))	//if an integer URI was given
		{
			return Long.valueOf(Long.parseLong(getLocalName(resourceURI)));	//parse a long from the local name
		}
		return null;	//no integer could be found
	}

	/**Determines the ordinal represented by the given URI.
	@param resourceURI The URI which is expected to represent an ordinal, or <code>null</code>.
	@return The ordinal represented by the given URI, or <code>null</code> if the URI does not represent an ordinal.
	@exception IllegalArgumentException if the given URI represents an ordinal that does not have the correct syntax.
	@see #ORDINAL_CLASS_URI
	@see #ORDINAL_NAMESPACE_URI
	*/
	public static Long asOrdinal(final URI resourceURI)
	{
		if(resourceURI!=null && ORDINAL_NAMESPACE_URI.equals(getNamespaceURI(resourceURI)))	//if an ordinal URI was given
		{
			return Long.valueOf(Long.parseLong(getLocalName(resourceURI)));	//parse a long from the local name
		}
		return null;	//no ordinal could be found
	}

	/**Determines the real represented by the given URI.
	@param resourceURI The URI which is expected to represent a real, or <code>null</code>.
	@return The real represented by the given URI, or <code>null</code> if the URI does not represent a real.
	@exception IllegalArgumentException if the given URI represents a real that does not have the correct syntax.
	@see #REAL_CLASS_URI
	@see #REAL_NAMESPACE_URI
	*/
	public static Double asReal(final URI resourceURI)
	{
		if(resourceURI!=null && REAL_NAMESPACE_URI.equals(getNamespaceURI(resourceURI)))	//if a real URI was given
		{
			return Double.parseDouble(getLocalName(resourceURI));	//parse a double from the local name
		}
		return null;	//no real could be found
	}

	/**Determines the string represented by the given resource.
	@param resource The resource which is expected to represent a string, or <code>null</code>.
	@return The string represented by the given resource, or <code>null</code> if the resource does not represent a string.
	@exception IllegalArgumentException if the given resource represents a string that does not have the correct syntax.
	*/
	public static String asString(final Resource resource)
	{
		return resource!=null ? asString(resource.getURI()) : null;	//if a resource was given, see if its URI represents a string
	}

	/**Determines the string represented by the given URI.
	@param resourceURI The URI which is expected to represent a string, or <code>null</code>.
	@return The string represented by the given URI, or <code>null</code> if the URI does not represent a string.
	@exception IllegalArgumentException if the given URI represents a string that does not have the correct syntax.
	@see #STRING_CLASS_URI
	@see #STRING_NAMESPACE_URI
	*/
	public static String asString(final URI resourceURI)
	{
		if(resourceURI!=null && STRING_NAMESPACE_URI.equals(getNamespaceURI(resourceURI)))	//if a string URI was given
		{
			return getLocalName(resourceURI);	//return the local name, which is the string value
		}
		return null;	//no string could be found
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
		if(resourceURI!=null && URI_NAMESPACE_URI.equals(getNamespaceURI(resourceURI)))	//if a URI URI was given
		{
			return URI.create(getLocalName(resourceURI));	//create a URI from the local name
		}
		return null;	//no URI could be found
	}

	/**Converts an URF data model to a string for debugging purposes.
	@param urf The URF data model to represent as a string.
	@return A string representation of the URF data model.
	 */
	public static String toString(final URF urf)
	{
		final StringWriter stringWriter=new StringWriter();	//create a new string writer
		try
		{
			new URFTURFGenerator().generateResources(stringWriter, urf);	//generate TURF from the URF
		}
		catch(final IOException ioException)	//there should never be a problem writing to a string writer
		{
			throw new AssertionError(ioException);
		}
		return stringWriter.toString();	//return the generated string contents
	}
	
	/**Converts an URF resource to a string for debugging purposes.
	@param resource The RDF resource to represent as a string.
	@return A string representation of the URF resource.
	*/
	public static String toString(final URFResource resource)
	{
		final StringWriter stringWriter=new StringWriter();	//create a new string writer
		try
		{
			new URFTURFGenerator().generateResources(stringWriter, resource);	//generate TURF from the resource
		}
		catch(final IOException ioException)	//there should never be a problem writing to a string writer
		{
			throw new AssertionError(ioException);
		}
		return stringWriter.toString();	//return the generated string contents
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

	/**Retrieves the first encountered resource in the data model that is of the requested type.
	If there are more than one resource with the requested type, it is undefined which one will be returned.
	@param typeURI The URI of the type requested.
	@return A resource of the requested type, or <code>null</code> if there are no resourcees with the specified type.
	@exception NullPointerException if the given type URI is <code>null</code>.
	*/
	public URFResource getResourceByType(final URI typeURI)
	{
		for(final URFResource resource:getResources())  //for each resource in this data model
		{
		  if(resource.hasTypeURI(typeURI)) //if this resource is of the requested type
		  {
				return resource;	//return the resource
		  }
		}
		return null;	//indicate that no resources of the given type could be found
	}

	/**Retrieves the resources in the data model that are of the requested type.
	@param typeURI The URI of the type requested.
	@return A read-only iterable of resources that are of the requested type.
	@exception NullPointerException if the given type URI is <code>null</code>.
	*/
	public Iterable<URFResource> getResourcesByType(final URI typeURI)
	{
		final List<URFResource> resourceList=new ArrayList<URFResource>();  //create a list in which to store the resources; because we iterate a set, the gathered resources are ensured not to be duplicated, so storing them in a list is faster then storing them in another set
		for(final URFResource resource:getResources())  //for each resource in this data model
		{
		  if(resource.hasTypeURI(typeURI)) //if this resource is of the requested type
		  {
				resourceList.add(resource); //add this resource to our list
		  }
		}
		return Collections.unmodifiableList(resourceList);  //make the list read-only and return it
	}

	/**Default constructor.*/
	public URF()
	{
		registerResourceFactory(URF_NAMESPACE_URI, DEFAULT_URF_RESOURCE_FACTORY);	//register the default URF resource factory with the URF namespace
		registerResourceFactory(Content.CONTENT_NAMESPACE_URI, Content.DEFAULT_CONTENT_RESOURCE_FACTORY);	//register the default content resource factory with the content namespace
	}

	/**Retreives a resource from the data model based upon a URI.
	If no such resource exists, a resource will be created and added to the data model.
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

	/**Retrieves a resource from the data model based upon the URI of the resource and optional type URIs.
	If no such resource exists, or no resource URI was given, a resource will be created and added to the data model.
	The given type URIs, if any, will be used to locate a resource factory to create the resource, and that type URI may be added as a type property.
	If the resource already exists, no checks are performed to ensure that the existing resource is of the requested type.
	@param resourceURI The URI of the resource to retrieve, or <code>null</code> if the resource should have no URI.
	@param typeURIs The URIs of the known types.
	@return A resource with the given URI.
	*/
	public URFResource locateResource(final URI resourceURI, final URI... typeURIs)
	{
		URFResource resource=resourceURI!=null ? getResource(resourceURI) : null;  //retrieve a resource from the data model if a resource URI was given
		if(resource==null)  //if no such resource exists
		{
			resource=createResource(resourceURI, typeURIs);  //create a new resource of the given types from the given URI and store the resource in the data model
		}
		return resource;  //return the resource we either found or created
	}

	/**Creates an anonymous resource and stores it in this data model.
	@return An anonymous resource.
	*/
	public URFResource createResource()
	{
		return createResource(null);  //create a resource with no URI
	}

	/**Creates a resource with the given URI and type URIs.
	The given type URIs will be used to attempt to find a resource factory to create the resource.
	The returned resource will have no properties, which implies that no type will be indicated for the resource.
	The created resource will be stored in this data model.
	@param resourceURI The URI of the resource to create, or <code>null</code> if the created resource created have no URI.
	@param typeURIs The URIs of the known types.
	@return The resource created with this URI, with the given type added if a type was given.
	@see #DEFAULT_RESOURCE_FACTORY
	@see URFResourceFactory#createResource(URI, URI)
	*/
	public URFResource createResource(final URI resourceURI, final URI... typeURIs)
	{
		URFResourceFactory selectedResourceFactory=DEFAULT_RESOURCE_FACTORY;	//we'll try to find a matching resource factory; if we can't, we'll use the default resource factory
		URI selectedTypeURI=null;	//we'll remember the type URI used for finding the resource factory		
		for(final URI typeURI:typeURIs)	//for each type URI
		{
			final URI typeNamespaceURI=getNamespaceURI(typeURI);	//try to get the namespace of this type
			if(typeNamespaceURI!=null)	//if this type URI is in a namespace
			{
				final URFResourceFactory resourceFactory=getResourceFactory(typeNamespaceURI); //get a resource factory for this namespace
				if(resourceFactory!=null) //if we have a resource factory for this namespace
				{
					selectedResourceFactory=resourceFactory;	//note the resource factory
					selectedTypeURI=typeURI;	//note the type URI
				}
			}
		}
		final URFResource resource=selectedResourceFactory.createResource(resourceURI, selectedTypeURI);	//create a resource from the resource factory, using the selected type URI, if any
		resource.removeProperties();	//remove any properties that the resource factory may have added
		addResource(resource);  //store the resource in the data model
		return resource;  //return the resource we created
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

	/**The shared resource factory for default resources.
	@see DefaultURFResource
	*/
	public final static URFResourceFactory DEFAULT_RESOURCE_FACTORY=new DefaultURFResourceFactory();

	/**The default resource factory for the URF ontology.
	This resource factory can create the following types of resource objects for the given types:
	<dl>
		<dt>Object returned by URF#asObject(URI)</dt> <dd>{@link DefaultURFValueResource}</dd>
		<dt>{@value #ARRAY_CLASS_URI}</dt> <dd>{@link URFArrayResource}</dd>
	</dl>
	@see URF#asObject(URI)
	*/
	public final static DefaultURFResourceFactory DEFAULT_URF_RESOURCE_FACTORY=new DefaultURFResourceFactory()
			{
				/**Creates a resource with the provided URI based upon the type URI, if any.
				If a type URI is provided, a corresponding type property value may be added to the resource before it is returned.
				@param resourceURI The URI of the resource to create, or <code>null</code> if the resource created should be anonymous.
				@param typeURI The URI of the resource type, or <code>null</code> if the type is not known.
				@return The resource created with this URI.
				@exception IllegalArgumentException if a lexical resource URI was given with a different type URI than the specified type URI.
				@see URF#asObject(URI)
				*/
				public URFResource createResource(final URI resourceURI, final URI typeURI)
				{
					if(resourceURI!=null && isLexicalURI(resourceURI))	//if we have a lexical resource URI
					{
						final URI lexicalTypeURI=getLexicalTypeURI(resourceURI);	//get the lexical type of the resource URI
						if(!lexicalTypeURI.equals(typeURI))	//if the given type doesn't equal the lexical type
						{
							throw new IllegalArgumentException("Specified type URI "+typeURI+" doesn't match type URI "+lexicalTypeURI+" of given lexical URI "+resourceURI);
						}
						final Object object=asObject(resourceURI);	//try to get an object for the resource URI
						if(object!=null)	//if we found an object to represent the URI
						{
							return new DefaultURFValueResource<Object>(resourceURI, object);	//create a value resource to represent the object
						}
					}
					if(ARRAY_CLASS_URI.equals(typeURI))	//if this is an array
					{
						return new URFArrayResource<URFResource>(resourceURI);	//create a new array
					}
					return super.createResource(resourceURI, typeURI);	//if we don't recognize the type, create a default resource
				}
			};

}