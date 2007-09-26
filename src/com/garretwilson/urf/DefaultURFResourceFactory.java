package com.garretwilson.urf;

import java.net.URI;

import static com.garretwilson.net.URIConstants.*;
import static com.garretwilson.urf.URF.*;

/**A default factory to create default resources.
This factory also has static methods to create default resources of several lexical types.
@author Garret Wilson
*/
public class DefaultURFResourceFactory implements URFResourceFactory
{

	/**Creates a resource with the provided URI based upon the type URI, if any.
	If a type URI is provided, a corresponding type property value may be added to the resource before it is returned.
	This implementation returns a {@link DefaultURFResource}.
	@param resourceURI The URI of the resource to create, or <code>null</code> if the resource created should be anonymous.
	@param typeURI The URI of the resource type, or <code>null</code> if the type is not known.
	@return The resource created with this URI.
	*/
	public URFResource createResource(final URI resourceURI, final URI typeURI)
	{
		return new DefaultURFResource(resourceURI, typeURI!=null ? new URI[]{typeURI} : NO_URIS);	//create and return a default resource with the type added, if any
	}

	/**Creates a default resource with a URI in a lexical namespace for the given resource type and lexical form.
	The indicated type is added as one of the resource's type property.
	@param typeURI The URI of the type of the resource.
	@param lexicalForm The canonical lexical form of the resource.
	@return A resource with the URI in the lexical namespace for the specified type based upon its lexical form.
	@exception NullPointerException if the given type URI and/or lexical form is <code>null</code>.
	*/
	public final static URFResource createLexicalResource(final URI typeURI, final String lexicalForm)
	{
		return new DefaultURFResource(createLexicalURI(typeURI, lexicalForm), typeURI);	//create a new resource from the appropriate lexical URI and add the indicated type
	}

	/**Creates a default string resource with its type added as a type property.
	@param string The string for which a default resource should be created.
	@return A default string resource with the appropriate type property added.
	@exception NullPointerException if the given string is <code>null</code>.
	*/
	public final static URFResource createResource(final String string)
	{
		return createLexicalResource(STRING_CLASS_URI, string);	//create and return a default string resource
	}
}