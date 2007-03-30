package com.garretwilson.rdf.directory.card;

import java.net.URI;
import java.util.Locale;

import static com.garretwilson.lang.JavaUtilities.*;
import static com.garretwilson.lang.ObjectUtilities.*;

import static com.garretwilson.lang.CharSequenceUtilities.*;
import com.garretwilson.rdf.RDFListResource;
import com.garretwilson.rdf.RDFLiteral;
import com.garretwilson.rdf.RDFObject;
import com.garretwilson.rdf.RDFResource;
import com.garretwilson.rdf.directory.Directory;

import static com.garretwilson.rdf.rdfs.RDFSUtilities.*;

import static com.garretwilson.rdf.RDFUtilities.*;
import com.garretwilson.text.directory.DirectorySerializer;
import com.garretwilson.text.directory.vcard.Name;
import com.garretwilson.text.directory.vcard.VCardProfile;
import static com.garretwilson.util.ArrayUtilities.*;
import com.garretwilson.util.LocaleUtilities;
import com.garretwilson.util.NameValuePair;

import static com.garretwilson.text.directory.vcard.VCardConstants.*;

/**An ontology to represent a vCard <code>text/directory</code> profile as defined in <a href="http://www.ietf.org/rfc/rfc2426.txt">RFC 2426</a>, "vCard MIME Directory Profile".
This implementation was created taking advice from <a href="http://www.w3.org/TR/vcard-rdf">Representing vCard Objects in RDF/XML</a>.
@author Garret Wilson
*/
public class Card extends Directory
{

	/**The recommended prefix to the card namespace.*/
	public final static String CARD_NAMESPACE_PREFIX="card";

	/**The URI to the card namespace.*/
	public final static URI CARD_NAMESPACE_URI=URI.create("http://globalmentor.com/namespaces/directory/card#");
	
		//card type names
	/**Specifies the components of the name of the object the card represents. The local name of <code>card:N</code>.*/
	public final static String N_TYPE_NAME=N_TYPE;

		//card property names
	/**Specifies the family name component of the name of the object the card represents. The local name of <code>card:familyName</code>.*/
	public final static String FAMILY_NAME_PROPERTY_NAME="familyName";
	/**Specifies the given name component of the name of the object the card represents. The local name of <code>card:givenName</code>.*/
	public final static String GIVEN_NAME_PROPERTY_NAME="givenName";
	/**Specifies additional name component of the name of the object the card represents. The local name of <code>card:additionalName</code>.*/
	public final static String ADDITIONAL_NAME_PROPERTY_NAME="additionalName";
	/**Specifies the honorific prefix component of the name of the object the card represents. The local name of <code>card:honorificPrefix</code>.*/
	public final static String HONORIFIC_PREFIX_PROPERTY_NAME="honorificPrefix";
	/**Specifies the honorific suffix component of the name of the object the card represents. The local name of <code>card:honorificSuffixes</code>.*/
	public final static String HONORIFIC_SUFFIX_PROPERTY_NAME="honorificSuffix";
	/**Specifies the components of the name of the object the card represents. The local name of <code>card:n</code>.*/
	public final static String N_PROPERTY_NAME=getVariableName(N_TYPE);

	/**The number of components of the card N type.*/
//TODO del if not needed	private final static int N_COMPONENT_COUNT=5;

	/**Sets the {@link #N_PROPERTY_NAME} name information for a resource.
	@param resource The resource the <code>card:n</code> property of which should be set.
	@param name The name information to set.
	@exception NullPointerException if the given resource and/or name is <code>null</code>.
	*/
	public static void setName(final RDFResource resource, final Name name)
	{
		checkInstance(name, "Name cannot be null.");
		final RDFResource nResource=locateTypedResource(resource, null, CARD_NAMESPACE_URI, N_TYPE_NAME);	//create the card:N resource
		final Locale locale=name.getLocale();	//get the name locale, if any
		if(locale!=null)	//if there is a locale
		{
			setLanguage(nResource, locale);	//set the card:N directory:language property
		}
			//create an array of the properties and values to store
		final NameValuePair<URI, String[]>[] propertyValuesPairs=(NameValuePair<URI, String[]>[])new NameValuePair[]
        {
					new NameValuePair<URI, String[]>(createReferenceURI(CARD_NAMESPACE_URI, FAMILY_NAME_PROPERTY_NAME), name.getFamilyNames()),
					new NameValuePair<URI, String[]>(createReferenceURI(CARD_NAMESPACE_URI, GIVEN_NAME_PROPERTY_NAME), name.getGivenNames()),
					new NameValuePair<URI, String[]>(createReferenceURI(CARD_NAMESPACE_URI, ADDITIONAL_NAME_PROPERTY_NAME), name.getAdditionalNames()),
					new NameValuePair<URI, String[]>(createReferenceURI(CARD_NAMESPACE_URI, HONORIFIC_PREFIX_PROPERTY_NAME), name.getHonorificPrefixes()),
					new NameValuePair<URI, String[]>(createReferenceURI(CARD_NAMESPACE_URI, HONORIFIC_SUFFIX_PROPERTY_NAME), name.getHonorificSuffixes())
        };
		for(final NameValuePair<URI, String[]> propertyValuesPair:propertyValuesPairs)	//for all the property value pairs
		{
			final URI propertyURI=propertyValuesPair.getName();	//get the property URI
			final String[] values=propertyValuesPair.getValue();	//get the string values
			final int valueCount=values.length;	//see how many values there are
			if(values.length>0)	//if there is at least one value
			{
				if(valueCount==1)	//if there is only one value
				{
					nResource.setProperty(propertyURI, values[0]);	//set the first value as the single value
				}
				else	//if there is more than one value
				{
					final RDFListResource valueListResource=new RDFListResource(nResource.getRDF());	//create a new list
					for(final String value:values)	//for each value
					{
						final RDFResource valueResource=locateResource(valueListResource, null);	//create a blank node
						setValue(valueResource, value);	//set the value of the value resource
						valueListResource.add(valueResource);	//add the value resource to the list resource
					}
					nResource.setProperty(propertyURI, valueListResource);	//set the list of values as the property value
				}
			}
		}
		resource.setProperty(CARD_NAMESPACE_URI, N_PROPERTY_NAME, nResource);	//set the name property with the name resource we constructed
	}

	/**The property URIs for the name components.*/
	private final static URI[] N_COMPONENT_PROPERTY_URIS=new URI[]
			{
				createReferenceURI(CARD_NAMESPACE_URI, FAMILY_NAME_PROPERTY_NAME),
				createReferenceURI(CARD_NAMESPACE_URI, GIVEN_NAME_PROPERTY_NAME),
				createReferenceURI(CARD_NAMESPACE_URI, ADDITIONAL_NAME_PROPERTY_NAME),
				createReferenceURI(CARD_NAMESPACE_URI, HONORIFIC_PREFIX_PROPERTY_NAME),
				createReferenceURI(CARD_NAMESPACE_URI, HONORIFIC_SUFFIX_PROPERTY_NAME)
		  };
	
	/**Retrieves the {@link #N_PROPERTY_NAME} name information from a resource.
	@param resource The resource the name of which should be retrieved.
	@return The name information, or <code>null</code> if there is no <code>card:n</code> property or the property value is not an {@link RDFResource}.
	@exception NullPointerException if the given resource and/or name is <code>null</code>.
	*/
	public static Name getName(final RDFResource resource)
	{
		final RDFResource nResource=asResource(resource.getPropertyValue(CARD_NAMESPACE_URI, N_PROPERTY_NAME));	//get the name preperty value as a resource
		if(nResource!=null)	//if there is a name resource
		{
			final Locale language=getLanguage(resource);	//get the language specification, if there is one
			final String[][] nameComponentValues=new String[N_COMPONENT_PROPERTY_URIS.length][];	//create arrays of string arrays
			for(int i=nameComponentValues.length-1; i>=0; --i)	//for each name component 
			{
				final RDFObject nComponentObject=resource.getPropertyValue(N_COMPONENT_PROPERTY_URIS[i]);	//get this name component
					//if there is a name component, convert it to one or more LocaleText objects and then convert that to a string; otherwise, use an empty string array
				nameComponentValues[i]=nComponentObject!=null ? toStringArray(getTexts(nComponentObject)) : EMPTY_STRING_ARRAY;
			}			
			return new Name(nameComponentValues[0], nameComponentValues[1], nameComponentValues[2], nameComponentValues[3], nameComponentValues[4], language);	//return a new name from the values we read
		}
		else	//if there is no name resource
		{
			return null;	//indicate that there was no name specified
		}
	}


	/**Creates an array of associates between property URIs and elements of a given name.
	@param name The name
	 * @return
	 */
/*TODO del; not needed
	protected static NameValuePair<URI, String[]>[] creatPropertyValuePairs(final Name name)
	{
		//create an array of the properties and values to store
		return (NameValuePair<URI, String[]>[])new NameValuePair[]
        {
					new NameValuePair<URI, String[]>(createReferenceURI(VCARD_NAMESPACE_URI, FAMILY_NAME_PROPERTY_NAME), name.getFamilyNames()),
					new NameValuePair<URI, String[]>(createReferenceURI(VCARD_NAMESPACE_URI, GIVEN_NAME_PROPERTY_NAME), name.getGivenNames()),
					new NameValuePair<URI, String[]>(createReferenceURI(VCARD_NAMESPACE_URI, ADDITIONAL_NAMES_PROPERTY_NAME), name.getAdditionalNames()),
					new NameValuePair<URI, String[]>(createReferenceURI(VCARD_NAMESPACE_URI, HONORIFIC_PREFIXES_PROPERTY_NAME), name.getHonorificPrefixes()),
					new NameValuePair<URI, String[]>(createReferenceURI(VCARD_NAMESPACE_URI, HONORIFIC_SUFFIXES_PROPERTY_NAME), name.getHonorificSuffixes())
        };
		
	}
*/
	
}
