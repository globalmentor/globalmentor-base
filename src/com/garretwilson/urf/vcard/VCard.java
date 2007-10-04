package com.garretwilson.urf.vcard;

import java.net.URI;
import java.util.Locale;

import static com.garretwilson.lang.JavaUtilities.*;
import static com.garretwilson.lang.ObjectUtilities.checkInstance;

import com.garretwilson.net.URIConstants;
import static com.garretwilson.net.URIConstants.*;
import static com.garretwilson.rdf.RDFUtilities.asResource;
import static com.garretwilson.rdf.RDFUtilities.createReferenceURI;
import static com.garretwilson.rdf.RDFUtilities.getDisplayLabel;
import static com.garretwilson.rdf.RDFUtilities.locateResource;
import static com.garretwilson.rdf.RDFUtilities.locateTypedResource;
import static com.garretwilson.rdf.RDFUtilities.setValue;
import static com.garretwilson.text.directory.vcard.VCardConstants.*;

import com.garretwilson.rdf.RDFListResource;
import com.garretwilson.rdf.RDFLiteral;
import com.garretwilson.rdf.RDFObject;
import com.garretwilson.rdf.RDFResource;
import com.garretwilson.rdf.RDFUtilities;
import com.garretwilson.text.directory.vcard.Name;
import com.garretwilson.urf.DefaultURFResource;
import com.garretwilson.urf.URFResource;
import static com.garretwilson.urf.dcmi.DCMI.*;
import com.garretwilson.util.NameValuePair;

import static com.garretwilson.urf.URF.*;
import static com.garretwilson.util.ArrayUtilities.EMPTY_STRING_ARRAY;
import static com.garretwilson.util.ArrayUtilities.toStringArray;

/**The URF ontology to represent a vCard <code>text/directory</code> profile as defined in
	<a href="http://www.ietf.org/rfc/rfc2426.txt">RFC 2426</a>, "vCard MIME Directory Profile".
@author Garret Wilson
*/
public class VCard
{

	/**The recommended prefix to the VCard namespace.*/
	public final static String VCARD_NAMESPACE_PREFIX="vcard";
	/**The URI to the VCard namespace.*/
	public final static URI VCARD_NAMESPACE_URI=URI.create("http://urf.name/vcard");
	
		//classes 
	/**Specifies the components of the name of the object the vCard represents.*/
	public final static URI N_CLASS_URI=createResourceURI(VCARD_NAMESPACE_URI, N_TYPE);

		//properties
	/**Specifies the electronic mail address for communication with the object the vCard represents.*/
	public final static URI EMAIL_PROPERTY_URI=createResourceURI(VCARD_NAMESPACE_URI, getVariableName(EMAIL_TYPE));
	/**Specifies the formatted text corresponding to the name of the object the vCard represents.*/
	public final static URI FN_PROPERTY_URI=createResourceURI(VCARD_NAMESPACE_URI, getVariableName(FN_TYPE));
	/**Specifies the components of the name of the object the vCard represents.*/
	public final static URI N_PROPERTY_URI=createResourceURI(VCARD_NAMESPACE_URI, getVariableName(N_TYPE));
		/**Specifies the family name component of the name of the object the vCard represents.*/
		public final static URI FAMILY_NAME_PROPERTY_URI=createResourceURI(VCARD_NAMESPACE_URI, "familyName");
		/**Specifies the given name component of the name of the object the vCard represents.*/
		public final static URI GIVEN_NAME_PROPERTY_URI=createResourceURI(VCARD_NAMESPACE_URI, "givenName");
		/**Specifies additional name component of the name of the object the vCard represents.*/
		public final static URI ADDITIONAL_NAME_PROPERTY_URI=createResourceURI(VCARD_NAMESPACE_URI, "additionalName");
		/**Specifies the honorific prefix component of the name of the object the vCard represents.*/
		public final static URI HONORIFIC_PREFIX_PROPERTY_URI=createResourceURI(VCARD_NAMESPACE_URI, "honorificPrefix");
		/**Specifies the honorific suffix component of the name of the object the vCard represents.*/
		public final static URI HONORIFIC_SUFFIX_PROPERTY_URI=createResourceURI(VCARD_NAMESPACE_URI, "honorificSuffix");

		
	/**Sets the name information for a resource.
	@param resource The resource the <code>vcard.n</code> property of which should be set.
	@param name The name information to set.
	@exception NullPointerException if the given resource and/or name is <code>null</code>.
	*/
	public static void setName(final URFResource resource, final Name name)
	{
		checkInstance(name, "Name cannot be null.");
		final URFResource nResource=new DefaultURFResource(null, N_CLASS_URI);	//create the vcard.N resource
		final Locale locale=name.getLocale();	//get the name locale, if any
		if(locale!=null)	//if there is a locale
		{
			setLanguage(nResource, locale);	//set the vcard:N directory:language property
		}
			//create an array of the properties and values to store
		final NameValuePair<URI, String[]>[] propertyValuesPairs=(NameValuePair<URI, String[]>[])new NameValuePair[]
        {
					new NameValuePair<URI, String[]>(FAMILY_NAME_PROPERTY_URI, name.getFamilyNames()),
					new NameValuePair<URI, String[]>(GIVEN_NAME_PROPERTY_URI, name.getGivenNames()),
					new NameValuePair<URI, String[]>(ADDITIONAL_NAME_PROPERTY_URI, name.getAdditionalNames()),
					new NameValuePair<URI, String[]>(HONORIFIC_PREFIX_PROPERTY_URI, name.getHonorificPrefixes()),
					new NameValuePair<URI, String[]>(HONORIFIC_SUFFIX_PROPERTY_URI, name.getHonorificSuffixes())
        };
		for(final NameValuePair<URI, String[]> propertyValuesPair:propertyValuesPairs)	//for all the property value pairs
		{
			final URI propertyURI=propertyValuesPair.getName();	//get the property URI
			final String[] values=propertyValuesPair.getValue();	//get the string values
			final int valueCount=values.length;	//see how many values there are
			if(values.length>0)	//if there is at least one value
			{
				
				
				
//TODO add sequence support to URF API
				
				if(valueCount==1)	//if there is only one value
				{
					nResource.setPropertyValue(propertyURI, values[0]);	//set the first value as the single value
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
		resource.setProperty(VCARD_NAMESPACE_URI, N_PROPERTY_NAME, nResource);	//set the name property with the name resource we constructed
	}

	/**The property URIs for the name components.*/
	private final static URI[] N_COMPONENT_PROPERTY_URIS=new URI[]
			{
				createReferenceURI(VCARD_NAMESPACE_URI, FAMILY_NAME_PROPERTY_NAME),
				createReferenceURI(VCARD_NAMESPACE_URI, GIVEN_NAME_PROPERTY_NAME),
				createReferenceURI(VCARD_NAMESPACE_URI, ADDITIONAL_NAME_PROPERTY_NAME),
				createReferenceURI(VCARD_NAMESPACE_URI, HONORIFIC_PREFIX_PROPERTY_NAME),
				createReferenceURI(VCARD_NAMESPACE_URI, HONORIFIC_SUFFIX_PROPERTY_NAME)
		  };
	
	/**Retrieves the {@link #N_PROPERTY_NAME} name information from a resource.
	@param resource The resource the name of which should be retrieved.
	@return The name information, or <code>null</code> if there is no <code>vcard:n</code> property or the property value is not an {@link RDFResource}.
	@exception NullPointerException if the given resource and/or name is <code>null</code>.
	*/
	public static Name getName(final RDFResource resource)
	{
		final RDFResource nResource=asResource(resource.getPropertyValue(VCARD_NAMESPACE_URI, N_PROPERTY_NAME));	//get the name preperty value as a resource
		if(nResource!=null)	//if there is a name resource
		{
			final Locale language=getLanguage(resource);	//get the language specification, if there is one
			final String[][] nameComponentValues=new String[N_COMPONENT_PROPERTY_URIS.length][];	//create arrays of string arrays
			for(int i=nameComponentValues.length-1; i>=0; --i)	//for each name component 
			{
				final RDFObject nComponentObject=nResource.getPropertyValue(N_COMPONENT_PROPERTY_URIS[i]);	//get this name component
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

	/**Determines a formatted name for the given resource.
	The formatted name is determined in this order:
	<ol>
		<li>The lexical form of the literal value of the <code>vcard:fn</code> property, if available.</li>
		<li>A formatted string derived from the value of the <code>vcard:n</code> property, if available.</li>
		<li>The label of the resource as determined by {@link RDFUtilities#getDisplayLabel(RDFResource)}.
	</ol>
	@param resource The resource for which a formatted name should be determined.
	@return The best possible formatted name string for the resource.
	@exception NullPointerException if the given resource is <code>null</code>.
	*/
	public static String getFormattedName(final RDFResource resource)
	{
		final RDFObject fnObject=resource.getPropertyValue(VCARD_NAMESPACE_URI, FN_PROPERTY_NAME);	//get the vcard:fn value
		if(fnObject instanceof RDFLiteral)	//if the object is a literal
		{
			return ((RDFLiteral)fnObject).getLexicalForm();	//return the lexical form of the literal vcard:fn value
		}
		final Name name=getName(resource);	//otherwise, see if the resource has a name specified
		if(name!=null)	//if there is a name specified
		{
			return name.toString();	//format the name and return it
		}
		return getDisplayLabel(resource);	//if all else fails, just return a label for the resource
	}

		
		
	/**Sets the email of a resource using the vCard {@value #EMAIL_PROPERTY_URI} property.
	@param resource The resource the email of which to set.
	@param emailURI The URI expressing the email to set.
	@exception NullPointerException if the given resource and/or email URI is <code>null</code>.
	@exception IllegalArgumentException if the given email URI does not have a scheme of {@value URIConstants#MAILTO_SCHEME}.
	*/
	public static void setEmail(final URFResource resource, final URI emailURI)
	{
		if(emailURI.getScheme().equals(MAILTO_SCHEME))	//if the email doesn't have the mailto URI scheme
		{
			throw new IllegalArgumentException("Email URI "+emailURI+" does not have the "+MAILTO_SCHEME+" scheme.");
		}
		resource.setPropertyValue(VCARD_NAMESPACE_URI, emailURI);	//set the email using the given email URI
	}

}
