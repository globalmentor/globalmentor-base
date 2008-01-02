package com.globalmentor.urf.vcard;

import java.net.URI;
import java.util.Locale;

import com.garretwilson.net.URIConstants;
import static com.garretwilson.net.URIConstants.*;
import static com.garretwilson.text.directory.vcard.VCardConstants.*;
import com.garretwilson.text.directory.vcard.Name;
import com.globalmentor.urf.*;

import static com.globalmentor.java.Java.*;
import static com.globalmentor.java.Objects.*;
import static com.globalmentor.urf.URF.*;
import static com.globalmentor.urf.dcmi.DCMI.*;

/**The URF ontology to represent a vCard <code>text/directory</code> profile as defined in
	<a href="http://www.ietf.org/rfc/rfc2426.txt">RFC 2426</a>, "vCard MIME Directory Profile".
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public class VCard
{

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
		final URFResource nResource=new DefaultURFResource((URI)null, N_CLASS_URI);	//create the vcard.N resource
		final Locale locale=name.getLocale();	//get the name locale, if any
		if(locale!=null)	//if there is a locale
		{
			setLanguage(nResource, locale);	//set the vcard:N directory:language property
		}
		nResource.setOrderedPropertyValues(FAMILY_NAME_PROPERTY_URI, name.getFamilyNames());	//set the family names
		nResource.setOrderedPropertyValues(GIVEN_NAME_PROPERTY_URI, name.getGivenNames());	//set the given names
		nResource.setOrderedPropertyValues(ADDITIONAL_NAME_PROPERTY_URI, name.getAdditionalNames());	//set the additional names
		nResource.setOrderedPropertyValues(HONORIFIC_PREFIX_PROPERTY_URI, name.getHonorificPrefixes());	//set the honorific prefixes
		nResource.setOrderedPropertyValues(HONORIFIC_SUFFIX_PROPERTY_URI, name.getHonorificSuffixes());	//set the honorific suffixes
		resource.setPropertyValue(N_PROPERTY_URI, nResource);	//set the name property with the name resource we constructed
	}

	/**Retrieves the «{@link #N_PROPERTY_URI}» name information from a resource.
	@param resource The resource the name of which should be retrieved.
	@return The name information, or <code>null</code> if there is no «{@link #N_PROPERTY_URI}» property or the property value is not an {@link URFResource}.
	@exception NullPointerException if the given resource and/or name is <code>null</code>.
	*/
	public static Name getName(final URFResource resource)
	{
		final URFResource nResource=resource.getPropertyValue(N_PROPERTY_URI);	//get the name property value as a resource
		if(nResource!=null)	//if there is a name resource
		{
			final Locale language=getLanguage(resource);	//get the language specification, if there is one
			final String[] familyNames=asStrings(nResource.getPropertyValues(FAMILY_NAME_PROPERTY_URI));	//get the family names
			final String[] givenNames=asStrings(nResource.getPropertyValues(GIVEN_NAME_PROPERTY_URI));	//get the given names
			final String[] additionalNames=asStrings(nResource.getPropertyValues(ADDITIONAL_NAME_PROPERTY_URI));	//get the additional names
			final String[] honorificPrefixes=asStrings(nResource.getPropertyValues(HONORIFIC_PREFIX_PROPERTY_URI));	//get the honorifix prefixes
			final String[] honorificSuffixes=asStrings(nResource.getPropertyValues(HONORIFIC_SUFFIX_PROPERTY_URI));	//get the honorifix suffixes
			return new Name(familyNames, givenNames, additionalNames, honorificPrefixes, honorificSuffixes, language);	//return a new name from the values we read
		}
		else	//if there is no name resource
		{
			return null;	//indicate that there was no name specified
		}
	}

	/**Determines a formatted name for the given resource.
	The formatted name is determined in this order:
	<ol>
		<li>The lexical form of the literal value of the «{@link #FN_PROPERTY_URI}», if available.</li>
		<li>A formatted string derived from the value of the «{@link #N_PROPERTY_URI}» property, if available.</li>
		<li>The label of the resource as determined by {@link URFResource#determineLabel()}.
	</ol>
	@param resource The resource for which a formatted name should be determined.
	@return The best possible formatted name string for the resource.
	@exception NullPointerException if the given resource is <code>null</code>.
	*/
	public static String getFormattedName(final URFResource resource)
	{
		String formattedName=asString(resource.getPropertyValue(FN_PROPERTY_URI));	//get the vcard.fn value
		if(formattedName==null)	//if there was no vcard.vn property value
		{
			final Name name=getName(resource);	//see if the resource has a name specified
			if(name!=null)	//if there is a name specified
			{
				formattedName=name.toString();	//format the name
			}
			else	//if no name was specified
			{
				formattedName=resource.determineLabel();	//use the resource's label
			}
		}
		return formattedName;	//return whatever formatted name we determined
	}

	/**Sets the email of a resource using the vCard «{@value #EMAIL_PROPERTY_URI}» property.
	@param resource The resource the email of which to set.
	@param emailURI The URI expressing the email to set.
	@exception NullPointerException if the given resource and/or email URI is <code>null</code>.
	@exception IllegalArgumentException if the given email URI does not have a scheme of {@value URIConstants#MAILTO_SCHEME}.
	*/
	public static void setEmail(final URFResource resource, final URI emailURI)
	{
		if(!MAILTO_SCHEME.equals(emailURI.getScheme()))	//if the email doesn't have the mailto URI scheme
		{
			throw new IllegalArgumentException("Email URI "+emailURI+" does not have the "+MAILTO_SCHEME+" scheme.");
		}
		resource.setPropertyValue(EMAIL_PROPERTY_URI, emailURI);	//set the email using the given email URI
	}

}
