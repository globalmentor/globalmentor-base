package com.garretwilson.text.directory.vcard;

import java.util.*;

import static com.garretwilson.lang.ObjectUtilities.*;
import static com.garretwilson.text.FormatUtilities.*;

import com.garretwilson.util.DefaultLocaleable;

/**An object representing the "N" type of a vCard <code>text/directory</code>
	profile as defined in <a href="http://www.ietf.org/rfc/rfc2426.txt">RFC 2426</a>,
	"vCard MIME Directory Profile".
@author Garret Wilson
*/
public class Name extends DefaultLocaleable
{

	/**The family names.*/
	private String[] familyNames;

		/**@return The family names.*/
		public String[] getFamilyNames() {return familyNames;}

		/**@return The first family name, or <code>null</code> if there are no family names.*/
		public String getFamilyName() {return familyNames.length>0 ? familyNames[0] : null;}
		
		/**Sets the family names.
		@param familyNames The family names.
		@exception NullPointerException if the provided family names is <code>null</code>.
		*/
		public void setFamilyNames(final String... familyNames) {this.familyNames=checkInstance(familyNames, "Family names cannot be null.");}
		
	/**The given names.*/
	private String[] givenNames;

		/**@return The given names.*/
		public String[] getGivenNames() {return givenNames;}

		/**@return The first given name, or <code>null</code> if there are no given names.*/
		public String getGivenName() {return givenNames.length>0 ? givenNames[0] : null;}
		
		/**Sets the given names.
		@param givenNames The given names.
		@exception NullPointerException if the provided given names is <code>null</code>.
		*/
		public void setGivenNames(final String... givenNames) {this.givenNames=checkInstance(givenNames, "Given names cannot be null.");}
		
	/**The additional names.*/
	private String[] additionalNames;

		/**@return The additional names.*/
		public String[] getAdditionalNames() {return additionalNames;}
		
		/**@return The first additional name, or <code>null</code> if there are no additional names.*/
		public String getAdditionalName() {return additionalNames.length>0 ? additionalNames[0] : null;}

		/**Sets the additional names.
		@param additionalNames The additional names.
		@exception NullPointerException if the provided additional names is <code>null</code>.
		*/
		public void setAdditionalNames(final String... additionalNames) {this.additionalNames=checkInstance(additionalNames, "Additional names cannot be null");}
		
	/**The honorific prefixes.*/
	private String[] honorificPrefixes;

		/**@return The honorific prefixes.*/
		public String[] getHonorificPrefixes() {return honorificPrefixes;}

		/**@return The first honorific prefix, or <code>null</code> if there are no honorific prefixes.*/
		public String getHonorificPrefix() {return honorificPrefixes.length>0 ? honorificPrefixes[0] : null;}

		/**Sets the honorific prefixes.
		@param honorificPrefixes The honorific prefixes.
		@exception NullPointerException if the provided honorific prefixes is <code>null</code>.
		*/
		public void setHonorificPrefixes(final String... honorificPrefixes) {this.honorificPrefixes=checkInstance(honorificPrefixes, "Honorific prefixes cannot be null.");}

	/**The honorific suffixes.*/
	private String[] honorificSuffixes;

		/**@return The honorific suffixes.*/
		public String[] getHonorificSuffixes() {return honorificSuffixes;}

		/**@return The first honorific suffix, or <code>null</code> if there are no honorific suffixes.*/
		public String getHonorificSuffix() {return honorificSuffixes.length>0 ? honorificSuffixes[0] : null;}
		
		/**Sets the honorific suffixes.
		@param honorificSuffixes The honorific suffixes.
		@exception NullPointerException if the provided honorific suffixes is <code>null</code>.
		*/
		public void setHonorificSuffixes(final String... honorificSuffixes) {this.honorificSuffixes=checkInstance(honorificSuffixes, "Honorific suffixes cannot be null.");}
		
	/**Name constructor.
	@param familyNames The family names.
	@param givenNames The given names.
	*/
	public Name(final String[] familyNames, final String[] givenNames)
	{
		this(familyNames, givenNames, new String[]{});	//construct the name with no additional names
	}

	/**Additional constructor.
	@param familyNames The family names.
	@param givenNames The given names.
	@param additionalNames The additional names.
	*/
	public Name(final String[] familyNames, final String[] givenNames, final String[] additionalNames)
	{
		this(familyNames, givenNames, additionalNames, new String[]{}, new String[]{});	//construct the name with no prefixes or suffixes
	}

	/**Full name constructor.
	@param familyNames The family names.
	@param givenNames The given names.
	@param additionalNames The additional names.
	@param honorificPrefix The honorific prefixes.
	@param honorificSuffixes The honorific suffixes.
	*/
	public Name(final String[] familyNames, final String[] givenNames, final String[] additionalNames, final String[] honorificPrefixes, final String[] honorificSuffixes)
	{
		this(familyNames, givenNames, additionalNames, honorificPrefixes, honorificSuffixes, null);	//construct the name with no locale	
	}

	/**Full constructor.
	@param familyNames The family names.
	@param givenNames The given names.
	@param additionalNames The additional names.
	@param honorificPrefix The honorific prefixes.
	@param honorificSuffixes The honorific suffixes.
	@param locale The locale that represents the language of the text, or <code>null</code> if no language should be indicated.
	*/
	public Name(final String[] familyNames, final String[] givenNames, final String[] additionalNames, final String[] honorificPrefixes, final String[] honorificSuffixes, final Locale locale)
	{
		super(locale);	//construct the parent class
		setFamilyNames(familyNames);	//set the names
		setGivenNames(givenNames);
		setAdditionalNames(additionalNames);
		setHonorificPrefixes(honorificPrefixes);
		setHonorificSuffixes(honorificSuffixes);
	}

	/**Single constructor.
	@param familyName The family name, or <code>null</code> for no family name.
	@param givenName The given name, or <code>null</code> for no given name.
	*/
	public Name(final String familyName, final String givenName)
	{
		this(familyName, givenName, null);	//construct the name with no additional name
	}

	/**Additional single constructor.
	@param familyName The family name, or <code>null</code> for no family name.
	@param givenName The given name, or <code>null</code> for no given name.
	@param additionalName The additional name, or <code>null</code> for no additional name.
	*/
	public Name(final String familyName, final String givenName, final String additionalName)
	{
		this(familyName, givenName, additionalName, null, null);	//construct the name with no prefix or suffix
	}

	/**Full name constructor.
	@param familyName The family name, or <code>null</code> for no family name.
	@param givenName The given name, or <code>null</code> for no given name.
	@param additionalName The additional name, or <code>null</code> for no additional name.
	@param honorificPrefix The honorific prefix, or <code>null</code> for no honorific prefix.
	@param honorificSuffix The honorific suffix, or <code>null</code> for no honorific suffix.
	*/
	public Name(final String familyName, final String givenName, final String additionalName, final String honorificPrefix, final String honorificSuffix)
	{
		this(familyName, givenName, additionalName, honorificPrefix, honorificSuffix, null);	//construct the name with no locale	
	}

	/**Full single constructor.
	@param familyName The family name, or <code>null</code> for no family name.
	@param givenName The given name, or <code>null</code> for no given name.
	@param additionalName The additional name, or <code>null</code> for no additional name.
	@param honorificPrefix The honorific prefix, or <code>null</code> for no honorific prefix.
	@param honorificSuffix The honorific suffix, or <code>null</code> for no honorific suffix.
	@param locale The locale that represents the language of the text, or
		<code>null</code> if no language should be indicated.
	*/
	public Name(final String familyName, final String givenName, final String additionalName, final String honorificPrefix, final String honorificSuffix, final Locale locale)
	{
		super(locale);	//construct the parent class
		setFamilyNames(familyName);	//set the names
		setGivenNames(givenName);
		setAdditionalNames(additionalName);
		setHonorificPrefixes(honorificPrefix);
		setHonorificSuffixes(honorificSuffix);
	}

	/**@return A string representation of the name.*/
	public String toString()
	{
		final StringBuilder stringBuilder=new StringBuilder();	//create a new string builder to hold the string we'll construct
		formatList(stringBuilder, ',', honorificPrefixes);	//append the honorific prefixes, separated by a comma
		if(honorificPrefixes.length>0 && (givenNames.length>0 || additionalNames.length>0 || familyNames.length>0 || honorificSuffixes.length>0))	//if we added information and there is more information following
			stringBuilder.append(' ');	//append a space
		formatList(stringBuilder, '/', givenNames);	//append the given names, separated by a slash
		if(givenNames.length>0 && (additionalNames.length>0 || familyNames.length>0 || honorificSuffixes.length>0))	//if we added information and there is more information following
			stringBuilder.append(' ');	//append a space
		formatList(stringBuilder, ", ", (Object[])additionalNames);	//append the additional names, separated by a comma
		if(additionalNames.length>0 && (familyNames.length>0 || honorificSuffixes.length>0))	//if we added information and there is more information following
			stringBuilder.append(' ');	//append a space
		formatList(stringBuilder, '/', familyNames);	//append the family names, separated by a slash
		if(stringBuilder.length()>0 && honorificSuffixes.length>0)	//if we have any content before the suffixes, and there are suffixes
			stringBuilder.append(',');		//add a comma before the suffixes
		if(familyNames.length>0 && (honorificSuffixes.length>0))	//if we added information and there is more information following
			stringBuilder.append(' ');	//append a space
		formatList(stringBuilder, ", ", (Object[])honorificSuffixes);	//append the honorific suffixes, separated by a comma
		return stringBuilder.toString();	//return the string we constructed
	}
}
