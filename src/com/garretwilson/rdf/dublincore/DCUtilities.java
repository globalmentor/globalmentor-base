package com.garretwilson.rdf.dublincore;

import java.text.*;
import java.util.*;
import com.garretwilson.text.*;
import com.garretwilson.rdf.*;
import com.garretwilson.util.*;

/**Utilities for working with Dublin Core stored in RDF.
@author Garret Wilson
*/
public class DCUtilities implements DCConstants
{

	/**@return A new date format for formatting W3C-style dates.*/
	public final static DateFormat createDateFormat()  //G***put this in an appropriate package
	{
		return new W3CDateFormat(W3CDateFormat.DATE_STYLE);	//return a W3C date and time formatter for just the date 
//G***del		return new SimpleDateFormat(W3C_DATE_FORMAT); //return a new simple date formatter with the W3C date template
	}

	/**Adds a <code>dc:creator</code> property with the given value to the
		resource.
	@param rdf The RDF data model to be used as a property factory.
	@param resource The resource to which the property should be added.
	@param value The property value to add.
	@return The added literal property value.
	*/
	public static RDFLiteral addCreator(final RDF rdf, final RDFResource resource, final String value)
	{
		return RDFUtilities.addProperty(rdf, resource, DCMI11_ELEMENTS_NAMESPACE_URI, DC_CREATOR_PROPERTY_NAME, value);
	}

	/**Adds a <code>dc:contributor</code> property with the given value to the
		resource.
	@param rdf The RDF data model to be used as a property factory.
	@param resource The resource to which the property should be added.
	@param value The property value to add.
	@return The added literal property value.
	*/
	public static RDFLiteral addContributor(final RDF rdf, final RDFResource resource, final String value)
	{
		return RDFUtilities.addProperty(rdf, resource, DCMI11_ELEMENTS_NAMESPACE_URI, DC_CONTRIBUTOR_PROPERTY_NAME, value);
	}

	/**Adds a <code>dc:date</code> property with the given value to the
		resource.
	@param rdf The RDF data model to be used as a property factory.
	@param resource The resource to which the property should be added.
	@param date The property value to add.
	@return The added literal property value.
	*/
	public static RDFLiteral addDate(final RDF rdf, final RDFResource resource, final Date date)
	{
		  //G***fix
//G***del		final DateFormat dateFormat=DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.SHORT); //G***testing
		return RDFUtilities.addProperty(rdf, resource, DCMI11_ELEMENTS_NAMESPACE_URI, DC_DATE_PROPERTY_NAME, createDateFormat().format(date));
	}

	/**Adds a <code>dc:description</code> property with the given value to the
		resource.
	@param rdf The RDF data model to be used as a property factory.
	@param resource The resource to which the property should be added.
	@param value The property value to add.
	@return The added literal property value.
	*/
	public static RDFLiteral addDescription(final RDF rdf, final RDFResource resource, final String value)
	{
		return RDFUtilities.addProperty(rdf, resource, DCMI11_ELEMENTS_NAMESPACE_URI, DC_DESCRIPTION_PROPERTY_NAME, value);
	}

	/**Adds a <code>dc:identifier</code> property with the given value to the
		resource.
	@param rdf The RDF data model to be used as a property factory.
	@param resource The resource to which the property should be added.
	@param value The property value to add.
	@return The added literal property value.
	*/
	public static RDFLiteral addIdentifier(final RDF rdf, final RDFResource resource, final String value)
	{
		return RDFUtilities.addProperty(rdf, resource, DCMI11_ELEMENTS_NAMESPACE_URI, DC_IDENTIFIER_PROPERTY_NAME, value);
	}

	/**Adds a <code>dc:language</code> property with the given value to the
		resource.
	@param rdf The RDF data model to be used as a property factory.
	@param resource The resource to which the property should be added.
	@param locale The property value to add.
	@return The added literal property value.
	*/
	public static RDFLiteral addLanguage(final RDF rdf, final RDFResource resource, final Locale locale)
	{
			//add the literal language tag for this locale
		return RDFUtilities.addProperty(rdf, resource, DCMI11_ELEMENTS_NAMESPACE_URI, DC_LANGUAGE_PROPERTY_NAME, LocaleUtilities.getLanguageTag(locale));
	}

	/**Adds a <code>dc:publisher</code> property with the given value to the
		resource.
	@param rdf The RDF data model to be used as a property factory.
	@param resource The resource to which the property should be added.
	@param value The property value to add.
	@return The added literal property value.
	*/
	public static RDFLiteral addPublisher(final RDF rdf, final RDFResource resource, final String value)
	{
		return RDFUtilities.addProperty(rdf, resource, DCMI11_ELEMENTS_NAMESPACE_URI, DC_PUBLISHER_PROPERTY_NAME, value);
	}

	/**Adds a <code>dc:rights</code> property with the given value to the
		resource.
	@param rdf The RDF data model to be used as a property factory.
	@param resource The resource to which the property should be added.
	@param value The property value to add.
	@return The added literal property value.
	*/
	public static RDFLiteral addRights(final RDF rdf, final RDFResource resource, final String value)
	{
		return RDFUtilities.addProperty(rdf, resource, DCMI11_ELEMENTS_NAMESPACE_URI, DC_RIGHTS_PROPERTY_NAME, value);
	}

	/**Adds a <code>dc:source</code> property with the given value to the
		resource.
	@param rdf The RDF data model to be used as a property factory.
	@param resource The resource to which the property should be added.
	@param value The property value to add.
	@return The added literal property value.
	*/
	public static RDFLiteral addSource(final RDF rdf, final RDFResource resource, final String value)
	{
		return RDFUtilities.addProperty(rdf, resource, DCMI11_ELEMENTS_NAMESPACE_URI, DC_SOURCE_PROPERTY_NAME, value);
	}

	/**Adds a <code>dc:title</code> property with the given value to the
		resource.
	@param rdf The RDF data model to be used as a property factory.
	@param resource The resource to which the property should be added.
	@param value The property value to add.
	@return The added literal property value.
	*/
	public static RDFLiteral addTitle(final RDF rdf, final RDFResource resource, final String value)
	{
		return RDFUtilities.addProperty(rdf, resource, DCMI11_ELEMENTS_NAMESPACE_URI, DC_TITLE_PROPERTY_NAME, value);
	}

	/**Returns the value of the first <code>dc:creator</code> property.
	@param resource The resource the property of which should be located.
	@return The value of the first <code>dc:creator</code> property, or
		<code>null</code> if no such property exists.
	*/
	public static RDFObject getCreator(final RDFResource resource)
	{
		return resource.getPropertyValue(DCMI11_ELEMENTS_NAMESPACE_URI, DC_CREATOR_PROPERTY_NAME);
	}

	/**Returns the value of the first <code>dc:description</code> property.
	@param resource The resource the property of which should be located.
	@return The value of the first <code>dc:description</code> property, or
		<code>null</code> if no such property exists.
	*/
	public static RDFObject getDescription(final RDFResource resource)
	{
		return resource.getPropertyValue(DCMI11_ELEMENTS_NAMESPACE_URI, DC_DESCRIPTION_PROPERTY_NAME);
	}

	/**Returns the value of the first <code>dc:Language</code> property.
	@param resource The resource the property of which should be located.
	@return The value of the first <code>dc:language</code> property, or
		<code>null</code> if no such property exists or the property value does
		not contain a literal language tag.
	*/
	public static Locale getLanguage(final RDFResource resource)
	{
		final RDFObject languageObject=resource.getPropertyValue(DCMI11_ELEMENTS_NAMESPACE_URI, DC_LANGUAGE_PROPERTY_NAME);
		if(languageObject instanceof RDFLiteral)	//if this is a literal value
		{
			return LocaleUtilities.createLocale(((RDFLiteral)languageObject).getLexicalForm());	//create a locale from the literal's lexical form
		}
		else	//if there is no literal language tag
		{
			return null;	//show that we can't create a language locale from a non-literal
		}
	}

	/**Returns the value of the first <code>dc:title</code> property.
	@param resource The resource the property of which should be located.
	@return The value of the first <code>dc:title</code> property, or
		<code>null</code> if no such property exists.
	*/
	public static RDFObject getTitle(final RDFResource resource)
	{
		return resource.getPropertyValue(DCMI11_ELEMENTS_NAMESPACE_URI, DC_TITLE_PROPERTY_NAME);
	}

	/**Returns the value of the first <code>dc:rights</code> property.
	@param resource The resource the property of which should be located.
	@return The value of the first <code>dc:rights</code> property, or
		<code>null</code> if no such property exists.
	*/
	public static RDFObject getRights(final RDFResource resource)
	{
		return resource.getPropertyValue(DCMI11_ELEMENTS_NAMESPACE_URI, DC_RIGHTS_PROPERTY_NAME);
	}

	/**Sets the <code>dc:language</code> property with the given value to the
		resource.
	@param rdf The RDF data model to be used as a property factory.
	@param resource The resource to which the property should be set.
	@param locale The property value to set.
	@return The added literal property value.
	*/
	public static RDFLiteral setLanguage(final RDF rdf, final RDFResource resource, final Locale locale)
	{
			//set the literal language tag for this locale
		return RDFUtilities.setProperty(rdf, resource, DCMI11_ELEMENTS_NAMESPACE_URI, DC_LANGUAGE_PROPERTY_NAME, LocaleUtilities.getLanguageTag(locale));
	}

}