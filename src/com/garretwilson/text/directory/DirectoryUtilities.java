package com.garretwilson.text.directory;

import java.util.*;
import com.garretwilson.util.*;

/**Utilities for working with directories of type <code>text/directory</code> as
	defined in 
	<a href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>,
	"A MIME Content-Type for Directory Information".
@author Garret Wilson
*/
public class DirectoryUtilities implements DirectoryConstants
{
	/**Creates a directory content line from locale text.
	@param profile The profile of this content line, or <code>null</code> if
		there is no profile.
	@param group The group specification, or <code>null</code> if there is no group.
	@param name The name of the information.
	@param localeText The value of the information.
	*/
	public static ContentLine createContentLine(final String profile, final String group, final String name, final LocaleText localeText)
	{
		return createContentLine(profile, group, name, localeText, localeText.getLocale());	//create and return a content line from the locale text and the locale
	}

	/**Creates a directory content line with the given language parameter.
	@param profile The profile of this content line, or <code>null</code> if
		there is no profile.
	@param group The group specification, or <code>null</code> if there is no group.
	@param name The name of the information.
	@param value The value of the information.
	@param locale The value to give to the language parameter, or
		<code>null</code> if no language should be specified. 
	@see #setLanguageParamValue
	*/
	public static ContentLine createContentLine(final String profile, final String group, final String name, final Object value, final Locale locale)
	{
		final ContentLine contentLine=new ContentLine(profile, group, name, value);	//create a content line with the value
		if(locale!=null)	//if a locale was specified
			setLanguageParamValue(contentLine.getParamList(), locale);	//set the language of the content line
		return contentLine;	//return the content line we created
	}

	/**Creates a <code>LocaleText</code> by combining the language param, if
		present, with the string value of the given object.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>String</code>.
	@param value The content line value.
	@return An object representing the text and locale of the value
	*/
	public static LocaleText createLocaleTextValue(final List paramList, final Object value)
	{
		final Locale locale=getLanguageParamValue(paramList);	//get the locale
		return new LocaleText(value.toString(), locale);	//create and return the locale text
	}

	/**Retrieves the value of the first language parameter as a <code>Locale</code>.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>String</code>.
	@return A locale representing the given language, or <code>null</code> if
		no language is indicated.
	*/
	public static Locale getLanguageParamValue(final List paramList)
	{
		final String languageValue=getParamValue(paramList, LANGUAGE_PARAM_NAME);	//get the first language parameter
		return languageValue!=null && languageValue.trim().length()>0	//if there is a language and it isn't just whitespace 
			? LocaleUtilities.createLocale(languageValue.trim())	//create a locale from the language value
			: null;	//show that there was no language
	}
	
	/**Retrieves the first value of a parameter with the given name.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>String</code>.
	@param paramName The name of the parameter, which will be matched against
		available parameters in a case insensitive way.
	@return The value of the first matching parameter, or <code>null</code> if
		there is no matching parameter. 
	*/
	public static String getParamValue(final List paramList, final String paramName)
	{
		final Iterator paramIterator=paramList.iterator();	//get an iterator to the parameters
		while(paramIterator.hasNext())	//while there are more parameters
		{
			final NameValuePair parameter=(NameValuePair)paramIterator.next();	//get the next parameter name/value pair
			if(paramName.equals(parameter.getName()))	//if this is the correct parameter
			{
				return (String)parameter.getValue();	//return the parameter value
			}
		}
		return null;	//show that we could not find a matching parameter
	}

	/**Retrieves the values of all parameters with the given name.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>String</code>.
	@param paramName The name of the parameter, which will be matched against
		available parameters in a case insensitive way.
	@return The values of all matching parameters. 
	*/
	public static String[] getParamValues(final List paramList, final String paramName)
	{
		final List paramValueList=new ArrayList(paramList.size());	//create a list to hold parameters, knowing we won't need room for more parameters than the we were given
		final Iterator paramIterator=paramList.iterator();	//get an iterator to the parameters
		while(paramIterator.hasNext())	//while there are more parameters
		{
			final NameValuePair parameter=(NameValuePair)paramIterator.next();	//get the next parameter name/value pair
			if(paramName.equals(parameter.getName()))	//if this is the correct parameter
			{
				paramValueList.add((String)parameter.getValue());	//return the parameter value
			}
		}
		return (String[])paramValueList.toArray(new String[paramValueList.size()]);	//return an array version of the list of values
	}

	/**An iterator to stop through all parameters with a given name.*/
/*G***del; this is too complicated, and wouldn't save us from creating an object 
	private static ParamIterator implements Iterator
	{

		
		
	}
*/

	/**Removes all parameters with the given name.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>String</code>.
	@param paramName The name of the parameter, which will be matched against
		available parameters in a case insensitive way.
	*/
	public static void removeParams(final List paramList, final String paramName)
	{
		final Iterator paramIterator=paramList.iterator();	//get an iterator to the parameters
		while(paramIterator.hasNext())	//while there are more parameters
		{
			final NameValuePair parameter=(NameValuePair)paramIterator.next();	//get the next parameter name/value pair
			if(paramName.equals(parameter.getName()))	//if this is the correct parameter
			{
				paramIterator.remove();	//remove this parameter
			}
		}
	}

	/**Sets the language parameter to the value of a <code>Locale</code>.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>String</code>.
	@param locale The value to give to the language parameter. 
	*/
	public static void setLanguageParamValue(final List paramList, final Locale locale)
	{
		setParamValue(paramList, LANGUAGE_PARAM_NAME, LocaleUtilities.getLanguageTag(locale));	//store the language tag representation of the locale as the value of the language parameter 
	}

	/**Removes all parameters with the given name and adds a new
		parameter with the given value.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>String</code>.
	@param paramName The name of the parameter, which will be matched against
		available parameters in a case insensitive way.
	@param paramValue The value to give to the added parameter. 
	@see #removeParams
	@see #addParam
	*/
	public static void setParamValue(final List paramList, final String paramName, final String paramValue)
	{
		removeParams(paramList, paramName);	//remove all parameters with the given name
		addParam(paramList, paramName, paramValue);	//add the param name and value
	}

	/**Adds a new	parameter with the given value.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>String</code>.
	@param paramName The name of the parameter, which will be matched against
		available parameters in a case insensitive way.
	@param paramValue The value to give to the added parameter. 
	*/
	public static void addParam(final List paramList, final String paramName, final String paramValue)
	{
		paramList.add(new NameValuePair(paramName, paramValue));	//create a name value pair with the given name and value
	}

}
