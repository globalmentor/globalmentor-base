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
}
