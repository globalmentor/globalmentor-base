package com.garretwilson.text.directory;

import java.util.*;
import com.garretwilson.util.*;

/**Utilities for working with directories of type <code>text/directory</code> as
	defined in 
	<a href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>,
	"A MIME Content-Type for Directory Information".
@author Garret Wilson
*/
public class DirectoryUtilities
{
	
	/**Retrieves a parameter value of the content line.
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

}
