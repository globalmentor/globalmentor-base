package com.garretwilson.text.directory;

import java.util.*;

/**Abstract profile for predefined types of a <code>text/directory</code> as 
	defined in <a href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>,
	"A MIME Content-Type for Directory Information".
@author Garret Wilson
*/
public abstract class AbstractProfile implements Profile
{

	/**A map of value type strings keyed to supported type name strings.*/
	private final Map typeNameValueTypeMap=new HashMap();

		/**Registers a value type keyed to the lowercase version of a type name.
		@param typeName The type name for which a value type should be retrieved.
		@param valueType The value type to associate with this type name.
		*/
		protected void registerValueType(final String typeName, final String valueType)
		{
			typeNameValueTypeMap.put(typeName.toLowerCase(), valueType);	//put the value type in the map, keyed to the lowercase version of the type name		
		}

		/**Returns a value type keyed to the lowercase version of a type name.
		@param typeName The type name for which a value type should be associated.
		@return The value type associated with this type name, or
			<code>null</code> if no value type has been registered with the type name.
		*/
		protected String getValueType(final String typeName)
		{
			return (String)typeNameValueTypeMap.get(typeName.toLowerCase());	//get whatever value type we have associated with this type name, if any
		}
}
