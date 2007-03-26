package com.garretwilson.text.directory;

import java.util.*;

import com.garretwilson.util.NameValuePair;

/**Abstract profile for predefined types of a <code>text/directory</code> as 
	defined in <a href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>,
	"A MIME Content-Type for Directory Information".
@author Garret Wilson
*/
public abstract class AbstractProfile implements Profile
{

	/**A map of value type strings keyed to supported type name strings.*/
	private final Map<String, String> typeNameValueTypeMap=new HashMap<String, String>();

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

	/**Determines the value type of the given content line value.
	<p>This version determines the value type by the value name.</p>
	@param profile The profile of this content line, or <code>null</code> if
		there is no profile.
	@param group The group specification, or <code>null</code> if there is no group.
	@param name The name of the information.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>String</code>.
	@return The value type of the content line, or <code>null</code> if the
		value type cannot be determined.
	@see #getValueType(String)
	*/	
	public String getValueType(final String profile, final String group, final String name, final List<NameValuePair<String, String>> paramList)
	{
		return getValueType(name);	//return whatever value type we have associated with this type name, if any
	}
}
