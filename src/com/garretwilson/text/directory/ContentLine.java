package com.garretwilson.text.directory;

import java.util.*;
import com.garretwilson.util.*;

/**A line in a directory of type <code>text/directory</code> as defined in
	<a href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>,
	"A MIME Content-Type for Directory Information".
@author Garret Wilson
*/
public class ContentLine extends NameValuePair implements DirectoryConstants
{

	/**The profile of this content line, or <code>null</code> if there is no profile.*/
	private String profile=null;	
	
		/**@return The profile of this content line, or <code>null</code> if there is no profile.*/
		public String getProfile() {return profile;}

		/**Sets the profile.
		@param profile The profile of this content line, or <code>null</code> if there is no profile.
		*/
		public void setProfile(final String profile) {this.profile=profile;}

	/**The group specification, or <code>null</code> if there is no group.*/
	private String group=null;	
	
		/**@return The group specification, or <code>null</code> if there is no group.*/
		public String getGroup() {return group;}

		/**Sets the group.
		@param group The group specification, or <code>null</code> if there is no group.
		*/
		public void setGroup(final String group) {this.group=group;}
	
	/**@return The type name of this content line as a string.
	Convenience method for <code>getName()</code>.
	@see NameValuePair#getName
	*/
	public String getTypeName() {return (String)getName();}
			
	/**The list of parameters.*/
	private final List paramList;

		/**@return The list of parameters, each item of which is a
			<code>NameValuePair</code> with a name of type <code>String</code> and a
			value of type <code>Object</code>.
		@see NameValuePair
		*/
		public List getParamList() {return paramList;}

	/**Creates a directory content line with a name and value.
	@param name The name of the information.
	@param value The value of the information.
	*/
	public ContentLine(final String name, final Object value)
	{
		this(null, name, value);	//create a content line with no group
	}

	/**Creates a directory content line with a group, name, and value.
	@param group The group specification, or <code>null</code> if there is no group.
	@param name The name of the information.
	@param value The value of the information.
	*/
	public ContentLine(final String group, final String name, final Object value)
	{
		this(null, group, name, value);	//create a content line with no profile
	}

	/**Creates a directory content line with a group, name, parameter list, and value.
	@param group The group specification, or <code>null</code> if there is no group.
	@param name The name of the information.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>Object</code>.
	@param value The value of the information.
	*/
	public ContentLine(final String group, final String name, final List paramList, final Object value)
	{
		this(null, group, name, paramList, value);	//create a content line with no profile
	}

	/**Creates a directory content line with a profile, group, name, and value.
	@param profile The profile of this content line, or <code>null</code> if
		there is no profile.
	@param group The group specification, or <code>null</code> if there is no group.
	@param name The name of the information.
	@param value The value of the information.
	*/
	public ContentLine(final String profile, final String group, final String name, final Object value)
	{
		this(profile, group, name, new ArrayList(), value);	//create a content line with an empty param list
	}
	
	/**Creates a directory content line with a profile, group, name, parameter list, and value.
	@param profile The profile of this content line, or <code>null</code> if
		there is no profile.
	@param group The group specification, or <code>null</code> if there is no group.
	@param name The name of the information.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>Object</code>.
	@param value The value of the information.
	*/
	public ContentLine(final String profile, final String group, final String name, final List paramList, final Object value)
	{
		super(name, value);	//construct the parent class
		this.profile=profile;	//save the profile
		this.group=group;	//save the group
		this.paramList=paramList;	//save the parameter list
	}
	
	/**Retrieves a parameter value of the content line.
	@param paramName The name of the parameter, which will be matched against
		available parameters in a case insensitive way.
	@return The value of the first matching parameter, or <code>null</code> if
		there is no matching parameter. 
	*/
	public String getParamValue(final String paramName)
	{
		return DirectoryUtilities.getParamValue(getParamList(), paramName);	//get the value from the parameter list 
	}

	/**@return A string representation of this content line.*/
	public String toString()
	{
		final StringBuffer stringBuffer=new StringBuffer();	//create a string buffer to use in constructing the string
		if(getProfile()!=null)	//if there's a profile
		{
			stringBuffer.append('[').append(getProfile()).append(']').append(' ');	//append the profile
		}
		if(getGroup()!=null)	//if there's a group
		{
			stringBuffer.append(getGroup()).append(GROUP_NAME_SEPARATOR_CHAR);	//append the group
		}
		stringBuffer.append(getTypeName());	//append the type name
		if(getParamList().size()>0)	//if there are parameters
		{
			final Iterator paramIterator=getParamList().iterator();	//get an iterator to the parameters
			while(paramIterator.hasNext())	//while there are more parameters
			{
				final NameValuePair param=(NameValuePair)paramIterator.next();	//get the next parameter name/value pair
					//append the parameter name and value
				stringBuffer.append(PARAM_SEPARATOR_CHAR).append(param.getName().toString()).append(PARAM_NAME_VALUE_SEPARATOR_CHAR).append(param.getValue().toString());
			}
		}
		stringBuffer.append(NAME_VALUE_SEPARATOR_CHAR).append(getValue().toString());	//append the value
		return stringBuffer.toString();	//return the string we constructed
	}
}
