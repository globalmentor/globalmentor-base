package com.garretwilson.text.directory;

import java.util.*;
import com.garretwilson.util.*;

/**A line in a directory of type <code>text/directory</code> as defined in
	<a href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>,
	"A MIME Content-Type for Directory Information".
@author Garret Wilson
*/
public class ContentLine extends NameValuePair
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
	
	/**Creates a directory line with a group, name, parameter, list, and value.
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
	
}
