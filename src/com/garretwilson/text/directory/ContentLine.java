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

	/**The group specification, or <code>null</code> if there is no group.*/
	private String group=null;	
	
		/**@return The group specification, or <code>null</code> if there is no group.*/
		public String getGroup() {return group;}

		/**Sets the group.
		@param group The group specification, or <code>null</code> if there is no group.
		*/
		public void setGroup(final String group) {this.group=group;}
		
	/**The list of parameters.*/
	private final List paramList;

		/**@return The list of parameters, each item of which is a
			<code>NameValuePair</code> with a name of type <code>String</code> and a
			value of type <code>Object</code>.
		@see NameValuePair
		*/
		public List getParamList() {return paramList;}
	
	/**Creates a directory line with a group, name, parameter, list, and value.
	@param group The group specification, or <code>null</code> if there is no group.
	@param name The name of the information.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>Object</code>.
	@param value The value of the information.
	*/
	public ContentLine(final String group, final String name, final List paramList, final Object value)
	{
		super(name, value);	//construct the parent class
		this.group=group;	//save the group
		this.paramList=paramList;	//save the parameter list
	}
}
