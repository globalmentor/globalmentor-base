package com.garretwilson.text.directory;

import java.util.*;
import com.garretwilson.util.*;

/**A line in a directory of type <code>text/directory</code> as defined in
	<a href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>,
	"A MIME Content-Type for Directory Information".
@author Garret Wilson
*/
public class DirectoryLine extends NameValuePair
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
	private List paramList=new ArrayList();
	
	
		
		
//G***fix	contentline  = [group "."] name *(";" param) ":" value CRLF

	public DirectoryLine(final String name, final Object value)
	{
		super(name, value);	//construct the parent class
	}
}
