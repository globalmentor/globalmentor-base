package com.garretwilson.text.directory;

import java.io.*;
import java.util.*;
import com.garretwilson.io.*;

/**Class that knows how to give profile-specific information for types within
	a profile of a <code>text/directory</code> as defined in 
	<a href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>,
	"A MIME Content-Type for Directory Information".
<p>A profile may return <code>null</code> from <code>getValueType()</code> if
	if it wishes to do custom creation in <code>createValues()</code>, providing
	it has been registered with the particular type name.
@author Garret Wilson
*/
public interface Profile
{
	
	/**Determines the value type of the given content line value.
	@param profile The profile of this content line, or <code>null</code> if
		there is no profile.
	@param group The group specification, or <code>null</code> if there is no group.
	@param name The name of the information.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>String</code>.
	@return The value type of the content line, or <code>null</code> if the
		value type cannot be determined.
	*/	
	public String getValueType(final String profile, final String group, final String name, final List paramList);
	
}
