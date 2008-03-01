package com.garretwilson.text.directory;

import java.io.*;
import java.util.*;
import com.garretwilson.io.*;
import com.globalmentor.util.NameValuePair;

/**Class that knows how to create objects to represent values provided in
	the lines of a <code>text/directory</code> as defined in 
	<a href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>,
	"A MIME Content-Type for Directory Information".
@author Garret Wilson
*/
public interface ValueFactory
{
	
	/**Processes the textual representation of a line's value and returns
		one or more object representing the value(s).
	<p>Whatever delimiter ended the value will be left in the reader.</p>
	@param profile The profile of this content line, or <code>null</code> if
		there is no profile.
	@param group The group specification, or <code>null</code> if there is no group.
	@param name The name of the information.
	@param paramList The list of parameters, each item of which is a
		<code>NameValuePair</code> with a name of type <code>String</code> and a
		value of type <code>String</code>.
	@param valueType The type of value, or <code>null</code> if the type of value
		is unknown.
	@param reader The reader that contains the lines of the directory.
	@return An array of objects represent the value string, or <code>null</code>
		if the type of value cannot be determined by the given line information,
		in which case no information is removed from the reader.
	@exception IOException Thrown if there is an error reading the directory.
	@exception ParseIOException Thrown if there is a an error interpreting the directory.
	@see NameValuePair
	*/	
	public Object[] createValues(final String profile, final String group, final String name, final List<NameValuePair<String, String>> paramList, final String valueType, final LineUnfoldParseReader reader) throws IOException, ParseIOException;
	
}
