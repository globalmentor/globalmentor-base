package com.garretwilson.text.directory;

import java.io.*;
import java.util.*;
import java.net.MalformedURLException;
import java.net.URL;
//G***del if we don't need import java.text.MessageFormat;
import org.w3c.dom.DOMException;
import com.garretwilson.io.*;
import com.garretwilson.net.URLUtilities;
import com.garretwilson.text.CharacterEncoding;
import com.garretwilson.text.xml.schema.*;
//G***del import com.garretwilson.util.StringManipulator;
import com.garretwilson.util.*;

/**Class that can process a directory of type <code>text/directory</code> as
	defined in 
	<a href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>,
	"A MIME Content-Type for Directory Information".
@author Garret Wilson
*/
public class DirectoryProcessor implements DirectoryConstants
{

	/**The delimiter characters separating the main components of a content line
		with no group provided (';', ':', CR, and LF).
	*/
	protected final static String GROUPLESS_CONTENT_LINE_DELIMITER_CHARS=""+PARAM_SEPARATOR_CHAR+NAME_VALUE_SEPARATOR_CHAR+CR+LF;	

	/**The delimiter characters separating the main components of a content line
		('.', ';', ':', CR, and LF).
	*/
	protected final static String CONTENT_LINE_DELIMITER_CHARS=GROUP_NAME_SEPARATOR_CHAR+GROUPLESS_CONTENT_LINE_DELIMITER_CHARS;	

	/**Retrieves a line of content from a directory.
	@param reader The reader that contains the lines of the directory.
	@return A line of content from the directory
	@exception IOException Thrown if there is an error reading the directory.
	@exception ParseIOException Thrown if there is a an error interpreting the directory.
	*/
	public DirectoryLine processDirectoryLine(final LineUnfoldParseReader reader) throws IOException, ParseIOException
	{
		String group=null;	//we'll store the group here
		String name=null;	//we'll store the name here
		List paramList=null;	//we'll store parameters here, if we have any
		String token=reader.readStringUntilCharEOF(CONTENT_LINE_DELIMITER_CHARS);	//read the next line token
		char c=reader.readChar();	//get the delimiter character we encountered
		if(c==GROUP_NAME_SEPARATOR_CHAR)	//if we just read a group
		{
			//G***check the syntax of the group
			group=token;	//save the group we read
			token=reader.readStringUntilCharEOF(GROUPLESS_CONTENT_LINE_DELIMITER_CHARS);	//read the next line token after the group, which should be the name
			c=reader.readChar();	//get the delimiter character we encountered, and fall through to checking the name
		}
		switch(c)	//see which delimiter character we encountered
		{
			case PARAM_SEPARATOR_CHAR:	//if we just read a parameter separator
			case NAME_VALUE_SEPARATOR_CHAR:	//if we just read the character separates the name from the value
				//G***check the name
				name=token;	//this is the name
				if(c==PARAM_SEPARATOR_CHAR)	//if this was the character separating the name from parameters, read the parameters
				{
					paramList=processParameters(reader);	//process the parameters, consuming the ending delimiter
				}
				//G***process the value
				//G***decide whether to require CRLF
				return null; //G***create and return the directory line


//G***this isn't right---we shouldn't even check for a CR or LF at this point
			case CR:	//if we just read a carriage return
				reader.readExpectedChar(LF);	//there should always be an LF after a CR
				return null;	//G***decide what to do with an empty line
			case LF:	//if we see an LF before a CR
			default:	//if we read anything else (there shouldn't be anything else unless there is a logic error)					
				throw new ParseUnexpectedDataException(c, reader.getLineIndex(), reader.getCharIndex(), reader.getName());	//show that we didn't expect an LF here
		}
	}

	/**When reading the parameter name, we expect either a parameter name/value
		separator ('=') or the line name/value separator (':'), indicating we've
		finished parameters.
	*/ 
	protected final static String PARAM_NAME_DELIMITER_CHARS=""+PARAM_NAME_VALUE_SEPARATOR_CHAR+NAME_VALUE_SEPARATOR_CHAR;

	/**When reading the parameter name, we expect either a parameter separator
		(';') the parameter value separator (',') indicating more values, or the
		line name/value separator (':'), indicating we've finished parameters.
	*/ 
	protected final static String PARAM_VALUE_DELIMITER_CHARS=""+PARAM_SEPARATOR_CHAR+PARAM_VALUE_SEPARATOR_CHAR+NAME_VALUE_SEPARATOR_CHAR;

	/**Retrieves parameters from a line of content from a directory.
		Whatever delimiter ended the parameters will no longer exist in the reader.
	@param reader The reader that contains the lines of the directory.
	@return A list of name/object pairs, the value of each is a <code>List</code>
		of values.
	@exception IOException Thrown if there is an error reading the directory.
	@exception ParseIOException Thrown if there is a an error interpreting the directory.
	@see NameValuePair
	*/
	public List processParameters(final LineUnfoldParseReader reader) throws IOException, ParseIOException
	{
		final List paramList=new ArrayList();	//create a list of parameters
		while(true)	//keep reading parameters until we reach the end of the parameters
		{	
					//read the parameter name
			final String paramName=reader.readStringUntilChar(PARAM_NAME_DELIMITER_CHARS);	//get the parameter name
			if(reader.readChar()==NAME_VALUE_SEPARATOR_CHAR)	//if we're finished with the parameters
				break;	//stop reading parameters
			//G***check the param name for validity
			NameValuePair parameter=null;	//we'll create this when we have all the values for the parameter
			final List paramValueList=new ArrayList();	//create a list to hold the parameter values
					//read the parameter value
			while(parameter==null)	//keep reading values until we reach the end of the parameter values and create a parameter
			{
				final String paramValue;	//we'll read the value and store it here
				switch(reader.peekChar())	//see what character is first in the value
				{
					case DQUOTE:	//if the string starts with a quote
						paramValue=reader.readDelimitedString(DQUOTE, DQUOTE);	//read the value within the quotes 
						break;
					default:	//if the string doesn't end with a quote
						paramValue=reader.readStringUntilChar(PARAM_VALUE_DELIMITER_CHARS);	//read everything until the end of this parameter
						break;
				}
				//G***check the parameter value, here
				paramValueList.add(paramValue);	//add this value to our list of values
				switch(reader.peekChar())	//see what comes after this parameter value
				{
					case PARAM_SEPARATOR_CHAR:	//if this is the last of the values for this parameter, and there's another parameter coming up
					case NAME_VALUE_SEPARATOR_CHAR:	//if this is the last of the values for this parameter, and there are no more parameters
						parameter=new NameValuePair(paramName, paramValueList);	//create a parameter with the name and list of values
						break;
				}				
				reader.resetPeek();	//unpeek the character we peeked
			}
			paramList.add(parameter);	//add this parameter to our list
			if(reader.readChar()==NAME_VALUE_SEPARATOR_CHAR)	//if we're finished with the parameters
				break;	//stop reading parameters
		}
		return paramList;	//return the list of parameters we filled
	}

}
