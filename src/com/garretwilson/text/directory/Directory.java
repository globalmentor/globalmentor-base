package com.garretwilson.text.directory;

import java.util.*;
import com.garretwilson.util.*;

/**A directory of type <code>text/directory</code> as defined in
	"RFC 2425: A MIME Content-Type for Directory Information".
@author Garret Wilson
*/
public class Directory extends DefaultNamedObject
{

	/**Sets the name of the directory.
	@param name The new name of the directory.
	*/
	public void setName(final Object name) {super.setName(name);}	//G***maybe replace any name content lines when this is set

	/**The list of content lines in the directory.*/
	private final List contentLineList=new ArrayList();
	
		/**@return The list of content lines in the directory.*/
		public List getContentLineList() {return contentLineList;}

	/**Default constructor.*/
	public Directory()
	{
		super(null);	//construct a directory with no name
	}

	/**@return A string representation of the directory.*/
	public String toString()
	{
		final StringBuffer stringBuffer=new StringBuffer("text/directory");	//create a new string buffer in which to construct the string
		if(getName()!=null)	//if there is a name
		{
			stringBuffer.append('[').append(getName()).append(']');	//append the name
		}
		stringBuffer.append(':').append(' ').append(getContentLineList().size()).append(" content line(s)\n");
		final Iterator contentLineIterator=getContentLineList().iterator();	//get an iterator to the content lines
		while(contentLineIterator.hasNext())	//while there are more content lines
		{
			final ContentLine contentLine=(ContentLine)contentLineIterator.next();	//get the next content line
			stringBuffer.append(contentLine.toString()).append('\n');	//append this content line
		}
		return stringBuffer.toString();	//return the string we constructed
	}
}
