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
}
