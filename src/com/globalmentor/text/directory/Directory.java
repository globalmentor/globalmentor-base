package com.globalmentor.text.directory;

import java.util.*;

import com.globalmentor.util.*;

/**A directory of type <code>text/directory</code> as defined in
	<a href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>, "A MIME Content-Type for Directory Information".
@author Garret Wilson
*/
public class Directory
{

	/**The display name of the directory.*/
	private LocaledText displayName=null;

		/**@return The display name of the directory.*/
		public LocaledText getDisplayName() {return displayName;}

		/**Sets the display name of the directory.
		@param displayName The new display name of the directory.
		*/
		public void setDisplayName(final LocaledText displayName) {this.displayName=displayName;}

	/**The list of content lines that represent unrecognized and/or unprocessed information.*/
	private final List<ContentLine> contentLineList=new ArrayList<ContentLine>();

		/**@return The list of content lines that represent unrecognized and/or unprocessed information.*/
		public List<ContentLine> getContentLineList() {return contentLineList;}

	/**Default constructor.*/
	public Directory()
	{
	}

}
