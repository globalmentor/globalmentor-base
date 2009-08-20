/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.text.directory;

import java.util.*;

import com.globalmentor.model.LocaledText;
import com.globalmentor.util.*;

/**A directory of type <code>text/directory</code> as defined in
	<a href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>, "A MIME Content-Type for Directory Information".
@author Garret Wilson
*/
public class Directory
{

	/**The recommended character length greater than which a line should be folded.*/
	public final static int LONG_LINE_LENGTH=75;

	/**The character separating the group from the name in a content line.*/
	public final static char GROUP_NAME_SEPARATOR_CHAR='.';

	/**The character separating parameters in a content line.*/
	public final static char PARAM_SEPARATOR_CHAR=';';
	
	/**The character separating a name from a value in a content line.*/
	public final static char NAME_VALUE_SEPARATOR_CHAR=':';

	/**The character separating a parameter name from a pareter value in a content line.*/
	public final static char PARAM_NAME_VALUE_SEPARATOR_CHAR='=';

	/**The character separating multiple parameter values in a content line.*/
	public final static char PARAM_VALUE_SEPARATOR_CHAR=',';

	/**The character used to separate multiple values.*/
	public final static char VALUE_SEPARATOR_CHAR=','; 

		//predefined types
		
	/**The type to identify the source of directory information
   contained in the content type.
  @see #URI_VALUE_TYPE
  */
	public final static String SOURCE_TYPE="SOURCE";

	/**The type to identify the displayable name of the directory
		entity to which information in the content type pertains.
	@see #TEXT_VALUE_TYPE
	*/
	public final static String NAME_TYPE="NAME";

	/**The type to identify the type of directory entity to which
		information in the content type pertains.
	@see #TEXT_VALUE_TYPE
	*/
	public final static String PROFILE_TYPE="PROFILE";

	/**The type to denote the beginning of a syntactic entity within a
		text/directory content-type.
	@see #TEXT_VALUE_TYPE
	*/
	public final static String BEGIN_TYPE="BEGIN";

	/**The type to denote the end of a syntactic entity within a
		text/directory content-type.
	@see #TEXT_VALUE_TYPE
	*/
	public final static String END_TYPE="END";

		//predefined parameters and predefined value types
		
	/**The encoding predefined type.*/
	public final static String ENCODING_PARAM_NAME="encoding";

		/**The binary encoding type from RFC 2047.*/
		public final static String B_ENCODING_TYPE="b";	//G***check to see how this relates to RFC 2047, and if this constant should be defined elsewhere	

	/**The value type predefined type.*/
	public final static String VALUE_PARAM_NAME="value";

		/**The generic URI value type from section 5 of RFC 1738.*/
		public final static String URI_VALUE_TYPE="uri";	//G***check to see how this relates to RFC 1738, and why the test/directory specification says "genericurl"

		/**The text value type.*/
		public final static String TEXT_VALUE_TYPE="text";
		
			/**The character used to escape characters in text values ('\\').*/
			public final static char TEXT_ESCAPE_CHAR=92;

			/**The string of a single character used to escape characters in text values ("\\").*/
			public final static String TEXT_ESCAPE_STRING=String.valueOf(TEXT_ESCAPE_CHAR);

			/**The lowercase version of an escaped line break in text ('n').*/
			public final static char TEXT_LINE_BREAK_ESCAPED_LOWERCASE_CHAR=110;

			/**The uppercase version of an escaped line break in text ('N').*/
			public final static char TEXT_LINE_BREAK_ESCAPED_UPPERCASE_CHAR=78;

		/**The date value type.*/
		public final static String DATE_VALUE_TYPE="date";

		/**The time value type.*/
		public final static String TIME_VALUE_TYPE="time";
	
		/**The date/time value type.*/
		public final static String DATE_TIME_VALUE_TYPE="date-time";
	
		/**The integer value type.*/
		public final static String INTEGER_VALUE_TYPE="integer";
	
		/**The boolean value type.*/
		public final static String BOOLEAN_VALUE_TYPE="boolean";
	
		/**The float value type.*/
		public final static String FLOAT_VALUE_TYPE="float";

	/**The language parameter type, as defined by RFC 1766.*/
	public final static String LANGUAGE_PARAM_NAME="language";

	/**The context parameter type.*/
	public final static String CONTEXT_PARAM_NAME="context";

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
