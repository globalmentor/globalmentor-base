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
