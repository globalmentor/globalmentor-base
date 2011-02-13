/*
 * Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.urf.content;

import java.net.URI;

/**A content resource that holds text content in its description through the {@value Content#CONTENT_PROPERTY_URI} property.
@author Garret Wilson
*/
public class Text extends AbstractContentResource
{

	/**Default constructor with no URI.*/
	public Text()
	{
		this(null);	//create a resource without a URI
	}

	/**URI constructor.
	@param uri The URI for the resource, or <code>null</code> if the resource should have no URI.
	*/
	public Text(final URI uri)
	{
		super(uri, Content.CONTENT_NAMESPACE_URI);	//construct the class, specifying the Content namespace
	}

	/**Returns the actual content of the resource.
	This method delegates to {@link #getStringContent()}. 
	@return This resource's string content declaration, or <code>null</code> if the resource has no <code>content.content</code> property specified or the content is not a string.
	*/
	public String getContent()
	{
		return getStringContent();	//get any string content
	}

	/**Determines a string value to use for representation.
	This method may take into account the current properties of the resource in order to provide the best possible string representation.
	This implementation determines the label in the following sequence:
	<ol>
		<li>The determined label of any «{@value Content#CONTENT_PROPERTY_URI}» property.</li>
		<li>Whatever label is determined by the parent class.</li>
	</ol>
	@return A string label to use for representation of the resource.
	@see #getStringContent()
	*/
	public String determineLabel()
	{
		final String content=getStringContent();	//get the string content, if there is any
		return content!=null ? content : super.determineLabel();	//determine the string content, if there is any; otherwise determine the default label
	}
}
