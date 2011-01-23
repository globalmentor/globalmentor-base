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

package com.globalmentor.text.xml;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**A factory to create XML elements from objects.
@author Garret Wilson
*/
public interface ObjectXMLFactory
{

	/**Creates an XML element from the provided object.
	@param object The object from which an XML element should be created.
	@param document The XML document to be used to create an XML element.
	@return A new XML element created from the provided object.
	*/
	public Element create(final Object object, final Document document);

}