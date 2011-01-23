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

package com.globalmentor.rdf.xmlschema;

import com.globalmentor.rdf.*;

import static com.globalmentor.text.xml.schema.XMLSchema.*;

/**An RDF typed literal that represents an XML Schema string.
@author Garret Wilson
@see String
*/
public class StringLiteral extends RDFTypedLiteral<String>
{

	/**Constructs a string literal using the datatype <code>http://www.w3.org/2001/XMLSchema#string</code>.
	@param string The <code>String</code> object representing the value of the literal.
	*/
	public StringLiteral(final String string)
	{
		super(string, STRING_DATATYPE_URI);	//save the string object as the value, specifying the XML Schema boolean datatype URI
	}

}