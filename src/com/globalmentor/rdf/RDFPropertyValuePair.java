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

package com.globalmentor.rdf;

import com.globalmentor.util.NameValuePair;

/**An RDF property and its value.
@author Garret Wilson
 */
public class RDFPropertyValuePair extends NameValuePair<RDFResource, RDFObject>
{
	
	/**@return A property resource; the predicate of an RDF statement.*/
	public RDFResource getProperty() {return getName();}
	
	/**@return A property value; the object of an RDF statement.*/
	public RDFObject getPropertyValue() {return getValue();}

	/**Constructor specifying the RDF property and value.
	@param property A property resource; the predicate of an RDF statement.
	@param value A property value; the object of an RDF statement.
	*/
	public RDFPropertyValuePair(final RDFResource property, final RDFObject value)
	{
		super(property, value);	//construct the parent class
	}

}
