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

package com.globalmentor.rdf.maqro;

import java.net.URI;

import com.globalmentor.rdf.*;

import static com.globalmentor.rdf.RDFResources.*;
import static com.globalmentor.rdf.maqro.MAQRO.*;

/**An evaluation of a MAQRO activity.
@author Garret Wilson
*/
public abstract class Evaluation extends TypedRDFResource
{

	/**@return The namespace URI of the ontology defining the default type of this resource.*/
	public URI getDefaultTypeNamespaceURI() {return MAQRO_NAMESPACE_URI;}

	/**Default constructor.*/
	public Evaluation()
	{
		super();	//construct the parent class
	}
	
	/**Reference URI constructor.
	@param referenceURI The reference URI for the new resource.
	*/
	public Evaluation(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**@return The <code>maqro:condition</code> literal value, or <code>null</code> if there is condition no value or it is not a plain literal.*/
	public String getCondition()
	{
		return asString(getPropertyValue(MAQRO_NAMESPACE_URI, CONDITION_PROPERTY_NAME));	//return the maqro:condition property value if it represents a string
	}
	
	/**Sets the <code>maqro:condition</code> property of the evaluation of the dialogue with the new condition string.
	@param condition The new conditoin string.
	*/
	public void setCondition(final String condition)
	{
		setProperty(MAQRO_NAMESPACE_URI, CONDITION_PROPERTY_NAME, condition); //set the maqro:condition property value
	}

}
