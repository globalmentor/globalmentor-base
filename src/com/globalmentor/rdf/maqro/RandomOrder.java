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

import static com.globalmentor.rdf.maqro.MAQRO.*;

/**Random ordering of MAQRO interactions.
@author Garret Wilson
*/
public class RandomOrder extends Order
{

	/**@return The local name of the default type of this resource.*/
	public String getDefaultTypeName() {return RANDOM_ORDER_CLASS_NAME;}

	/**Default constructor.*/
	public RandomOrder()
	{
		super();	//construct the parent class
	}

	/**Reference URI constructor.
	@param referenceURI The reference URI for the new resource.
	*/
	public RandomOrder(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

}
