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

import java.net.URI;

/**A factory to create typed literals by doing the required
	lexical form to value mapping.
@author Garret Wilson
@see RDF
*/
public interface RDFTypedLiteralFactory
{

	/**Creates a typed literal from the provided lexical form based upon the
		datatype reference URI
	@param lexicalForm The lexical form of the resource.
	@param datatypeURI The datatype reference URI of the datatype.
	@return A typed literal containing a value object mapped from the given lexical form and datatype
	@exception IllegalArgumentException if the given lexical form is not in the correct format to be interpreted as the given type.
	*/
	public RDFTypedLiteral<?> createTypedLiteral(final String lexicalForm, final URI datatypeURI);

}