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

package com.globalmentor.rdf.version;

import java.net.URI;

import com.globalmentor.rdf.*;

/**Utilities for working with version metadata stored in RDF.
@author Garret Wilson
*/
public class RDFVersion
{

	/**The recommended prefix to the version ontology namespace.*/
	public final static String VERSION_NAMESPACE_PREFIX="ver";
	/**The URI to the version namespace.*/
	public final static URI VERSION_NAMESPACE_URI=URI.create("http://globalmentor.com/namespaces/2003/version#");

		//version ontology property names

	/**The version of a resource. The local name of ver:version.*/
	public final static String VERSION_PROPERTY_NAME="version";

	/**Adds a <code>ver:version</code> property with the given value to the
		resource.
	@param resource The resource to which the property should be added.
	@param value The property value to add.
	@return The added literal property value.
	*/
	public static RDFLiteral addVersion(final RDFResource resource, final String value)
	{
		return resource.addProperty(VERSION_NAMESPACE_URI, VERSION_PROPERTY_NAME, value);
	}

	/**Returns the value of the first <code>ver:version</code> property.
	@param resource The resource the property of which should be located.
	@return The value of the first <code>ver:version</code> property, or
		<code>null</code> if no such property exists.
	*/
	public static RDFObject getVersion(final RDFResource resource)
	{
		return resource.getPropertyValue(VERSION_NAMESPACE_URI, VERSION_PROPERTY_NAME);
	}
}