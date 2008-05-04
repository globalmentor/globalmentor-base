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

package com.globalmentor.urf;

import java.net.URI;
import java.util.*;

import com.globalmentor.text.directory.*;
import static com.globalmentor.text.directory.Directory.*;
import static com.globalmentor.urf.URF.*;

/**URF factory for the predefined types of a directory of type
	<code>text/directory</code> as defined in
	<a href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>,
	"A MIME Content-Type for Directory Information".
@author Garret Wilson
*/
public class URFPredefinedDirectoryProfile extends PredefinedProfile implements DirectoryContentLineURFPropertyURIFactory, DirectoryContentLineURFPropertyValueFactory
{

	/**A map of property URIs keyed to supported type name strings.*/
	private final Map<String, URI> typeNamePropertyURIMap=new HashMap<String, URI>();

		/**Registers a property URI keyed to the lowercase version of a type name.
		@param typeName The type name for which a property URIshould be associated.
		@param propertyURI The property URI to associate with this type name.
		*/
		protected void registerPropertyURI(final String typeName, final URI propertyURI)
		{
			typeNamePropertyURIMap.put(typeName.toLowerCase(), propertyURI);	//put the property URI in the map, keyed to the lowercase version of the type name		
		}

		/**Returns a property URI keyed to the lowercase version of a type name.
		@param typeName The type name for which property URI should be retrieved.
		@return The property URI associated with this type name, or
			<code>null</code> if no property URI has been registered with the type name.
		*/
		protected URI getPropertyURI(final String typeName)
		{
			return typeNamePropertyURIMap.get(typeName.toLowerCase());	//get whatever property URI we have associated with this type name, if any
		}

	/**Default constructor.*/
	public URFPredefinedDirectoryProfile()
	{
			//register property URIs for the predefined types
/*TODO fix if needed
		registerPropertyURI(SOURCE_TYPE, RDFResources.createReferenceURI(RDFDirectory.DIRECTORY_NAMESPACE_URI, Directory.SOURCE_TYPE.toLowerCase()));	//SOURCE		
		registerPropertyURI(NAME_TYPE, RDFResources.createReferenceURI(RDFDirectory.DIRECTORY_NAMESPACE_URI, Directory.NAME_TYPE.toLowerCase()));	//NAME
*/
	}

	/**Creates an URF property URI to represent the given directory content line.
	@param contentLine The directory content line to be converted to an URF property.
	@return An URF property URI representing the directory content line,
		or <code>null</code> if an URF property resource cannot be creatd. 
	*/
	public URI createPropertyURI(final ContentLine contentLine)
	{	
		return getPropertyURI(contentLine.getName());	//get whatever property URI we have associated with this type name, if any
	}	

	/**Creates an URF resource to represent the value of the given directory content line.
	@param urf The URFdata model to use when creating the URF resources.
	@param contentLine The directory content line to be converted to an URF resource.
	@param valueType The type of directory value the content line represents.
	@return An URF resource representing the value of the directory content line,
		or <code>null</code> if an URF resource cannot be creatd. 
	*/
	public URFResource createPropertyValue(final URF urf, final ContentLine contentLine, final String valueType)
	{
		if(TEXT_VALUE_TYPE.equalsIgnoreCase(valueType))	//if this is the "text" value type
		{
			return DEFAULT_URF_RESOURCE_FACTORY.createStringResource(contentLine.getValue().toString());	//return a resource with the string value of the content line as a literal
/*TODO fix
				if(contentLine.getGroup()==null && contentLine.getParamList().size()==0)	//if there is no group and no parameters
				{
					return new Literal(contentLine.getValue().toString());	//return the string value of the content line as a literal
				}
				else	//if there is a group or parameters, we'll need to create a blank node for a structured value
				{
				}
*/
		}
		return null;	//show that we can't create a value
	}
	
}
