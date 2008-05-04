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

/**Class that is able to construct an URF data model from a directory of type
	<code>text/directory</code> as defined in
	<a href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>,
	"A MIME Content-Type for Directory Information".
@author Garret Wilson
*/
public class URFDirectoryProcessor extends AbstractURFProcessor
{
	/**The profile for the predefined types.*/
	private final URFPredefinedDirectoryProfile predefinedProfile=new URFPredefinedDirectoryProfile();		

		/**@return The profile for the predefined types.*/
		protected URFPredefinedDirectoryProfile getPredefinedProfile() {return predefinedProfile;}
	
	/**A map of profiles keyed to the lowercase version of the profile name.*/
	private final Map<String, Profile> profileMap=new HashMap<String, Profile>();	

		/**Registers a profile.
		@param profileName The name of the profile.
		@param profile The profile to be registered with this profilename.
		*/	
		public void registerProfile(final String profileName, final Profile profile)
		{
			profileMap.put(profileName.toLowerCase(), profile);	//put the profile in the map, keyed to the lowercase version of the profile name
		}

		/**Retrieves a profile for the given profile name.
		@param profileName The name of the profile to return, or <code>null</code>
			if the predefined profile should be returned.
		@return A profile for this profile name, or <code>null</code> if there
			is no profile registered for this profile name.
		@see #getPredefinedProfile()
		*/ 
		protected Profile getProfile(final String profileName)
		{
			return profileName!=null ? profileMap.get(profileName.toLowerCase()) : getPredefinedProfile();	//get the profile keyed to the lowercase version of the profile name, or return the predefined profile if null was passed
		}

	/**A map of property value factories keyed to the lowercase version of the value type.*/
	private final Map<String, DirectoryContentLineURFPropertyValueFactory> valueTypePropertyValueFactoryMap=new HashMap<String, DirectoryContentLineURFPropertyValueFactory>();	

		/**Registers a property value factory by value type.
		@param valueType The value type for which this property value factory can create URF resources.
		@param propertyValueFactory The property value factory to be registered with this value type.
		*/
		public void registerPropertyValueFactoryByValueType(final String valueType, final DirectoryContentLineURFPropertyValueFactory propertyValueFactory)
		{
			valueTypePropertyValueFactoryMap.put(valueType.toLowerCase(), propertyValueFactory);	//put the property value factory in the map, keyed to the lowercase version of the type
		}
		
		/**Retrieves a property value factory to create URF resources for the given value type.
		@param valueType The value type for which a property value factory should be returned.
		@return A property value factory for this value type, or <code>null</code> if there
			is no property value factory registered for this value type.
		*/
		protected DirectoryContentLineURFPropertyValueFactory getPropertyValueFactoryByValueType(final String valueType)
		{
			return valueTypePropertyValueFactoryMap.get(valueType.toLowerCase());	//get the property value factory keyed to the lowercase version of this value type
		}
		
	/**Default constructor.*/
	public URFDirectoryProcessor()
	{
		this(new URF());  //create an URF data model to use
	}

	/**Constructor that specifies an existing data model to continue filling.
	@param urf The URF data model to use.
	*/
	public URFDirectoryProcessor(final URF urf)
	{
		super(urf);  //construct the parent class
			//register the predefined profile as a property value factory for the standard value types
		registerPropertyValueFactoryByValueType(URI_VALUE_TYPE, getPredefinedProfile());
		registerPropertyValueFactoryByValueType(TEXT_VALUE_TYPE, getPredefinedProfile());
		registerPropertyValueFactoryByValueType(DATE_VALUE_TYPE, getPredefinedProfile());
		registerPropertyValueFactoryByValueType(TIME_VALUE_TYPE, getPredefinedProfile());
		registerPropertyValueFactoryByValueType(DATE_TIME_VALUE_TYPE, getPredefinedProfile());
		registerPropertyValueFactoryByValueType(INTEGER_VALUE_TYPE, getPredefinedProfile());
		registerPropertyValueFactoryByValueType(BOOLEAN_VALUE_TYPE, getPredefinedProfile());
		registerPropertyValueFactoryByValueType(FLOAT_VALUE_TYPE, getPredefinedProfile());
	}

	/**Processes a directory and converts content lines into property of the
		given URF resource.
	<p>For each content line, an URF property resource and an URF value object
		are created and added to the resource as a property. The property and
		value are created using factories obtained in the following manner:</p>
	<ol>
		<li>If a profile is registered that implements
			{@link DirectoryContentLineURFPropertyURIFactory}, it is used to create the property.
			Otherwise, the predefined profile is asked to create the property.</li>
		<li>If the content line doesn't specifiy a value type, but the profile
			implements {@link DirectoryContentLineURFPropertyValueFactory}, that profile is
			asked for the value type.</li>
		<li>If the profile implements {@link DirectoryContentLineURFPropertyValueFactory},
			the profile is asked to create the value.</li>
		<li>If there is a value type and there is a property value factory
			specifically registered for that value type, it is asked to create the
			value.</li>
		<li>If no URF property value resource can be created, the content line is
			ignored.</li>
	</ol> 
	@param resource The resource the directory represents.
	@param directory The directory containing information to convert to URF.
	@return The URF data model resulting from this processing and any previous
		processing.
	*/
	public URF process(final URFResource resource, final Directory directory)
	{
		reset();	//make sure we don't have temporary data left over from last time
		final URF urf=getURF(); //get the URF data model we're using
		for(final ContentLine contentLine:directory.getContentLineList())	//get an iterator to all the content lines of the directory
		{
			final String profileName=contentLine.getProfile();	//get the name of this content line's profile 
			final Profile profile=getProfile(profileName);	//see if we have a profile registered with this profile name
			URI propertyURI=null;	//we'll try to get the property URI to use
			if(profile instanceof DirectoryContentLineURFPropertyURIFactory)	//if this profile knows how to create properties
			{
				propertyURI=((DirectoryContentLineURFPropertyURIFactory)profile).createPropertyURI(contentLine);	//ask the profile to create a property URI for this content line
			}
			if(propertyURI==null && profile!=getPredefinedProfile())	//if we still don't know the property, and we didn't already check the predefined profile 
			{
				propertyURI=getPredefinedProfile().createPropertyURI(contentLine);	//ask the predefined profile for the property URI
			}
			if(propertyURI!=null)	//if we have a property URI
			{
				URFResource valueResource=null;	//we'll try to create an URF resource object from the value
					//try to create a property value from a property value factory registered with the value type
				String valueType=contentLine.getParamValue(VALUE_PARAM_NAME);	//get the value type parameter
				if(valueType==null)	//if the value type wasn't explicitly given
				{
					if(profile!=null)	//if there is a profile for this profile name
					{
						valueType=profile.getValueType(contentLine.getProfile(), contentLine.getGroup(), contentLine.getName(), contentLine.getParamList());	//ask this profile's value factory for the value type
					}
					if(valueType==null && profile!=getPredefinedProfile())	//if we still don't know the type, and we didn't already check the predefined profile 
					{
						valueType=getPredefinedProfile().getValueType(contentLine.getProfile(), contentLine.getGroup(), contentLine.getName(), contentLine.getParamList());	//ask the predefined profile for the value type
					}
				}
					//try to create a property value from the profile
				if(profile instanceof DirectoryContentLineURFPropertyValueFactory)	//if there is yet no value, and profile is an URF property value factory
				{
					((DirectoryContentLineURFPropertyValueFactory)profile).createPropertyValue(urf, contentLine, valueType);	//ask the profile to create an URF resource for the value
				}
					//try to create a property value from any value factory specifically registered for this value type
				if(valueResource==null && valueType!=null)	//if we don't have a value but we determined a value type
				{
					final DirectoryContentLineURFPropertyValueFactory valueTypePropertyValueFactory=getPropertyValueFactoryByValueType(valueType);	//try to get a property value factory registered with the value type
					if(valueTypePropertyValueFactory!=null)	//if we have a property value factory registered with the value type
					{
						valueResource=valueTypePropertyValueFactory.createPropertyValue(urf, contentLine, valueType);	//ask the factory to create an URF resource for the value 
					}
				}
				if(valueResource!=null)	//if we  have a value (we also have a property, or we wouldn't have made it here)
				{
					resource.addPropertyValue(propertyURI, valueResource);	//add the property to the resource
				}
			} 
		}
		return urf;  //return the URF data model
	}
	
}