package com.garretwilson.rdf.directory;

import java.io.*;
import java.net.URI;
import java.net.MalformedURLException;	//G***del when works
import java.net.URISyntaxException;
import java.util.*;
import com.garretwilson.io.*;
import com.garretwilson.net.URLUtilities;
import com.garretwilson.rdf.*;
import com.garretwilson.text.directory.*;
import com.garretwilson.util.*;

/**Class that is able to construct an RDF data model from a directory of type
	<code>text/directory</code> as defined in
	<a href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>,
	"A MIME Content-Type for Directory Information".
@author Garret Wilson
*/
public class DirectoryRDFProcessor extends AbstractRDFProcessor implements DirectoryConstants, com.garretwilson.text.directory.DirectoryConstants
{
	/**The profile for the predefined types.*/
	private final PredefinedRDFProfile predefinedProfile=new PredefinedRDFProfile();		

		/**@return The profile for the predefined types.*/
		protected PredefinedRDFProfile getPredefinedProfile() {return predefinedProfile;}
	
	/**A map of profiles keyed to the lowercase version of the profile name.*/
	private final Map profileMap=new HashMap();	

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
		@see #getPredefinedProfile
		*/ 
		protected Profile getProfile(final String profileName)
		{
			return profileName!=null ? (Profile)profileMap.get(profileName.toLowerCase()) : getPredefinedProfile();	//get the profile keyed to the lowercase version of the profile name, or return the predefined profile if null was passed
		}

	/**A map of property value factories keyed to the lowercase version of the value type.*/
	private final Map valueTypePropertyValueFactoryMap=new HashMap();	

		/**Registers a property value factory by value type.
		@param valueType The value type for which this property value factory can create RDF objects.
		@param propertyValueFactory The property value factory to be registered with this value type.
		*/	
		public void registerPropertyValueFactoryByValueType(final String valueType, final RDFPropertyValueFactory propertyValueFactory)
		{
			valueTypePropertyValueFactoryMap.put(valueType.toLowerCase(), propertyValueFactory);	//put the property value factory in the map, keyed to the lowercase version of the type
		}
		
		/**Retrieves a property value factory to create RDF objects for the given value type.
		@param valueType The value type for which a property value factory should be returned.
		@return A property value factory for this value type, or <code>null</code> if there
			is no property value factory registered for this value type.
		*/ 
		protected RDFPropertyValueFactory getPropertyValueFactoryByValueType(final String valueType)
		{
			return (RDFPropertyValueFactory)valueTypePropertyValueFactoryMap.get(valueType.toLowerCase());	//get the property value factory keyed to the lowercase version of this value type
		}
		
	/**Default constructor.*/
	public DirectoryRDFProcessor()
	{
		this(new RDF());	//construct the class with a default RDF data model
	}

	/**Constructor that specifies an existing data model to continue filling.
	@param rdf The RDF data model to use.
	*/
	public DirectoryRDFProcessor(final RDF rdf)
	{
		super(rdf);  //construct the parent class
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
		given RDF resource.
	<p>For each content line, an RDF property resource and an RDF value object
		are created and added to the resource as a property. The property and
		value are created using factories obtained in the following manner:</p>
	<ol>
		<li>If a profile is registered that implements
			<code>RDFPropertyFactory</code>, it is used to create the property.
			Otherwise, the predefined profile is asked to create the property.</li>
		<li>If the content line doesn't specifiy a value type, but the profile
			implements <code>ValueFactory</code>, that profile is asked for the
			value type.</li>
		<li>If the profile implements <code>RDFPropertyValueFactory</code>, the
			profile is asked to create the value.</li>
		<li>If there is a value type and there is a property value factory
			specifically registered for that value type, it is asked to create the
			value.</li>
		<li>If no RDF property value object can be created, the content line is
			ignored.</li>
	</ol> 
	@param resource The resource the directory represents.
	@param directory The directory containing information to convert to RDF.
	@return The RDF data model resulting from this processing and any previous
		processing.
	*/
	public RDF process(final RDFResource resource, final Directory directory)
	{
		final RDF rdf=getRDF(); //get the RDF data model we're using
		final Iterator contentLineIterator=directory.getContentLineList().iterator();	//get an iterator to all the content lines of the directory
		while(contentLineIterator.hasNext())	//while there are more content lines
		{
			final ContentLine contentLine=(ContentLine)contentLineIterator.next();	//get the next content line
			final String profileName=contentLine.getProfile();	//get the name of this content line's profile 
			final Profile profile=getProfile(profileName);	//see if we have a profile registered with this profile name
			RDFResource property=null;	//we'll try to get the property resource to use
			if(profile instanceof RDFPropertyFactory)	//if this profile knows how to create properties
			{
				property=((RDFPropertyFactory)profile).createProperty(rdf, contentLine);	//ask the profile to create a property for this content line
			}
			if(property==null && profile!=getPredefinedProfile())	//if we still don't know the property, and we didn't already check the predefined profile 
			{
				property=getPredefinedProfile().createProperty(rdf, contentLine);	//ask the predefined profile for the property
			}
			if(property!=null)	//if we have a property
			{
				RDFObject value=null;	//we'll try to create an RDF object from the value
					//try to create a property value from a property value factory registered with the value type
				String valueType=contentLine.getParamValue(VALUE_PARAM_NAME);	//get the value type parameter
				if(valueType==null)	//if the value type wasn't explicitly given
				{
					if(profile!=null)	//if there is a profile for this profile name
					{
						valueType=profile.getValueType(contentLine.getProfile(), contentLine.getGroup(), contentLine.getTypeName(), contentLine.getParamList());	//ask this profile's value factory for the value type
					}
					if(valueType==null && profile!=getPredefinedProfile())	//if we still don't know the type, and we didn't already check the predefined profile 
					{
						valueType=getPredefinedProfile().getValueType(contentLine.getProfile(), contentLine.getGroup(), contentLine.getTypeName(), contentLine.getParamList());	//ask the predefined profile for the value type
					}
				}
					//try to create a property value from the profile
				if(profile instanceof RDFPropertyValueFactory)	//if there is yet no value, and profile is an RDF property value factory
				{
					((RDFPropertyValueFactory)profile).createPropertyValue(rdf, contentLine, valueType);	//ask the profile to create an an RDF object for the value
				}
					//try to create a property value from any value factory specifically registered for this value type
				if(value==null && valueType!=null)	//if we don't have a value but we determined a value type
				{
					final RDFPropertyValueFactory valueTypePropertyValueFactory=getPropertyValueFactoryByValueType(valueType);	//try to get a property value factory registered with the value type
					if(valueTypePropertyValueFactory!=null)	//if we have a property value factory registered with the value type
					{
						value=valueTypePropertyValueFactory.createPropertyValue(rdf, contentLine, valueType);	//ask the factory to create an RDF object for the value 
					}
				}
				if(value!=null)	//if we  have a value (we also have a property, or we wouldn't have made it here)
				{
					resource.addProperty(property, value);	//add the property to the resource
				}
			} 
		}
		return rdf;  //return the RDF data model
	}
	
}