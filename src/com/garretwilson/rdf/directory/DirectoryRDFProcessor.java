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
public class DirectoryRDFProcessor extends AbstractRDFProcessor implements DirectoryConstants, com.garretwilson.text.directory.DirectoryConstants, RDFPropertyFactory, RDFPropertyValueFactory
{

	/**A map of property URIs keyed to supported type name strings.*/
	private final Map typeNamePropertyURIMap=new HashMap();

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
			return (URI)typeNamePropertyURIMap.get(typeName.toLowerCase());	//get whatever property URI we have associated with this type name, if any
		}

	/**A map of property factories keyed to the lowercase version of the type name.*/
	private final Map typeNamePropertyFactoryMap=new HashMap();	

		/**Registers a property factory by type name.
		@param typeName The type name for which this property factory can create RDF property resources.
		@param propertyFactory The property factory to be registered with this type name.
		*/	
		public void registerPropertyFactoryByTypeName(final String typeName, final RDFPropertyFactory propertyFactory)
		{
			typeNamePropertyFactoryMap.put(typeName.toLowerCase(), propertyFactory);	//put the property factory in the map, keyed to the lowercase version of the type name
		}
		
		/**Retrieves a property factory to create RDF property resources for the given type name.
		@param typeName The type name for which a property factory should be returned.
		@return A property factory for this type name, or <code>null</code> if there
			is no property factory registered for this type name.
		*/ 
		protected RDFPropertyFactory getPropertyFactoryByTypeName(final String typeName)
		{
			return (RDFPropertyFactory)typeNamePropertyFactoryMap.get(typeName.toLowerCase());	//get the property factory keyed to the lowercase version of this type name
		}
		
	/**A map of property factories keyed to the lowercase version of the profile.*/
	private final Map profilePropertyFactoryMap=new HashMap();	

		/**Registers a property factory by profile.
		@param profile The profile for which this property factory can create RDF property resources.
		@param propertyFactory The property factory to be registered with this profile.
		*/	
		public void registerPropertyFactoryByProfile(final String profile, final RDFPropertyFactory propertyFactory)
		{
			profilePropertyFactoryMap.put(profile.toLowerCase(), propertyFactory);	//put the property factory in the map, keyed to the lowercase version of the profile
		}

		/**Retrieves a property factory to create RDF property resources for the given value profile.
		@param profile The profile for which a property factory should be returned.
		@return A property factory for this profile, or <code>null</code> if there
			is no property factory registered for this profile.
		*/ 
		protected RDFPropertyFactory getPropertyFactoryByProfile(final String profile)
		{
			return (RDFPropertyFactory)profilePropertyFactoryMap.get(profile.toLowerCase());	//get the property factory keyed to the lowercase version of this profile
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
		
	/**A map of property value factories keyed to the lowercase version of the profile.*/
	private final Map profilePropertyValueFactoryMap=new HashMap();	

		/**Registers a property value factory by profile.
		@param profile The profile for which this property value factory can create RDF objects.
		@param propertyValueFactory The property value factory to be registered with this profile.
		*/	
		public void registerPropertyValueFactoryByProfile(final String profile, final RDFPropertyValueFactory propertyValueFactory)
		{
			profilePropertyValueFactoryMap.put(profile.toLowerCase(), propertyValueFactory);	//put the property value factory in the map, keyed to the lowercase version of the profile
		}

		/**Retrieves a property value factory to create RDF objects for the given value profile.
		@param profile The profile for which a property value factory should be returned.
		@return A property value factory for this profile, or <code>null</code> if there
			is no property value factory registered for this profile.
		*/ 
		protected RDFPropertyValueFactory getPropertyValueFactoryByProfile(final String profile)
		{
			return (RDFPropertyValueFactory)profilePropertyValueFactoryMap.get(profile.toLowerCase());	//get the property value factory keyed to the lowercase version of this profile
		}

	/**Default constructor.*/
	public DirectoryRDFProcessor()
	{
			//register property URIs for the predefined types
		registerPropertyURI(SOURCE_TYPE, RDFUtilities.createReferenceURI(DIRECTORY_NAMESPACE_URI, SOURCE_TYPE.toLowerCase()));	//SOURCE		
		registerPropertyURI(NAME_TYPE, RDFUtilities.createReferenceURI(DIRECTORY_NAMESPACE_URI, SOURCE_TYPE.toLowerCase()));	//NAME
		registerPropertyURI(PROFILE_TYPE, RDFUtilities.createReferenceURI(DIRECTORY_NAMESPACE_URI, SOURCE_TYPE.toLowerCase()));	//PROFILE		
		registerPropertyURI(BEGIN_TYPE, RDFUtilities.createReferenceURI(DIRECTORY_NAMESPACE_URI, SOURCE_TYPE.toLowerCase()));	//BEGIN
		registerPropertyURI(END_TYPE, RDFUtilities.createReferenceURI(DIRECTORY_NAMESPACE_URI, SOURCE_TYPE.toLowerCase()));	//END		
			//register ourselves as a property value factory for the standard value types
		registerPropertyValueFactoryByValueType(URI_VALUE_TYPE, this);			
		registerPropertyValueFactoryByValueType(TEXT_VALUE_TYPE, this);		
		registerPropertyValueFactoryByValueType(DATE_VALUE_TYPE, this);		
		registerPropertyValueFactoryByValueType(TIME_VALUE_TYPE, this);		
		registerPropertyValueFactoryByValueType(DATE_TIME_VALUE_TYPE, this);		
		registerPropertyValueFactoryByValueType(INTEGER_VALUE_TYPE, this);		
		registerPropertyValueFactoryByValueType(BOOLEAN_VALUE_TYPE, this);		
		registerPropertyValueFactoryByValueType(FLOAT_VALUE_TYPE, this);		
	}

	/**Constructor that specifies an existing data model to continue filling.
	@param rdf The RDF data model to use.
	*/
	public DirectoryRDFProcessor(final RDF rdf)
	{
		super(rdf);  //construct the parent class
	}

	/**Processes a directory and converts content lines into property of the
		given RDF resource.
	<p>For each content line, an RDF property resource and an RDF value object
		are created and added to the resource as a property. The property and
		value are created using factories obtained in the following manner:</p>
	<ol>
		<li>Obtain a property factory from the type name.</li>
		<li>If no property factory is available for the type name, or it cannot
			create a property resource, obtain a property factory for the profile.</li>
		<li>If no property factory has been obtained, attempt to create a property
			for one of the predefined types using the processor property factory.</li> 
		<li>If no RDF property object can be created, the content line is ignored.</li>
		<li>If the content line has an explicit value type, obtain a property value
			factory for that value type.</li>
		<li>If no property value factory is available for the value type, or it
			cannot create a property value, obtain a property value factory for the
			profile.</li>
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
			RDFResource property=null;	//we'll try to get the property resource to use
				//try to create a property from a property factory registered with the type name
			final RDFPropertyFactory typeNamePropertyFactory=getPropertyFactoryByTypeName(contentLine.getTypeName());	//try to get a property factory for the type name
			if(typeNamePropertyFactory!=null)	//if there is a property factory for this type name
			{
				property=typeNamePropertyFactory.createProperty(rdf, contentLine);	//try to create a property
			}
				//try to create a property from a property factory registered with the profile
			if(property==null && contentLine.getProfile()!=null)	//if there is yet no property, and this content line has a profile
			{
				final RDFPropertyFactory profilePropertyFactory=getPropertyFactoryByProfile(contentLine.getProfile());	//try to get a property factory for the profile
				if(profilePropertyFactory!=null)	//if there is a property factory for this profile
				{
					property=profilePropertyFactory.createProperty(rdf, contentLine);	//try to create a property
				}
			}
				//try to create a property from our own property factory
			if(property==null)	//if there is still no property
			{
				property=createProperty(rdf, contentLine);	//try to create a property ourselves
			}
			if(property!=null)	//if we have a property
			{
				RDFObject value=null;	//we'll try to create an RDF object from the value
					//try to create a property value from a property value factory registered with the value type
				final String valueType=contentLine.getParamValue(VALUE_PARAM_NAME);	//get the value type parameter
				if(valueType!=null)	//if there is an explicit value type
				{
					final RDFPropertyValueFactory valueTypePropertyValueFactory=getPropertyValueFactoryByValueType(valueType);	//try to get a property value factory registered with the value type
					if(valueTypePropertyValueFactory!=null)	//if we have a property value factory registered with the value type
					{
						value=valueTypePropertyValueFactory.createPropertyValue(rdf, contentLine);	//ask the factory to create an RDF object for the value 
					}
				}
					//try to create a property value from a property value factory registered with the profile
				if(value==null && contentLine.getProfile()!=null)	//if there is yet no value, and this content line has a profile
				{
					final RDFPropertyValueFactory profilePropertyValueFactory=getPropertyValueFactoryByProfile(contentLine.getProfile());	//try to get a property value factory for the profile
					if(profilePropertyValueFactory!=null)	//if there is a property value factory for this profile
					{
						value=profilePropertyValueFactory.createPropertyValue(rdf, contentLine);	//try to create an an RDF object for the value
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
	
	/**Creates an RDF property resource to represent the given directory
		content line.
	@param rdf The RDF data model to use when creating the RDF property resource.
	@param contentLine The directory content line to be converted to an RDF
		property.
	@return An RDF property resource representing the directory content line,
		or <code>null</code> if an RDF property resource cannot be creatd. 
	*/
	public RDFResource createProperty(final RDF rdf, final ContentLine contentLine)
	{
		final URI propertyURI=getPropertyURI(contentLine.getTypeName());	//get whatever property URI we have associated with this type name, if any
		return propertyURI!=null ? rdf.locateResource(propertyURI) : null;	//if we have a property URI, return a resource for it from the RDF data model
	}	

	/**Creates an RDF object to represent the value of the given directory
		content line.
	@param rdf The RDF data model to use when creating the RDF objects.
	@param contentLine The directory content line to be converted to an RDF
		object.
	@return An RDF object representing the value of the directory content line,
		or <code>null</code> if an RDF object cannot be creatd. 
	*/
	public RDFObject createPropertyValue(final RDF rdf, final ContentLine contentLine)
	{
		final String valueType=contentLine.getParamValue(VALUE_PARAM_NAME);	//get the value type parameter
		if(valueType!=null)	//if there is an explicit value type
		{ 
			if(TEXT_VALUE_TYPE.equalsIgnoreCase(valueType))	//if this is the "text" value type
			{
				return new Literal(contentLine.getValue().toString());	//return the string value of the content line
			}
		}
		return null;	//show that we can't create a value
	}

}