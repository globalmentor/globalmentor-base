package com.garretwilson.rdf.directory;

import java.io.*;
import java.net.URI;
import java.util.*;
import com.garretwilson.net.URLUtilities;
import com.garretwilson.rdf.*;
import com.garretwilson.text.directory.*;
import static com.garretwilson.text.directory.DirectoryConstants.*;

import com.globalmentor.io.*;
import com.globalmentor.util.*;

/**RDF factory for the predefined types of a directory of type
	<code>text/directory</code> as defined in
	<a href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>,
	"A MIME Content-Type for Directory Information".
@author Garret Wilson
*/
public class PredefinedRDFProfile extends PredefinedProfile implements RDFPropertyFactory, RDFPropertyValueFactory
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
	public PredefinedRDFProfile()
	{
			//register property URIs for the predefined types
		registerPropertyURI(SOURCE_TYPE, RDFUtilities.createReferenceURI(Directory.DIRECTORY_NAMESPACE_URI, SOURCE_TYPE.toLowerCase()));	//SOURCE		
		registerPropertyURI(NAME_TYPE, RDFUtilities.createReferenceURI(Directory.DIRECTORY_NAMESPACE_URI, NAME_TYPE.toLowerCase()));	//NAME
	//G***del		registerPropertyURI(PROFILE_TYPE, RDFUtilities.createReferenceURI(DIRECTORY_NAMESPACE_URI, PROFILE_TYPE.toLowerCase()));	//PROFILE		
	//G***del		registerPropertyURI(BEGIN_TYPE, RDFUtilities.createReferenceURI(DIRECTORY_NAMESPACE_URI, BEGIN_TYPE.toLowerCase()));	//BEGIN
	//G***del		registerPropertyURI(END_TYPE, RDFUtilities.createReferenceURI(DIRECTORY_NAMESPACE_URI, END_TYPE.toLowerCase()));	//END		
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
		final URI propertyURI=getPropertyURI(contentLine.getName());	//get whatever property URI we have associated with this type name, if any
		return propertyURI!=null ? rdf.locateResource(propertyURI) : null;	//if we have a property URI, return a resource for it from the RDF data model
	}	

	/**Creates an RDF object to represent the value of the given directory
		content line.
	@param rdf The RDF data model to use when creating the RDF objects.
	@param contentLine The directory content line to be converted to an RDF
		object.
	@param valueType The type of directory value the content line represents.
	@return An RDF object representing the value of the directory content line,
		or <code>null</code> if an RDF object cannot be creatd. 
	*/
	public RDFObject createPropertyValue(final RDF rdf, final ContentLine contentLine, final String valueType)
	{
		if(TEXT_VALUE_TYPE.equalsIgnoreCase(valueType))	//if this is the "text" value type
		{
			return new RDFPlainLiteral(contentLine.getValue().toString());	//return the string value of the content line as a literal
/*G***fix
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
