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

	/**Default constructor.*/
	public DirectoryRDFProcessor()
	{
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
			final ContentLine contentLint=(ContentLine)contentLineIterator.next();	//get the next content line
			
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
		return null;	//TODO fix to return the correct property resource
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
		if(TEXT_VALUE_TYPE.equalsIgnoreCase(valueType))	//if this is the "text" value type
		{
			return new Literal(contentLine.getValue().toString());	//return the string value of the content line
		}
		return null;	//show that we can't create a value
	}

}