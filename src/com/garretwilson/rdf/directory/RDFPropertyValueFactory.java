package com.garretwilson.rdf.directory;

import com.garretwilson.rdf.*;
import com.garretwilson.text.directory.ContentLine;

/**Class that can create an RDF object to represent the value of content lines
	of a directory of type <code>text/directory</code> as
	defined in 
	<a href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>,
	"A MIME Content-Type for Directory Information".
@author Garret Wilson
*/
public interface RDFPropertyValueFactory
{
	
	/**Creates an RDF object to represent the value of the given directory
		content line.
	@param rdf The RDF data model to use when creating the RDF objects.
	@param contentLine The directory content line to be converted to an RDF
		object.
	@return An RDF object representing the value of the directory content line,
		or <code>null</code> if an RDF object cannot be creatd. 
	*/
	public RDFObject createPropertyValue(final RDF rdf, final ContentLine contentLine);
	
}
