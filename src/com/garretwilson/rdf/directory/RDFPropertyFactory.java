package com.garretwilson.rdf.directory;

import com.garretwilson.rdf.*;
import com.garretwilson.text.directory.ContentLine;

/**Class that can create an RDF property resource to represent a content lines
	of a directory of type <code>text/directory</code> as
	defined in 
	<a href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>,
	"A MIME Content-Type for Directory Information".
@author Garret Wilson
*/
public interface RDFPropertyFactory
{
	
	/**Creates an RDF property resource to represent the given directory
		content line.
	@param rdf The RDF data model to use when creating the RDF property resource.
	@param contentLine The directory content line to be converted to an RDF
		property.
	@return An RDF property resource representing the directory content line,
		or <code>null</code> if an RDF property resource cannot be creatd. 
	*/
	public RDFResource createProperty(final RDF rdf, final ContentLine contentLine);
	
}
