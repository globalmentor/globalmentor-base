package com.garretwilson.rdf;

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
	@return A typed literal containing a value object mapped from the given
		lexical form and datatype
	*/
	public RDFTypedLiteral createTypedLiteral(final String lexicalForm, final URI datatypeURI);

}