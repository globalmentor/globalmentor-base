package com.garretwilson.rdf.directory;

import java.net.URI;

/**Constants used in RDF processing of a directory of type
	<code>text/directory</code> as defined in 
	<a href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>,
	"A MIME Content-Type for Directory Information"..
@author Garret Wilson
*/
public interface DirectoryConstants
{
	/**The recommended prefix to the directory ontology namespace.*/
	public final static String DIRECTORY_NAMESPACE_PREFIX="directory";
	/**The URI to the directory namespace.*/
	public final static URI DIRECTORY_NAMESPACE_URI=URI.create("http://globalmentor.com/namespaces/2003/directory#");
}
