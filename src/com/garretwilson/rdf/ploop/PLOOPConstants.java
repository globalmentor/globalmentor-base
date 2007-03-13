package com.garretwilson.rdf.ploop;

import java.net.URI;

/**Constants used in PLOOP processing.
@author Garret Wilson
*/
public class PLOOPConstants
{

	/**The recommended prefix to the PLOOP ontology namespace.*/
	public final static String PLOOP_NAMESPACE_PREFIX="ploop";
	/**The URI to the PLOOP namespace.*/
	public final static URI PLOOP_NAMESPACE_URI=URI.create("http://ploop.org/namespaces/ploop#");

	/**The recommended prefix to the PLOOP property ontology namespace.*/
	public final static String PLOOP_PROPERTY_NAMESPACE_PREFIX="property";
	/**The URI to the PLOOP property ontology namespace.*/
	public final static URI PLOOP_PROPERTY_NAMESPACE_URI=URI.create("http://ploop.org/namespaces/property#");
}