package com.garretwilson.urf.ploop;

import java.net.URI;

import static com.garretwilson.lang.ClassUtilities.*;
import static com.garretwilson.urf.URF.*;

/**General PLOOP definitions.
@author Garret Wilson
*/
public class PLOOP
{

	/**The recommended prefix to the PLOOP namespace.*/
//TODO bring back	public final static String PLOOP_NAMESPACE_PREFIX="ploop";
	/**The URI to the PLOOP namespace.*/
//TODO bring back	public final static URI PLOOP_NAMESPACE_URI=URI.create("http://ploop.org/namespaces/ploop");

	/**The recommended prefix to the PLOOP ontology namespace.*/
	public final static String PLOOP_NAMESPACE_PREFIX="ploop";
	/**The URI to the PLOOP namespace.*/
	public final static URI PLOOP_NAMESPACE_URI=URI.create("http://ploop.org/namespaces/ploop#");

	/**The recommended prefix to the PLOOP property ontology namespace.*/
	public final static String PLOOP_PROPERTY_NAMESPACE_PREFIX="property";
	/**The URI to the PLOOP property ontology namespace.*/
	public final static URI PLOOP_PROPERTY_NAMESPACE_URI=URI.create("http://ploop.org/namespaces/property#");

	/**Determines the URF property URI for a property of a given object.
	@param object The object for which a property URI should be determined.
	@param propertyName The name of the property.
	@return The URF property URI for the object's property.
	@exception NullPointerException if the given object, and/or property name is <code>null</code>.
	*/
	public static URI getPropertyURI(final Object object, final String propertyName)
	{
		final URI namespaceURI=createInfoJavaURI(object.getClass().getPackage());	//the URI of the object's class package will be the namespace of the object's class
		return createResourceURI(namespaceURI, propertyName);	//create and return a property URI for the given property
	}

}