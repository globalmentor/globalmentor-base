package com.garretwilson.urf.select;

import java.net.URI;

import static com.garretwilson.urf.URF.*;

/**The URF select ontology.
@author Garret Wilson
*/
public class Select
{

	/**The recommended prefix to the URF select namespace.*/
	public final static String SELECT_NAMESPACE_PREFIX="select";
	/**The URI to the URF select namespace.*/
	public final static URI SELECT_NAMESPACE_URI=URI.create("http://urf.name/select");

		//properties
	/**Specifies the selector to select one or more resources.*/
	public final static URI SELECT_PROPERTY_URI=createResourceURI(SELECT_NAMESPACE_URI, "select");
	/**Specifies the selected class of a class selector.*/
	public final static URI SELECT_CLASS_PROPERTY_URI=createResourceURI(SELECT_NAMESPACE_URI, "selectClass");
	/**Specifies the name of a selected property of a property selector.*/
	public final static URI SELECT_PROPERTY_NAME_PROPERTY_URI=createResourceURI(SELECT_NAMESPACE_URI, "selectPropertyName");
	/**Specifies the selected URI of a URI selector.*/
	public final static URI SELECT_URI_PROPERTY_URI=createResourceURI(SELECT_NAMESPACE_URI, "selectURI");

}
