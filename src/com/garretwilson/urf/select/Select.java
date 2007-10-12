package com.garretwilson.urf.select;

import java.net.URI;

import static com.garretwilson.urf.URF.*;

/**The URF select ontology.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) <http://www.urf.name/> specification and processing
written by Garret Wilson <http://www.garretwilson.com/> and Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public class Select
{

	/**The URI to the URF select namespace.*/
	public final static URI SELECT_NAMESPACE_URI=URI.create("http://urf.name/select");

		//properties
	/**Specifies the selected class of a class selector.*/
	public final static URI SELECT_CLASS_PROPERTY_URI=createResourceURI(SELECT_NAMESPACE_URI, "selectClass");
	/**Specifies the name of a selected property of a property selector.*/
	public final static URI SELECT_PROPERTY_NAME_PROPERTY_URI=createResourceURI(SELECT_NAMESPACE_URI, "selectPropertyName");
	/**Specifies the selected URI of a URI selector.*/
	public final static URI SELECT_URI_PROPERTY_URI=createResourceURI(SELECT_NAMESPACE_URI, "selectURI");
	/**Specifies the selector to select one or more resources.*/
	public final static URI SELECTOR_PROPERTY_URI=createResourceURI(SELECT_NAMESPACE_URI, "selector");

}
