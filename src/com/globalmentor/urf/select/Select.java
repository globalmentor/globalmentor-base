package com.globalmentor.urf.select;

import java.net.URI;

import static com.globalmentor.urf.URF.*;

/**The URF select ontology.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
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
	/**Specifies the selected property of a property selector.*/
	public final static URI SELECT_PROPERTY_PROPERTY_URI=createResourceURI(SELECT_NAMESPACE_URI, "selectProperty");
	/**Specifies the name of a selected property of an object property selector.*/
	public final static URI SELECT_PROPERTY_NAME_PROPERTY_URI=createResourceURI(SELECT_NAMESPACE_URI, "selectPropertyName");
	/**Specifies the selected URI of a URI selector.*/
	public final static URI SELECT_URI_PROPERTY_URI=createResourceURI(SELECT_NAMESPACE_URI, "selectURI");
	/**Specifies the selector to select one or more resources.*/
	public final static URI SELECTOR_PROPERTY_URI=createResourceURI(SELECT_NAMESPACE_URI, "selector");

}
