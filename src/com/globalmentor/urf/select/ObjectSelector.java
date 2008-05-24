package com.globalmentor.urf.select;

import java.net.URI;

import com.globalmentor.net.Resource;
import com.globalmentor.urf.*;
import static com.globalmentor.urf.URF.*;
import static com.globalmentor.urf.select.Select.*;

/**A selector that selects a resource based its equality to a runtime object represented by a resource.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
@see Object#equals(Object)
*/
public class ObjectSelector extends AbstractSelector
{

	/**Default constructor.*/
	public ObjectSelector()
	{
		this(null);	//construct the class with no URI
	}

	/**URI constructor.
	@param uri The URI for the new resource.
	*/
	public ObjectSelector(final URI uri)
	{
		super(uri);  //construct the parent class
	}

	/**Returns the object identified by this selector.
	@return This selector's object designation, or <code>null</code> if this selector has no <code>selectObject</code> property or the resource does not represent an object.
	@see Select#SELECT_OBJECT_PROPERTY_URI
	@see URF#asObject(Resource)
	*/
	public Object getSelectObject()
	{
		return asObject(getPropertyValue(SELECT_OBJECT_PROPERTY_URI));	//get the selectObject property
	}

	/**Determines if this selector selects a given object.
	A selector with no select object will not match any resources.
	This version returns <code>true</code> the the select object resource can be converted to an object the {@link Object#equals(Object)} method of which returns <code>true</code> for the given object.
	@param object The object to test for a match, or <code>null</code> if there is no object.
	@return <code>true</code> if this selector selects the given object.
	@see #getSelectObject()
	@see Object#equals(Object)
	*/
	public boolean selects(final Object object)
	{
		final Object selectObject=getSelectObject();	//get the select object, if any
		return selectObject!=null && selectObject.equals(object);	//if there is a select object, see if the given object is equal to it
	}
}