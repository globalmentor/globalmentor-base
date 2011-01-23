package com.globalmentor.urf.select;

import java.net.URI;

import com.globalmentor.net.Resource;
import com.globalmentor.urf.*;
import static com.globalmentor.urf.URF.*;
import static com.globalmentor.urf.select.Select.*;

/**A selector that selects a resource based its equality to a runtime object represented by a resource.
This implementation performs special comparisons on numbers to allow the URF integer and floating point types to select the multiple Java integer and floating point types.   
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
	This implementation performs special comparisons on numbers to allow the URF integer and floating point types to select the multiple Java integer and floating point types.   
	@param object The object to test for a match, or <code>null</code> if there is no object.
	@return <code>true</code> if this selector selects the given object.
	@see #getSelectObject()
	@see Object#equals(Object)
	*/
	public boolean selects(final Object object)
	{
		final Object selectObject=getSelectObject();	//get the select object, if any
		if(selectObject==null || object==null)	//if either object is null
		{
			return false;	//the selector or the selectee must be non-null for a match
		}
		if(selectObject instanceof Number)	//if the selector is a number, do special comparisons (perform the extra test to reduce overall tests in most cases, assuming most selectors are not numbers)
		{
			if(selectObject instanceof Long)	//if the object represents an URF integer
			{
				if(object instanceof Integer)	//Java integers hold valid URF integer values
				{
					return ((Long)selectObject).longValue()==((Integer)object).longValue();	//compare long values
				}
			}
			else if(selectObject instanceof Double)	//if the object represents an URF rational
			{
				if(object instanceof Float)	//Java floats hold valid URF rational values
				{
					return Double.doubleToLongBits(((Double)selectObject).doubleValue())==Double.doubleToLongBits(((Float)object).doubleValue());	//compare long bits of double values
				}
			}
		}
		return selectObject.equals(object);	//by default just use the object equality method
	}
}