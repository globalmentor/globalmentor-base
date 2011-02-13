/*
 * Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.urf.select;

import java.net.URI;

import com.globalmentor.net.Resource;
import com.globalmentor.urf.*;
import static com.globalmentor.urf.URF.*;
import static com.globalmentor.urf.select.Select.*;

/**A selector that selects a resource based its equality to a runtime object represented by a resource.
This implementation performs special comparisons on numbers to allow the URF integer and floating point types to select the multiple Java integer and floating point types.   
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