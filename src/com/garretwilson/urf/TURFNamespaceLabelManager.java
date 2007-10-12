package com.garretwilson.urf;

import java.net.URI;
import java.util.*;

import com.garretwilson.net.AbstractNamespaceLabelManager;

/**Map managing namespace URIs and labels for TURF.
<p>This class is not thread safe.</p>
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) <http://www.urf.name/> specification and processing
written by Garret Wilson <http://www.garretwilson.com/> and Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public class TURFNamespaceLabelManager extends AbstractNamespaceLabelManager
{

	/**Default constructor using a hash map.*/
	public TURFNamespaceLabelManager()
	{
		this(new HashMap<URI, String>());	//construct the class with a hash map
	}

	/**Map constructor.
	@param map The map this map should decorate.
	@exception NullPointerException if the provided map is <code>null</code>.
	*/
	public TURFNamespaceLabelManager(final Map<URI, String> map)
	{
		super(map);	//construct the parent class
	}

	/**Determines whether the given string is a valid label
	This version determines whether the string is valid TURF name.
	@param string The string to check for being a label.
	@return <code>true</code> if the given string represents a valid label.
	@exception NullPointerException if the given string is <code>null</code>.
	@see TURF#isName(String)
	*/
	protected boolean isLabel(final String string)
	{
		return TURF.isName(string);	//return whether the given string is a valid TURF name
	}

}
