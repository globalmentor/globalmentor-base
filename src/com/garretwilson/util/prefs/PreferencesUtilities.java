package com.garretwilson.util.prefs;

import com.garretwilson.lang.ClassUtilities;

/**Utilities methods for working with preferences.
@author Garret Wilson
*/
public class PreferencesUtilities
{
	/**Constructs a name for a preference based upon a given class and preference
		name, in the form "<em>classname</code>.<em>name</em>".
	@param c The class the lowercase local name of which will be used in
		constructing the name.
	@param name The locall name of the preference.
	@return A name suitable for storing the preference for the class.
	*/
	public final static String getPreferenceName(final Class c, final String name)
	{
			//use the lowercase local name of the class, because uppercase letters get a '/' character inserted 
		return ClassUtilities.getLocalName(c).toLowerCase()+'.'+name;	//return the lowercase version of the classname with the name appended, separated by a period
	}
}
