package com.garretwilson.util.prefs;

import java.util.prefs.Preferences;

import com.garretwilson.lang.ClassUtilities;
import static com.garretwilson.lang.JavaConstants.*;

/**Utilities methods for working with preferences.
@author Garret Wilson
*/
public class PreferencesUtilities
{
	
	/**The character used to separate elements in preference paths.*/
	protected final static char PATH_SEPARATOR='/';
	
	/**Constructs a name for a preference based upon a given class and preference
		name, in the form "<var>classname</var>.<var>name</var>".
	@param c The class the lowercase local name of which will be used in
		constructing the name.
	@param name The locall name of the preference.
	@return A name suitable for storing the preference for the class.
	*/
	public final static String getPreferenceName(final Class<?> c, final String name)
	{
			//use the lowercase local name of the class, because uppercase letters get a '/' character inserted 
		return ClassUtilities.getLocalName(c).toLowerCase()+'.'+name;	//return the lowercase version of the classname with the name appended, separated by a period
	}

	/**Returns the preference node from the calling user's preference tree
	 	that is associated with this class relative to the package node
	 	(i.e. <var>packageNode</var>/<var>classNode</var>).
	@param c The class for which a user preference node is desired.
	@return The user preference node associated with the class.
	@throws NullPointerException if <var>c</var> is <code>null</code>.
	@throws SecurityException if a security manager is present and it denies <code>RuntimePermission("preferences")</code>.
	@see RuntimePermission
	*/
/*TODO del or keep if not needed
	public static Preferences getUserNodeForClass(final Class<?> c)
	{
		return Preferences.userRoot().node(getNodeName(c));	//return the user node for the class, using the node name for the class
	}
*/

	/**Deterines the absolute path name of the node corresponding to the specified class.
	This is implemented to use the entire class name with every occurence of '.' replaced with '/'.
	Implemented by consulting java.util.prefs.Preferences.java	1.25 04/06/21 by Josh Bloch.
	@param c The class for which a node name should be returned.
	@return The absolute path name of the node corresponding to the specified class.
	@throws IllegalArgumentException if the package has no node preferences node associated with it. 
	*/
/*TODO del or keep if not needed
	protected static String getNodeName(final Class<?> c)
	{
		if(c.isArray())	//if this class is an array
		{
			throw new IllegalArgumentException("Arrays have no associated preferences node.");
		}
     return String.valueOf(PATH_SEPARATOR)+c.getName().replace(PACKAGE_SEPARATOR, PATH_SEPARATOR).toLowerCase();	//replace the class separator with the preference path separator; use lowercase because in Windows Java prefixes every uppercase character with '/'
  }
*/
}
