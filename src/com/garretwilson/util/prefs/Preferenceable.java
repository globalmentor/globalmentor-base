package com.garretwilson.util.prefs;

import java.util.prefs.Preferences;

/**Indicates that an object can store and retrieve preferences.
@author Garret Wilson
*/
public interface Preferenceable
{

	/**@return The preferences that should be used for this object, or the default preferences for this class if no preferences are specifically set.
	@exception SecurityException Thrown if a security manager is present and it denies <code>RuntimePermission("preferences")</code>.
	*/
	public Preferences getPreferences() throws SecurityException;
	
	/**Sets the preferences to be used for this panel.
	@param preferences The preferences that should be used for this panel, or <code>null</code> if the default preferences for this class should be used
	*/
	public void setPreferences(final Preferences preferences);

}
