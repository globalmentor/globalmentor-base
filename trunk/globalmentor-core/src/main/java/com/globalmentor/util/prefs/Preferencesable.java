/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.util.prefs;

import java.util.prefs.Preferences;

/**Indicates that an object can store and retrieve preferences.
@author Garret Wilson
*/
public interface Preferencesable
{

	/**@return The preferences that should be used for this object, or the default preferences for this class if no preferences are specifically set.
	@throws SecurityException Thrown if a security manager is present and it denies <code>RuntimePermission("preferences")</code>.
	*/
	public Preferences getPreferences() throws SecurityException;
	
	/**Sets the preferences to be used for this panel.
	@param preferences The preferences that should be used for this panel, or <code>null</code> if the default preferences for this class should be used
	*/
	public void setPreferences(final Preferences preferences);

}
