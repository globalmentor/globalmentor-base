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

package com.globalmentor.java;

import java.io.File;

import static com.globalmentor.java.Strings.*;
import static com.globalmentor.java.SystemConstants.*;

/**Utilities for working with system-specific attributes.
@author Garret Wilson
*/
public class SystemUtilities
{

	/**Returns the system line separator string.
	@return The the system line separator string.
	@exception SecurityException if a security manager exists and its <code>checkPropertyAccess</code> method doesn't allow access to this system property.
	@see SystemConstants#LINE_SEPARATOR_PROPERTY
	*/
	public static String getLineSeparator() throws SecurityException
	{
		return System.getProperty(LINE_SEPARATOR_PROPERTY);	//return the line separator
	}

	/**Returns the home directory of the user.
	@return The user home directory.
	@exception SecurityException if a security manager exists and its <code>checkPropertyAccess</code> method doesn't allow access to this system property.
	@see SystemConstants#USER_HOME_PROPERTY
	*/
	public static File getUserHomeDirectory() throws SecurityException
	{
		return new File(System.getProperty(USER_HOME_PROPERTY));	//return the user home directory
	}

	/**@return <code>true</code> if the operating system is a version of Windows.
	@exception SecurityException if a security manager exists and its <code>checkPropertyAccess</code> method doesn't allow access to this system property.
	@see SystemConstants#OS_NAME_PROPERTY
	*/
	public static boolean isWindowsOS()
	{
		final String osName=System.getProperty(OS_NAME_PROPERTY);	//get the name of the operating system
		return indexOfIgnoreCase(osName, WINDOWS)>=0;	//we're running on Windows if the operating system name has "windows" in it somewhere
	}
}
