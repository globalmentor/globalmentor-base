package com.garretwilson.lang;

import java.io.File;

import static com.garretwilson.lang.StringUtilities.*;
import static com.garretwilson.lang.SystemConstants.*;

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
