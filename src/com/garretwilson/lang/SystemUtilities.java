package com.garretwilson.lang;

/**Utilities for working with system-specific attributes.
@author Garret Wilson
*/
public class SystemUtilities implements SystemConstants
{

	/**@return <code>true</code> if the operating system is a version of Windows.*/
	public static boolean isWindowsOS()
	{
		final String osName=System.getProperty(OS_NAME_PROPERTY);	//get the name of the operating system
		return StringUtilities.indexOfIgnoreCase(osName, SystemConstants.WINDOWS)>=0;	//we're running on Windows if the operating system name has "windows" in it somewhere
	}
}
