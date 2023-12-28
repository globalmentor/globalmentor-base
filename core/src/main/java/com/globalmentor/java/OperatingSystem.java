/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <https://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.java;

import java.nio.file.Path;
import java.nio.file.Paths;

import static com.globalmentor.java.Strings.*;

/**
 * Utilities for working with operating system-specific attributes.
 * @author Garret Wilson
 */
public final class OperatingSystem {

	private OperatingSystem() {
	}

	/** The property specifying the line separator character for the platform. */
	public static final String LINE_SEPARATOR_PROPERTY = "line.separator";
	/** The temporary directory name. */
	public static final String JAVA_IO_TMPDIR_PROPERTY = "java.io.tmpdir";
	/** The name of the operating system. */
	public static final String OS_NAME_PROPERTY = "os.name";
	/** The operating system architecture. */
	public static final String OS_ARCH_PROPERTY = "os.arch";
	/** The operating system version. */
	public static final String OS_VERSION_PROPERTY = "os.version";
	/** The user name property. */
	public static final String USER_NAME_PROPERTY = "user.name";
	/** The property for the user's home directory. */
	public static final String USER_HOME_PROPERTY = "user.home";
	/** The property for the user's current working directory. */
	public static final String USER_DIR_PROPERTY = "user.dir";

	/** The string, "windows", which is a case-insensitive substring of a Windows operating system identification string. */
	public static final String WINDOWS = "windows";

	/**
	 * Returns the home directory of the user.
	 * @return The user home directory.
	 * @throws SecurityException if a security manager exists and its <code>checkPropertyAccess</code> method doesn't allow access to this system property.
	 * @see #USER_HOME_PROPERTY
	 */
	public static Path getUserHomeDirectory() throws SecurityException {
		return Paths.get(System.getProperty(USER_HOME_PROPERTY)); //return the user home directory
	}

	/**
	 * Returns the system temporary directory
	 * @return The system temporary directory directory.
	 * @throws SecurityException if a security manager exists and its <code>checkPropertyAccess</code> method doesn't allow access to this system property.
	 * @see #JAVA_IO_TMPDIR_PROPERTY
	 */
	public static Path getTempDirectory() throws SecurityException {
		return Paths.get(System.getProperty(JAVA_IO_TMPDIR_PROPERTY)); //return the system temporary directory
	}

	/**
	 * Returns the current directory of the user.
	 * @return The user's current working directory.
	 * @throws SecurityException if a security manager exists and its <code>checkPropertyAccess</code> method doesn't allow access to this system property.
	 * @see #USER_DIR_PROPERTY
	 */
	public static Path getWorkingDirectory() throws SecurityException {
		return Paths.get(System.getProperty(USER_DIR_PROPERTY)); //return the user working directory
	}

	/**
	 * Indicates whether the operating system is Windows.
	 * @return <code>true</code> if the operating system is a version of Windows.
	 * @throws SecurityException if a security manager exists and its <code>checkPropertyAccess</code> method doesn't allow access to this system property.
	 * @see #OS_NAME_PROPERTY
	 */
	public static boolean isWindowsOS() {
		final String osName = System.getProperty(OS_NAME_PROPERTY); //get the name of the operating system
		return indexOfIgnoreCase(osName, WINDOWS) >= 0; //we're running on Windows if the operating system name has "windows" in it somewhere
	}

}
