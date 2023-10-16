/*
 * Copyright © 1996-2013 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import java.net.*;
import java.util.ArrayList;
import java.util.List;

import static com.globalmentor.java.CharSequences.*;
import static com.globalmentor.java.Java.*;
import static com.globalmentor.net.URIs.*;

import com.globalmentor.net.*;

/**
 * Utilities for manipulating Java packages.
 * @author Garret Wilson
 */
public class Packages {

	/** This class cannot be publicly instantiated. */
	private Packages() {
	}

	/**
	 * Determines the Java package represented by the given URI. A URI represents a Java package if it has a {@value Java#JAVA_URI_SCHEME} scheme in the form
	 * <code>java:/<var>com</var>/<var>example</var>/<var>package</var>/</code>.
	 * @implSpec This implementation looks for a package using the class loader of the {@link Packages} class.
	 * @param resourceURI The URI which is expected to represent a Java package, or <code>null</code>.
	 * @return The Java package represented by the given URI, or <code>null</code> if the URI is not a <code>java:</code> URI.
	 * @throws IllegalArgumentException if the given URI represents a Java package that does not have the correct syntax, e.g. it does not have an absolute
	 *           collection path.
	 * @throws ClassNotFoundException if the package represented by the given URI could not be found.
	 * @see Java#JAVA_URI_SCHEME
	 */
	public static Package asPackage(final URI resourceURI) throws ClassNotFoundException {
		if(resourceURI != null && JAVA_URI_SCHEME.equals(resourceURI.getScheme())) { //if an java: URI was given
			final String classPath = resourceURI.getRawPath(); //get the path to the class
			if(classPath != null) { //if there is a path
				checkCollectionPath(classPath); //a package URI is a collection
				if(classPath.startsWith(ROOT_PATH)) { //if the path is absolute
					final String packageName = decode(classPath.substring(ROOT_PATH.length()).replace(PATH_SEPARATOR, PACKAGE_SEPARATOR)); //skip the root path delimiter, replace path separators with package separators, and decode the string before trying to load the class
					final Package pkg = Packages.class.getClassLoader().getDefinedPackage(packageName);
					if(pkg == null) {
						throw new ClassNotFoundException("Package not found: " + packageName);
					}
					return pkg;
				} else { //if the path is not absolute
					throw new IllegalArgumentException("Java URI " + resourceURI + " does not have an absolute path.");
				}
			} else { //if there is no path
				throw new IllegalArgumentException("Java URI " + resourceURI + " missing path.");
			}
		}
		return null; //no package could be found
	}

	/**
	 * Creates a Java URI for a Java package using the {@value Java#JAVA_URI_SCHEME} scheme in the form
	 * <code>java:/<var>com</var>/<var>example</var>/<var>package</var>/</code>.
	 * @param objectPackage The package to use in creating the <code>java:</code> URI.
	 * @return A <code>java:</code> URI based upon the given class.
	 * @throws NullPointerException if the given package name is <code>null</code>.
	 */
	public static URI createJavaURI(final Package objectPackage) {
		return createJavaURI(objectPackage.getName());
	}

	/**
	 * Creates a Java URI for a named Java package using the {@value Java#JAVA_URI_SCHEME} scheme in the form
	 * <code>java:/<var>com</var>/<var>example</var>/<var>package</var>/</code>.
	 * @param objectPackageName The name of the package to use in creating the <code>java:</code> URI.
	 * @return A <code>java:</code> URI based upon the given class.
	 * @throws NullPointerException if the given package name is <code>null</code>.
	 */
	public static URI createJavaURI(final String objectPackageName) {
		final String packagePath = URIPath.encodeSegment(objectPackageName).replace(PACKAGE_SEPARATOR, PATH_SEPARATOR); //get the package path by replacing the package separators with path separators after encoding
		return URI.create(JAVA_URI_SCHEME + SCHEME_SEPARATOR + ROOT_PATH + packagePath + PATH_SEPARATOR); //create and return a new Java URI for the package
	}

	/**
	 * Creates a full name given package and a local name. For example, a package of <code>com.example</code> and a local name of <code>Bar</code> will result in
	 * a full name of <code>com.example.Bar</code>.
	 * @param objectPackage The object to supply the package name.
	 * @param localName The local name for constructing the full name within the package.
	 * @return A full class name in the given package and the given local name.
	 * @see Classes#getFullName(Class, String)
	 */
	public static String getFullName(final Package objectPackage, final String localName) {
		return objectPackage.getName() + PACKAGE_SEPARATOR + localName; //return the package plus the name separated by a package separator
	}

	/**
	 * Determines if the given class is inside the named package.
	 * @param parentPackage The package against which to check.
	 * @param childClass The class to check to see if it is inside the given package.
	 * @return <code>true</code> if the given class is inside the given package.
	 * @throws NullPointerException if the given package and/or class is <code>null</code>.
	 */
	public static boolean isInsidePackage(final Package parentPackage, final Class<?> childClass) {
		return isInsidePackage(parentPackage.getName(), childClass.getName());
	}

	/**
	 * Determines if the given package is inside the named package.
	 * @param parentPackage The package against which to check.
	 * @param childPackage The package to check to see if it is inside the given package.
	 * @return <code>true</code> if the given child package is inside the given parent package.
	 * @throws NullPointerException if the given parent package and/or child package is <code>null</code>.
	 */
	public static boolean isInsidePackage(final Package parentPackage, final Package childPackage) {
		return isInsidePackage(parentPackage.getName(), childPackage.getName());
	}

	/**
	 * Determines if the given full name (which can represent the full name of a package or a class) is inside the named package.
	 * @param packageName The name of the package against which to check (e.g. <code>com.example</code>).
	 * @param fullName The full package or class name to check to see if it is inside the named package (e.g. <code>com.example.foo</code> or
	 *          <code>com.example.Bar</code> or <code>com.example.foo.Bar</code>).
	 * @return <code>true</code> if the full-named entity is inside the named package.
	 * @throws NullPointerException if the given package name and/or full name is <code>null</code>.
	 * @see Java#PACKAGE_SEPARATOR
	 */
	public static boolean isInsidePackage(final String packageName, final String fullName) {
		final int packageNameLength = packageName.length();
		if(fullName.length() < packageNameLength + 1) { //there must be enough room for the package name plus a package separator
			return false;
		}
		if(!fullName.startsWith(packageName)) { //we expect the full name to start with the package name
			return false;
		}
		if(fullName.charAt(packageNameLength) != PACKAGE_SEPARATOR) { //there must be a package separator '.' after the package name
			return false;
		}
		return true;
	}

	/**
	 * Determines the package of the given full name (which can represent the full name of a package or a class).
	 * @param fullName The full package or class name (e.g. <code>Example</code> or <code>com.example.foo</code> or <code>com.example.Bar</code> or
	 *          <code>com.example.foo.Bar</code>).
	 * @return The name of the direct enclosing package, or <code>null</code> if the full-named entity is not in a package.
	 * @throws NullPointerException if the given full name is <code>null</code>.
	 * @see Java#PACKAGE_SEPARATOR
	 */
	public static String getPackageName(final String fullName) {
		return truncateAtLast(fullName, PACKAGE_SEPARATOR).toString();
	}

	/**
	 * Retrieves a list of names of all parent packages of the given named package.
	 * @param packageName The name of the package for which parent packages should be listed.
	 * @return The list, possibly empty, of names of all parent packages, in order of child to parent packages.
	 * @throws NullPointerException if the given package name is <code>null</code>.
	 * @see Java#PACKAGE_SEPARATOR
	 */
	public static List<String> getParentPackageNames(final String packageName) {
		final List<String> parentPackageNames = new ArrayList<String>();
		int index = packageName.length() - 1;
		while((index = packageName.lastIndexOf(PACKAGE_SEPARATOR, index)) >= 0) { //keep looking backwards for the last package separator
			parentPackageNames.add(packageName.substring(0, index)); //get everything up to but not including the package separator
			--index; //start looking before the package separator
		}
		return parentPackageNames;
	}

}
