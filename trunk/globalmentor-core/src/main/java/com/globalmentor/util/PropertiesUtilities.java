/*
 * Copyright © 1996-2013 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.util;

import static com.globalmentor.io.Charsets.*;
import static com.globalmentor.net.URIs.*;
import static com.globalmentor.text.xml.XML.*;

import java.io.*;
import java.util.*;

import com.globalmentor.io.BOMInputStreamReader;

/**
 * Provides convenience access routines to {@link Properties}.
 * @author Garret Wilson
 */
public class PropertiesUtilities {

	/** The name extension for properties files, such as Java properties files. */
	public final static String PROPERTIES_NAME_EXTENSION = "properties";

	/** A string representation of boolean <code>true</code>. */
	public final static String TRUE_STRING = String.valueOf(true);

	/** A string representation of boolean <code>false</code>. */
	public final static String FALSE_STRING = String.valueOf(false);

	/**
	 * Retrieves a property as a boolean value.
	 * @param properties The properties object from which to retrieve the value.
	 * @param propertyName The name of the property to access.
	 * @return The boolean value of the named property, or false if there is no value or the value does not represent a boolean value.
	 * @see Properties#getProperty
	 */
	public static boolean getBooleanProperty(final Properties properties, final String propertyName) {
		return getBooleanProperty(properties, propertyName, false); //get the boolean version of the property, specifying false as the default
	}

	/**
	 * Retrieves a property as a boolean value, returning the default if no value is present.
	 * @param properties The properties object from which to retrieve the value.
	 * @param propertyName The name of the property to access.
	 * @param defaultValue The value to return if there is no value present.
	 * @return The boolean value of the named property or, if there is no value or the value does not represent a boolean value, the default value.
	 * @see Properties#getProperty
	 */
	public static boolean getBooleanProperty(final Properties properties, final String propertyName, final boolean defaultValue) {
		final String propertyValueString = properties.getProperty(propertyName); //get the property value
		//if the property value isn't null and is case-insensitively equal to the
		//  "true" string, return true; if not, return the default value
		if(TRUE_STRING.equalsIgnoreCase(propertyValueString)) //if this is "true" in some variation
			return true; //show that true was found
		else if(FALSE_STRING.equalsIgnoreCase(propertyValueString)) //if this is "false" in some variation
			return false; //show that false was found
		return defaultValue; //return the default value if the value is missing or isn't valid
	}

	/**
	 * Sets a boolean property by converting the boolean value to a string and storing it in the underlying properties.
	 * @param properties The properties object in which to set the value.
	 * @param propertyName The name of the property to set.
	 * @param propertyValue The new boolean value of the property.
	 * @see Properties#setProperty
	 */
	public static void setProperty(final Properties properties, final String propertyName, final boolean propertyValue) {
		properties.setProperty(propertyName, propertyValue ? TRUE_STRING : FALSE_STRING); //set the property value using either the true string or the false string to represent the value
	}

	/**
	 * Retrieves a property as a file, returning the default if no value is present.
	 * @param properties The properties object from which to retrieve the value.
	 * @param propertyName The name of the property to access.
	 * @param defaultValue The value to return if there is no value present.
	 * @return The value of the named property or, if there is no value, the default value.
	 * @see Properties#getProperty
	 */
	public static File getFileProperty(final Properties properties, final String propertyName, final File defaultValue) {
		final String propertyValueString = properties.getProperty(propertyName); //get the property value
		return propertyValueString != null ? new File(propertyValueString) : defaultValue; //return the value or the default
	}

	/**
	 * Sets a file property by converting the file to a string and storing it in the underlying properties.
	 * @param properties The properties object in which to set the value.
	 * @param propertyName The name of the property to set.
	 * @param propertyValue The new file value of the property.
	 * @see Properties#setProperty
	 */
	public static void setProperty(final Properties properties, final String propertyName, final File propertyValue) {
		properties.setProperty(propertyName, propertyValue.toString()); //set the property value using the given integer
	}

	/**
	 * Retrieves a property as an integer value, returning the default if no value is present.
	 * @param properties The properties object from which to retrieve the value.
	 * @param propertyName The name of the property to access.
	 * @param defaultValue The value to return if there is no value present.
	 * @return The integer value of the named property or, if there is no value or the value does not represent a integer value, the default value.
	 * @see Properties#getProperty
	 */
	public static int getIntProperty(final Properties properties, final String propertyName, final int defaultValue) {
		final String propertyValueString = properties.getProperty(propertyName); //get the property value
		try {
			return Integer.parseInt(propertyValueString); //parse the integer
		} catch(NumberFormatException numberFormatException) { //if this wasn't a valid integer
			return defaultValue; //return the default value if the value is missing or isn't valid
		}
	}

	/**
	 * Sets an integer property by converting the integer value to a string and storing it in the underlying properties.
	 * @param properties The properties object in which to set the value.
	 * @param propertyName The name of the property to set.
	 * @param propertyValue The new integer value of the property.
	 * @see Properties#setProperty
	 */
	public static void setProperty(final Properties properties, final String propertyName, final int propertyValue) {
		properties.setProperty(propertyName, Integer.toString(propertyValue)); //set the property value using the given integer
	}

	/**
	 * Converts the given map to a properties object. If the map is already a properties object, it is returned. Otherwise, a new properties object is created and
	 * populated with the entries of the given map.
	 * @param map The map to convert to a properties object.
	 * @return A properties object, potentially the same instance, containing entries from the given map.
	 */
	public static Properties toProperties(final Map<?, ?> map) {
		if(map instanceof Properties) { //if the map is already a properties object
			return (Properties)map; //return the map as a properties object
		} else { //if the map is not a properties object
			final Properties properties = new Properties(); //create a new properties object
			properties.putAll(map); //put all the properties from the map
			return properties; //return the populated properties object
		}
	}

	/**
	 * Loads properties for the given class from a properties resource.
	 * <p>
	 * The simple name of the class is used to determine the properties resource, either <code><var>classSimpleName</var>.properties</code> for the default
	 * properties file format or <code><var>classSimpleName</var>.xml</code> for the XML properties file format. The
	 * <code><var>classSimpleName</var>.properties</code> charset will reflect its BOM, if any; if no BOM is present, the ISO-8859-1 charset will be used as per
	 * the properties file format specification.
	 * </p>
	 * <p>
	 * The class loader of the given class is used for loading the resource.
	 * </p>
	 * @param objectClass The class in relation to which the properties should be loaded.
	 * @return The loaded properties.
	 * @throws FileNotFoundException if no properties resource could be found to load.
	 * @throws IOException if there is an error loading the properties.
	 * @see Class#getSimpleName()
	 * @see ClassLoader#getResourceAsStream(String)
	 */
	public static Properties loadPropertiesResource(final Class<?> objectClass) throws FileNotFoundException, IOException {
		return loadPropertiesResource(objectClass, objectClass.getSimpleName());
	}

	/**
	 * Loads properties for the given class from a properties resource determined from a given base name.
	 * <p>
	 * The base name is used to determine the properties resource, either <code><var>baseName</var>.properties</code> for the default properties file format or
	 * <code><var>baseName</var>.xml</code> for the XML properties file format. The <code><var>baseName</var>.properties</code> charset will reflect its BOM, if
	 * any; if no BOM is present, the ISO-8859-1 charset will be used as per the properties file format specification.
	 * </p>
	 * <p>
	 * The class loader of the given class is used for loading the resource.
	 * </p>
	 * @param objectClass The class in relation to which the properties should be loaded.
	 * @param baseName The base name for determining the properties resource to load.
	 * @return The loaded properties.
	 * @throws FileNotFoundException if no properties resource could be found to load.
	 * @throws IOException if there is an error loading the properties.
	 * @see ClassLoader#getResourceAsStream(String)
	 */
	public static Properties loadPropertiesResource(final Class<?> objectClass, final String baseName) throws FileNotFoundException, IOException {
		final Properties properties = new Properties();
		//try to load baseName.properties
		InputStream propertiesStream = objectClass.getResourceAsStream(addNameExtension(baseName, PROPERTIES_NAME_EXTENSION));
		if(propertiesStream != null) {
			propertiesStream = new BufferedInputStream(propertiesStream); //buffer the stream
			final Reader propertiesReader = new BOMInputStreamReader(propertiesStream, ISO_8859_1_CHARSET); //check for a BOM, falling back to the default properties charset
			try {
				properties.load(propertiesReader);
				return properties;
			} finally {
				propertiesReader.close();
			}
		}
		//try to load baseName.xml
		propertiesStream = objectClass.getResourceAsStream(addNameExtension(baseName, XML_NAME_EXTENSION));
		if(propertiesStream != null) {
			propertiesStream = new BufferedInputStream(propertiesStream); //buffer the stream
			try {
				properties.loadFromXML(propertiesStream);
				return properties;
			} finally {
				propertiesStream.close();
			}
		}
		throw new FileNotFoundException("No properties resource " + baseName + " found for class " + objectClass);
	}

}