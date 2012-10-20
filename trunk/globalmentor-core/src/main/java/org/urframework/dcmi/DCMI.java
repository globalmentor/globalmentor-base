/*
 * Copyright © 2007-2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package org.urframework.dcmi;

import java.util.Locale;

import org.urframework.*;

import static org.urframework.URF.*;

/**
 * Constants and methods used for Dublin Core as stored in URF.
 * @author Garret Wilson
 * @see <a href="http://dublincore.org/documents/dcmi-namespace/">DCMI Namespace Policy</a>
 * @see <a href="http://dublincore.org/documents/dces/">Dublin Core Metadata Element Set, Version 1.1</a>
 */
public class DCMI extends com.globalmentor.dcmi.DCMI
{

	/**
	 * Returns the date of the resource.
	 * @param resource The resource for which the date should be returned.
	 * @return The date of the resource, or <code>null</code> if there is no date or the property does not contain an <code>urf.Date</code> or
	 *         <code>urf.DateTime</code>.
	 * @see #DATE_PROPERTY_URI
	 */
	public static AbstractURFDateTime getDate(final URFResource resource)
	{
		return asAbstractDateTime(resource.getPropertyValue(DATE_PROPERTY_URI)); //return the dc.date as a date or date time
	}

	/**
	 * Returns the creator of the resource.
	 * @param resource The resource the property of which should be located.
	 * @return The string value of the property, or <code>null</code> if there is no such property or the property value is not a string.
	 * @see #CREATOR_PROPERTY_URI
	 */
	public static String getCreator(final URFResource resource)
	{
		return asString(resource.getPropertyValue(CREATOR_PROPERTY_URI));
	}

	/**
	 * Sets the creator of the resource.
	 * @param resource The resource of which the property should be set.
	 * @param value The property value to set.
	 * @see #CREATOR_PROPERTY_URI
	 */
	public static void setCreator(final URFResource resource, final String value)
	{
		resource.setPropertyValue(CREATOR_PROPERTY_URI, value);
	}

	/**
	 * Sets the date of the resource.
	 * @param resource The resource the date to set.
	 * @param date The new date.
	 * @see #DATE_PROPERTY_URI
	 */
	public static void setDate(final URFResource resource, final AbstractURFDateTime date)
	{
		if(date instanceof URFDate) //if this is a date
		{
			resource.setPropertyValue(DATE_PROPERTY_URI, (URFDate)date); //create a date resource and set the resource's dc.date
		}
		else if(date instanceof URFDateTime) //if this is a date time
		{
			resource.setPropertyValue(DATE_PROPERTY_URI, (URFDateTime)date); //create a date time resource and set the resource's dc.date
		}
		else
		//if we don't recognize the type of abstract date time
		{
			throw new AssertionError("Unrecognized abstract date time type: " + date.getClass());
		}
	}

	/**
	 * Returns the description of the resource.
	 * @param resource The resource the property of which should be located.
	 * @return The string value of the property, or <code>null</code> if there is no such property or the property value is not a string.
	 * @see #DESCRIPTION_PROPERTY_URI
	 */
	public static String getDescription(final URFResource resource)
	{
		return asString(resource.getPropertyValue(DESCRIPTION_PROPERTY_URI));
	}

	/**
	 * Sets the description of the resource.
	 * @param resource The resource of which the property should be set.
	 * @param value The property value to set.
	 * @see #DESCRIPTION_PROPERTY_URI
	 */
	public static void setDescription(final URFResource resource, final String value)
	{
		resource.setPropertyValue(DESCRIPTION_PROPERTY_URI, value);
	}

	/**
	 * Returns the language of the resource.
	 * @param resource The resource the property of which should be located.
	 * @return The value of the first language property, or <code>null</code> if no such property exists or the property value does not contain a language
	 *         resource.
	 * @throws IllegalArgumentException if the language value resource represents a locale that does not have the correct syntax, such as if the language tag has
	 *           more than three components.
	 * @see #LANGUAGE_PROPERTY_URI
	 */
	public static Locale getLanguage(final URFResource resource)
	{
		return asLanguage(resource.getPropertyValue(LANGUAGE_PROPERTY_URI)); //return the language as a locale
	}

	/**
	 * Sets the language of the resource.
	 * @param resource The resource to which the property should be set.
	 * @param locale The property value to set.
	 * @see #LANGUAGE_PROPERTY_URI
	 */
	public static void setLanguage(final URFResource resource, final Locale locale)
	{
		resource.setPropertyValue(LANGUAGE_PROPERTY_URI, DEFAULT_URF_RESOURCE_FACTORY.createLanguageResource(locale)); //create a resource for the given locale and add it as the property
	}

	/**
	 * Returns the rights of the resource
	 * @param resource The resource the property of which should be located.
	 * @return The string value of the property, or <code>null</code> if there is no such property or the property value is not a string.
	 * @see #RIGHTS_PROPERTY_URI
	 */
	public static String getRights(final URFResource resource)
	{
		return asString(resource.getPropertyValue(RIGHTS_PROPERTY_URI));
	}

	/**
	 * Sets the rights of the resource.
	 * @param resource The resource of which the property should be set.
	 * @param value The property value to set.
	 * @see #RIGHTS_PROPERTY_URI
	 */
	public static void setRights(final URFResource resource, final String value)
	{
		resource.setPropertyValue(RIGHTS_PROPERTY_URI, value);
	}

	/**
	 * Returns the subject of the resource
	 * @param resource The resource the property of which should be located.
	 * @return The string value of the property, or <code>null</code> if there is no such property or the property value is not a string.
	 * @see #SUBJECT_PROPERTY_URI
	 */
	public static String getSubject(final URFResource resource)
	{
		return asString(resource.getPropertyValue(SUBJECT_PROPERTY_URI));
	}

	/**
	 * Returns the subjects of the resource
	 * @param resource The resource the property of which should be located.
	 * @return The string values of the property.
	 * @see #SUBJECT_PROPERTY_URI
	 */
	public static String[] getSubjects(final URFResource resource)
	{
		return asStrings(resource.getPropertyValues(SUBJECT_PROPERTY_URI));
	}

	/**
	 * Sets the subject of the resource.
	 * @param resource The resource of which the property should be set.
	 * @param value The property value to set.
	 * @see #SUBJECT_PROPERTY_URI
	 */
	public static void setSubject(final URFResource resource, final String value)
	{
		resource.setPropertyValue(SUBJECT_PROPERTY_URI, value);
	}

	/**
	 * Sets the subjects of the resource.
	 * @param resource The resource of which the property should be set.
	 * @param values The property values to set.
	 * @see #SUBJECT_PROPERTY_URI
	 */
	public static void setSubjects(final URFResource resource, final String... values)
	{
		resource.setPropertyValues(SUBJECT_PROPERTY_URI, values);
	}

	/**
	 * Returns the title of the resource
	 * @param resource The resource the property of which should be located.
	 * @return The string value of the property, or <code>null</code> if there is no such property or the property value is not a string.
	 * @see #TITLE_PROPERTY_URI
	 */
	public static String getTitle(final URFResource resource)
	{
		return asString(resource.getPropertyValue(TITLE_PROPERTY_URI));
	}

	/**
	 * Sets the title of the resource.
	 * @param resource The resource of which the property should be set.
	 * @param value The property value to set.
	 * @see #TITLE_PROPERTY_URI
	 */
	public static void setTitle(final URFResource resource, final String value)
	{
		resource.setPropertyValue(TITLE_PROPERTY_URI, value);
	}

}