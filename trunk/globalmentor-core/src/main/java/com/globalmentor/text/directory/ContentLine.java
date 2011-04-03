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

package com.globalmentor.text.directory;

import java.util.*;

import static com.globalmentor.text.directory.Directory.*;

import com.globalmentor.model.NameValuePair;

/**
 * A line in a directory of type <code>text/directory</code> as defined in <a href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>,
 * "A MIME Content-Type for Directory Information".
 * @author Garret Wilson
 */
public class ContentLine extends NameValuePair<String, Object>
{

	/** The profile of this content line, or <code>null</code> if there is no profile. */
	private String profile = null;

	/** @return The profile of this content line, or <code>null</code> if there is no profile. */
	public String getProfile()
	{
		return profile;
	}

	/**
	 * Sets the profile.
	 * @param profile The profile of this content line, or <code>null</code> if there is no profile.
	 */
	public void setProfile(final String profile)
	{
		this.profile = profile;
	}

	/** The group specification, or <code>null</code> if there is no group. */
	private String group = null;

	/** @return The group specification, or <code>null</code> if there is no group. */
	public String getGroup()
	{
		return group;
	}

	/**
	 * Sets the group.
	 * @param group The group specification, or <code>null</code> if there is no group.
	 */
	public void setGroup(final String group)
	{
		this.group = group;
	}

	/** The list of parameters. */
	private final List<NameValuePair<String, String>> paramList;

	/**
	 * @return The list of parameters, each item of which is a <code>NameValuePair</code> with a name of type <code>String</code> and a value of type
	 *         <code>String</code>.
	 * @see NameValuePair
	 */
	public List<NameValuePair<String, String>> getParamList()
	{
		return paramList;
	}

	/**
	 * Creates a directory content line with a name and value.
	 * @param name The name of the information.
	 * @param value The value of the information.
	 */
	public ContentLine(final String name, final Object value)
	{
		this(null, name, value); //create a content line with no group
	}

	/**
	 * Creates a directory content line with a group, name, and value.
	 * @param group The group specification, or <code>null</code> if there is no group.
	 * @param name The name of the information.
	 * @param value The value of the information.
	 */
	public ContentLine(final String group, final String name, final Object value)
	{
		this(null, group, name, value); //create a content line with no profile
	}

	/**
	 * Creates a directory content line with a group, name, parameter list, and value.
	 * @param group The group specification, or <code>null</code> if there is no group.
	 * @param name The name of the information.
	 * @param paramList The list of parameters, each item of which is a <code>NameValuePair</code> with a name of type <code>String</code> and a value of type
	 *          <code>String</code>.
	 * @param value The value of the information.
	 */
	public ContentLine(final String group, final String name, final List<NameValuePair<String, String>> paramList, final Object value)
	{
		this(null, group, name, paramList, value); //create a content line with no profile
	}

	/**
	 * Creates a directory content line with a profile, group, name, and value.
	 * @param profile The profile of this content line, or <code>null</code> if there is no profile.
	 * @param group The group specification, or <code>null</code> if there is no group.
	 * @param name The name of the information.
	 * @param value The value of the information.
	 */
	public ContentLine(final String profile, final String group, final String name, final Object value)
	{
		this(profile, group, name, new ArrayList<NameValuePair<String, String>>(), value); //create a content line with an empty param list
	}

	/**
	 * Creates a directory content line with a profile, group, name, parameter list, and value.
	 * @param profile The profile of this content line, or <code>null</code> if there is no profile.
	 * @param group The group specification, or <code>null</code> if there is no group.
	 * @param name The name of the information.
	 * @param paramList The list of parameters, each item of which is a <code>NameValuePair</code> with a name of type <code>String</code> and a value of type
	 *          <code>String</code>.
	 * @param value The value of the information.
	 */
	public ContentLine(final String profile, final String group, final String name, final List<NameValuePair<String, String>> paramList, final Object value)
	{
		super(name, value); //construct the parent class
		this.profile = profile; //save the profile
		this.group = group; //save the group
		this.paramList = paramList; //save the parameter list
	}

	/**
	 * Retrieves a parameter value of the content line.
	 * @param paramName The name of the parameter, which will be matched against available parameters in a case insensitive way.
	 * @return The value of the first matching parameter, or <code>null</code> if there is no matching parameter.
	 */
	public String getParamValue(final String paramName)
	{
		return DirectoryUtilities.getParamValue(getParamList(), paramName); //get the value from the parameter list 
	}

	/** @return A string representation of this content line. */
	public String toString()
	{
		final StringBuilder stringBuilder = new StringBuilder(); //create a string builder to use in constructing the string
		if(getProfile() != null) //if there's a profile
		{
			stringBuilder.append('[').append(getProfile()).append(']').append(' '); //append the profile
		}
		if(getGroup() != null) //if there's a group
		{
			stringBuilder.append(getGroup()).append(GROUP_NAME_SEPARATOR_CHAR); //append the group
		}
		stringBuilder.append(getName()); //append the type name
		if(getParamList().size() > 0) //if there are parameters
		{
			for(final NameValuePair<String, String> parameter : getParamList()) //for each parameter
			{
				//append the parameter name and value
				stringBuilder.append(PARAM_SEPARATOR_CHAR).append(parameter.getName()).append(PARAM_NAME_VALUE_SEPARATOR_CHAR).append(parameter.getValue());
			}
		}
		stringBuilder.append(NAME_VALUE_SEPARATOR_CHAR).append(getValue().toString()); //append the value
		return stringBuilder.toString(); //return the string we constructed
	}
}
