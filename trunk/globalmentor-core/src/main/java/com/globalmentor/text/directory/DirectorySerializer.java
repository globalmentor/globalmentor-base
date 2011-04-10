/*
 * Copyright © 1996-2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import java.io.*;
import java.util.*;

import static com.globalmentor.text.ABNF.*;
import static com.globalmentor.text.directory.Directory.*;

import com.globalmentor.model.LocaledText;
import com.globalmentor.model.NameValuePair;

/**
 * Class that can serialize a directory of type <code>text/directory</code> as defined in <a href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>,
 * "A MIME Content-Type for Directory Information".
 * @author Garret Wilson
 * @see Profile
 * @see PredefinedProfile
 */
public class DirectorySerializer
{

	/** The profile for the predefined types. */
	private final PredefinedProfile predefinedProfile = new PredefinedProfile();

	/** @return The profile for the predefined types. */
	protected PredefinedProfile getPredefinedProfile()
	{
		return predefinedProfile;
	}

	/** A map of profiles keyed to the lowercase version of the profile name. */
	private final Map<String, Profile> profileMap = new HashMap<String, Profile>();

	/**
	 * Registers a profile.
	 * @param profileName The name of the profile.
	 * @param profile The profile to be registered with this profilename.
	 */
	public void registerProfile(final String profileName, final Profile profile)
	{
		profileMap.put(profileName.toLowerCase(), profile); //put the profile in the map, keyed to the lowercase version of the profile name
	}

	/**
	 * Retrieves a profile for the given profile name.
	 * @param profileName The name of the profile to return, or <code>null</code> if the predefined profile should be returned.
	 * @return A profile for this profile name, or <code>null</code> if there is no profile registered for this profile name.
	 * @see #getPredefinedProfile
	 */
	protected Profile getProfile(final String profileName)
	{
		return profileName != null ? profileMap.get(profileName.toLowerCase()) : getPredefinedProfile(); //get the profile keyed to the lowercase version of the profile name, or return the predefined profile if null was passed
	}

	/** A map of value serializers keyed to the lowercase version of the value type. */
	final Map<String, ValueSerializer> valueSerializerMap = new HashMap<String, ValueSerializer>();

	/**
	 * Registers a value serializer by value type.
	 * @param valueType The value type for which this value serializer can serialize values.
	 * @param valueSerializer The value serializer to be registered with this value type.
	 */
	public void registerValueSerializer(final String valueType, final ValueSerializer valueSerializer)
	{
		valueSerializerMap.put(valueType.toLowerCase(), valueSerializer); //put the value serializer in the map, keyed to the lowercase version of the type
	}

	/**
	 * Retrieves a value serializer to serializer values for the given value type.
	 * @param valueType The value type for which a value serializer should be returned.
	 * @return A value serializer for this value type, or <code>null</code> if there is no value serializer registered for this value type.
	 */
	protected ValueSerializer getValueSerializer(final String valueType)
	{
		return valueSerializerMap.get(valueType.toLowerCase()); //get the value serializer keyed to the lowercase version of this value type
	}

	/** The profile last encountered in a "profile:" type content line. */
	private String defaultProfile = null;

	/** Whether the default profile was the last profile encountered. */
	private boolean useDefaultProfile = false;

	/**
	 * The stack of profiles encountered in a "begin:"/"end:" blocks; created before processing and released afterwards.
	 */
	private LinkedList<String> profileStack = null;

	/**
	 * Sets the profile to be used for subsequent content lines. If in the middle of a profile "begin:"/"end:" block, the profile of that block will be suspended
	 * until the block ends or another block begins.
	 * @param profile The new profile of the directory.
	 */
	protected void setProfile(final String profile)
	{
		defaultProfile = profile; //save the profile
		useDefaultProfile = true; //show that we should use the default profile
	}

	/**
	 * @return The current profile, either the last set profile, the profile of the current "begin:"/"end:" block, or <code>null</code> if there is no profile, in
	 *         that order.
	 */
	protected String getProfile()
	{
		if(useDefaultProfile && defaultProfile != null) //if we should use the default profile and there is a profile set
		{
			return defaultProfile; //return the last set profile
		}
		else if(profileStack.size() > 0) //if we're in a profile "begin:"/"end:" block
		{
			return profileStack.getLast(); //return the profile of the current block
		}
		else
		//if no profile is set, and we're not in a profile "begin:"/"end:" block
		{
			return defaultProfile; //if there's no profile "begin:"/"end:" block, we'll have to use the default profile, even if it is null
		}
	}

	/**
	 * Pushes the given profile on the stack, and removes the set profile, if any. Suspends the currently set profile, if any.
	 * @param profile The profile of the new "begin:"/"end:" block block.
	 */
	protected void pushProfile(final String profile)
	{
		profileStack.addLast(profile); //push the profile onto the stack
		useDefaultProfile = false; //suspend use of the default profile
	}

	/**
	 * Removes the profile from the top of the stack. Suspends the currently set profile, if any.
	 * @return The profile from the top of the stack.
	 * @exception NoSuchElementException Thrown if there are no more profiles on the stack.
	 */
	protected String popProfile()
	{
		useDefaultProfile = false; //suspend use of the default profile
		return (String)profileStack.removeLast(); //pop the profile from the stack
	}

	/**
	 * Default constructor. This class automatically registers the predefined profile as a value serializer for standard value types.
	 */
	public DirectorySerializer()
	{
		//register the predefined profile as a value serializer for the standard value types
		registerValueSerializer(URI_VALUE_TYPE, getPredefinedProfile());
		registerValueSerializer(TEXT_VALUE_TYPE, getPredefinedProfile());
		registerValueSerializer(DATE_VALUE_TYPE, getPredefinedProfile());
		registerValueSerializer(TIME_VALUE_TYPE, getPredefinedProfile());
		registerValueSerializer(DATE_TIME_VALUE_TYPE, getPredefinedProfile());
		registerValueSerializer(INTEGER_VALUE_TYPE, getPredefinedProfile());
		registerValueSerializer(BOOLEAN_VALUE_TYPE, getPredefinedProfile());
		registerValueSerializer(FLOAT_VALUE_TYPE, getPredefinedProfile());
	}

	/**
	 * Serializes content lines from a directory of type <code>text/directory</code>.
	 * @param contentLines An array of content lines in the directory.
	 * @param writer The writer to which the lines of the directory should be serialized.
	 * @exception IOException Thrown if there is an error writing to the directory.
	 */
	public void serializeContentLines(final ContentLine[] contentLines, final Writer writer) throws IOException
	{
		serializeContentLines(contentLines, new LineFoldWriter(writer)); //create a new line fold writer and use that to serialize the directory
	}

	/**
	 * Serializes content lines from a directory of type <code>text/directory</code>.
	 * @param contentLines An array of content lines in the directory.
	 * @param writer The writer to which the lines of the directory should be serialized.
	 * @exception IOException Thrown if there is an error writing to the directory.
	 */
	protected void serializeContentLines(final ContentLine[] contentLines, final LineFoldWriter writer) throws IOException
	{
		profileStack = new LinkedList<String>(); //create a new profile stack
		defaultProfile = null; //show that there is no default profile
		useDefaultProfile = false; //don't use the default profile
		for(int i = 0; i < contentLines.length; ++i) //look at each content line
		{
			final ContentLine contentLine = contentLines[i]; //get a reference to this content line
			serializeContentLine(contentLine, writer); //serialize the content line
		}
		profileStack = null; //release the profile stack
		defaultProfile = null; //show that there is no default profile
		useDefaultProfile = false; //don't use the default profile
	}

	/**
	 * Serializes a single content line from a directory of type <code>text/directory</code>.
	 * <p>
	 * The delimiter after the content line will be written.
	 * </p>
	 * <p>
	 * When attempting to find a {@link ValueSerializer} to process a given value, an attempt is made to locate a value serializer in this order:
	 * </p>
	 * <ol>
	 * <li>If no explicit value type is given and a profile name is known, the {@link Profile} registered for that profile, if any, is asked for the type.</li>
	 * <li>If no explicit value type is still not known, the predefined profile is asked for the predefined type name.</li>
	 * <li>If a profile name is known and the {@link Profile} registered with the profile, if any, implements {@link ValueSerializer}, it is asked to serialize
	 * the value object.</li>
	 * <li>If the value was not serialized created, if the value type is known, the {@link ValueSerializer} registered for the type, if any, is asked to serialize
	 * the value object.</li>
	 * <li>If the value was not serialized, the string value of the object value is serialized.</li>
	 * </ol>
	 * @param contentLine The directory content line to be serialized.
	 * @param writer The writer to which the lines of the directory should be serialized.
	 * @exception IOException Thrown if there is an error writing to the directory.
	 */
	public void serializeContentLine(final ContentLine contentLine, final Writer writer) throws IOException
	{
		final String group = contentLine.getGroup(); //get the line group
		if(group != null) //if there is a group
		{
			writer.write(group); //write the group
			writer.write(GROUP_NAME_SEPARATOR_CHAR); //write the group name separator
		}
		final String typeName = contentLine.getName(); //get the type name of the line
		//TODO del Log.trace("Serializing content line for type name: ", typeName);
		writer.write(typeName); //write the type name
		final List<NameValuePair<String, String>> paramList = contentLine.getParamList(); //get the list of parameters
		if(paramList.size() > 0) //if there are parameters
		{
			serializeParameters(paramList, writer); //serialize the parameters
		}
		writer.write(NAME_VALUE_SEPARATOR_CHAR); //write the content line name-value separator
		if(PROFILE_TYPE.equalsIgnoreCase(typeName)) //if this is PROFILE
		{
			final String profile = ((LocaledText)contentLine.getValue()).getText(); //get the profile
			contentLine.setProfile(profile); //a profile type should have the same profile as the one it sets
			setProfile(profile); //set the profile to the new profile
		}
		else if(BEGIN_TYPE.equalsIgnoreCase(typeName)) //if this is BEGIN:xxx
		{
			final String profile = ((LocaledText)contentLine.getValue()).getText(); //get the profile
			contentLine.setProfile(profile); //a beginning profile type should have the same profile as the one it sets
			pushProfile(profile); //push the new profile
		}
		else if(END_TYPE.equalsIgnoreCase(typeName)) //if this is END:xxx
		{
			final String profile = ((LocaledText)contentLine.getValue()).getText(); //get the profile
			contentLine.setProfile(profile); //an ending profile type should have the same profile to which it refers
			try
			{
				final String oldProfile = popProfile(); //pop the profile from the stack
				//TODO make sure the old profile is what we expect
			}
			catch(NoSuchElementException noSuchElementException) //if there are no more profiles on the stack
			{
				//TODO fix or del throw new ParseIOException("Profile \""+profile+"\" END without BEGIN.", reader);	//throw an error indicating that there was no beginning to the profile
			}
		}
		final Object value = contentLine.getValue(); //get the value of the content line
		boolean isValueSerialized = false; //start out without having serialized the value
		//get the name of the profile for this content line; if the content line has no profile, get the current profile as we've been keeping track of it
		final String profileName = contentLine.getProfile() != null ? contentLine.getProfile() : getProfile();
		//TODO del Log.trace("found profile name: ", profileName);
		final Profile profile = getProfile(profileName); //see if we have a profile registered with this profile name
		String valueType = getParamValue(paramList, VALUE_PARAM_NAME); //get the value type parameter value
		if(valueType == null) //if the value type wasn't explicitly given
		{
			if(profile != null) //if there is a profile for this profile name
			{
				valueType = profile.getValueType(profileName, group, typeName, paramList); //ask this profile's value factory for the value type
			}
			if(valueType == null && profile != getPredefinedProfile()) //if we still don't know the type, and we didn't already check the predefined profile 
			{
				valueType = getPredefinedProfile().getValueType(profileName, group, typeName, paramList); //ask the predefined profile for the value type
			}
		}
		//TODO del Log.trace("using profile type: ", typeName);
		if(profile instanceof ValueSerializer) //if our profile is a value serializer, use the profile as a value serializer
		{
			isValueSerialized = ((ValueSerializer)profile).serializeValue(profileName, group, typeName, paramList, value, valueType, writer); //serialize the value for this profile
		}
		if(!isValueSerialized && valueType != null) //if the value was not serialized, but we know the value type
		{
			final ValueSerializer valueSerializer = getValueSerializer(valueType); //see if we have a value serializer registered with this value type
			if(valueSerializer != null) //if there is a value serializer for this value type
			{
				isValueSerialized = valueSerializer.serializeValue(profileName, group, typeName, paramList, value, valueType, writer); //serialize the value for this value type
			}
		}
		if(!isValueSerialized) //if the value was not serialized
		{
			writer.write(value.toString()); //just write a string representation of the value
		}
		writer.write(CRLF); //write the CR+LF that ends the content line
	}

	/**
	 * Serializes a directory content line's parameters. If multiple parameters exist with identical names, compared without regard to case, the values will be
	 * combined into multiple values for a single parameter. The parameter delimiters will be written, including the beginning parameters separator that appears
	 * before any parameters.
	 * @param paramList The list of parameters.
	 * @param writer The writer to which the lines of the directory should be serialized.
	 * @exception IOException Thrown if there is an error writing to the directory.
	 */
	protected void serializeParameters(final List<NameValuePair<String, String>> paramList, final Writer writer) throws IOException
	{
		final List<NameValuePair<String, String>> paramRemainingList = new ArrayList<NameValuePair<String, String>>(paramList); //create a list of remaining parameters
		while(paramRemainingList.size() > 0) //while there are parameters remaining
		{
			//get all parameter values for parameters that have the same name as the first parameter
			final String firstParamName = paramRemainingList.get(0).getName(); //get the name of the first parameter, and see if there are other parameters with the same name
			final List<String> valueList = new ArrayList<String>(); //create a list in which to place all param values of params that have the same name as the first remaining param
			final Iterator<NameValuePair<String, String>> paramIterator = paramRemainingList.iterator(); //get an iterator to the parameters
			while(paramIterator.hasNext()) //while there are remaining parameters
			{
				final NameValuePair<String, String> param = paramIterator.next(); //get the first parameter (there will always be at least one, because we checked the size first)
				if(param.getName().equalsIgnoreCase(firstParamName)) //if this parameter has the same name as the first parameter
				{
					valueList.add(param.getValue()); //add the parameter value to the list
					paramIterator.remove(); //remove this parameter from the remaining parameter list
				}
			}
			//write the parameter name and all the parameter values
			writer.write(PARAM_SEPARATOR_CHAR); //write the parameter separator ';'
			writer.write(firstParamName); //write the parameter name
			writer.write(PARAM_NAME_VALUE_SEPARATOR_CHAR); //write the parameter name-value separator '='
			final Iterator<String> valueIterator = valueList.iterator(); //get an iterator to the values
			while(valueIterator.hasNext()) //while there are more values
			{
				writer.write(valueIterator.next()); //write the next value
				if(valueIterator.hasNext()) //if there is another value
				{
					writer.write(PARAM_VALUE_SEPARATOR_CHAR); //write the separator between param values ','
				}
			}
		}
	}

}
