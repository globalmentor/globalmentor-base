package com.garretwilson.text.directory;

import java.util.*;
import com.garretwilson.lang.*;
import com.garretwilson.util.*;

/**A directory of type <code>text/directory</code> as defined in
	"RFC 2425: A MIME Content-Type for Directory Information".
@author Garret Wilson
*/
public class Directory
{
	
	/**The constant representing all profiles.*/
//G***del	public final static String ALL_PROFILES="$ALL_PROFILES";
	/**The constant representing all types.*/
//G***del	public final static String ALL_TYPES="$ALL_TYPES";

	/**The display name of the directory.*/
	private LocaleText displayName=null;

		/**@return The display name of the directory.*/
		public LocaleText getDisplayName() {return displayName;}

		/**Sets the display name of the directory.
		@param displayName The new display name of the directory.
		*/
		public void setDisplayName(final LocaleText displayName) {this.displayName=displayName;}

	/**The list of content lines in the directory.*/
//G***del	private final List contentLineList=new ArrayList();
	
		/**@return The list of content lines in the directory.*/
//G***del		public List getContentLineList() {return contentLineList;}

		/**@return An iterator to the list of all content lines in the directory.*/
//G***del		public Iterator getContentLineIterator() {return contentLineList.iterator();}

	/**The list of content lines that represent unrecognized and/or unprocessed information.*/
	private final List contentLineList=new ArrayList();

		/**@return The list of content lines that represent unrecognized and/or unprocessed information.*/
		public List getContentLineList() {return contentLineList;}

	/**Default constructor.*/
	public Directory()
	{
//G***del		super(null);	//construct a directory with no name
	}

	/**Gets all content lines within the given profile, ignoring case when making
		all matches.
	@param profile The profile of the lines to return, <code>null</code> for no
		profile (the predefined types), or <code>ALL_PROFILES</code>
		if all profiles should be included.
	@return An array of content lines that meet the given criteria.
	@see #ALL_PROFILES
	*/
/*G***del
	public ContentLine[] getContentLinesByProfile(final String profile)
	{
		return getContentLines(profile, ALL_TYPES);	//return content lines from the specified profile		
	}
*/

	/**Gets the value of the first content line with the specified type name
		within any profile, ignoring case when making all matches.
	@param typeName The type name of the line to return, or
		<code>ALL_TYPES</code> if all relevant content lines should be included.
	@return The value of the first content line meeting the given criteria, or
		<code>null</code> if no content lines met the given criteria.
	@see #ALL_TYPES
	*/
/*G***del
	public Object getContentLineValueByType(final String typeName)
	{
		final ContentLine contentLine=getContentLineByType(typeName);	//get the value of the first matchine content line
		return contentLine!=null ? contentLine.getValue() : null;	//if we found a content line, return its value
	}
*/
	/**Gets the first content line with the specified type name within any
		profile, ignoring case when making all matches.
	@param typeName The type name of the line to return, or
		<code>ALL_TYPES</code> if all relevant content lines should be included.
	@return The first content line meeting the given criteria, or
		<code>null</code> if no content lines met the given criteria.
	@see #ALL_TYPES
	*/
	/*G***del
	public ContentLine getContentLineByType(final String typeName)
	{
		return getContentLine(ALL_PROFILES, typeName);	//return the first content line with the specified type from any profiles
	}
*/
	/**Gets all content lines with the specified type name within any profile,
		ignoring case when making all matches.
	@param typeName The type name of the lines to return, or
		<code>ALL_TYPES</code> if all relevant content lines should be returned.
	@return An array of content lines that meet the given criteria.
	@see #ALL_TYPES
	*/
	/*G***del
	public ContentLine[] getContentLinesByType(final String typeName)
	{
		return getContentLines(ALL_PROFILES, typeName);	//return content lines with the specified type from all profiles
	}
*/
	/**Gets the value of the first content line with the specified type name
		within the given profile, ignoring case when making all matches.
	@param profile The profile of the lines to return, <code>null</code> for no
		profile (the predefined types), or <code>ALL_PROFILES</code>
		if all profiles should be included.
	@param typeName The type name of the lines to return, or
		<code>ALL_TYPES</code> if all relevant content lines should be returned.
	@return The value of the first content line meeting the given criteria, or
		<code>null</code> if no content lines met the given criteria.
	@see #ALL_PROFILES
	@see #ALL_TYPES
	*/
	/*G***del
	public Object getContentLineValue(final String profile, final String typeName)
	{
		final ContentLine contentLine=getContentLine(profile, typeName);	//get the value of the requested content line
		return contentLine!=null ? contentLine.getValue() : null;	//if we found a content line, return its value
	}
*/
	/**Gets the first content line with the specified type name within the given
		profile, ignoring case when making all matches.
	@param profile The profile of the lines to return, <code>null</code> for no
		profile (the predefined types), or <code>ALL_PROFILES</code>
		if all profiles should be included.
	@param typeName The type name of the lines to return, or
		<code>ALL_TYPES</code> if all relevant content lines should be returned.
	@return The first content line meeting the given criteria, or
		<code>null</code> if no content lines met the given criteria.
	@see #ALL_PROFILES
	@see #ALL_TYPES
	*/
	/*G***del
	public ContentLine getContentLine(final String profile, final String typeName)
	{
		final ContentLine[] contentLines=getContentLines(profile, typeName);	//get all relevant content lines
		return contentLines.length>0 ? contentLines[0] : null;	//return the first content line, or null if there were no content lines
	}
*/	
	/**Gets all content lines with the specified type name within the given
		profile, ignoring case when making all matches.
	@param profile The profile of the lines to return, <code>null</code> for no
		profile (the predefined types), or <code>ALL_PROFILES</code>
		if all profiles should be included.
	@param typeName The type name of the lines to return, or
		<code>ALL_TYPES</code> if all relevant content lines should be returned.
	@return An array of content lines that meet the given criteria.
	@see #ALL_PROFILES
	@see #ALL_TYPES
	*/
	/*G***del
	public ContentLine[] getContentLines(final String profile, final String typeName)
	{
		final boolean acceptAllProfiles=ALL_PROFILES.equals(profile);	//see if we should accept all profiles
		final boolean acceptAllTypes=ALL_TYPES.equals(typeName);	//see if we should accept all types
		final List matchingContentLineList=new ArrayList();	//create a list in which to place our matching content lines
		final Iterator contentLineIterator=getContentLineList().iterator();	//get an iterator to the content lines
		while(contentLineIterator.hasNext())	//while there are more content lines
		{
			final ContentLine contentLine=(ContentLine)contentLineIterator.next();	//get the next content line
			if((acceptAllProfiles || StringUtilities.equalsIgnoreCase(profile, contentLine.getProfile()))	//if the profile matches, or if we should get all profiles
					&& (acceptAllTypes || StringUtilities.equalsIgnoreCase(typeName, contentLine.getTypeName())))	//if the type name matches, or if we should get all type names
			{
				matchingContentLineList.add(contentLine);	//add this content line to our list of matching content lines
			}
		}
		return (ContentLine[])matchingContentLineList.toArray(new ContentLine[matchingContentLineList.size()]);	//return an array of the content lines we found
	}
*/
	/**@return A string representation of the directory.*/
	/*G***del
	public String toString()
	{
		final StringBuffer stringBuffer=new StringBuffer("text/directory");	//create a new string buffer in which to construct the string
		if(getName()!=null)	//if there is a name
		{
			stringBuffer.append('[').append(getName()).append(']');	//append the name
		}
		stringBuffer.append(':').append(' ').append(getContentLineList().size()).append(" content line(s)\n");
		final Iterator contentLineIterator=getContentLineList().iterator();	//get an iterator to the content lines
		while(contentLineIterator.hasNext())	//while there are more content lines
		{
			final ContentLine contentLine=(ContentLine)contentLineIterator.next();	//get the next content line
			stringBuffer.append(contentLine.toString()).append('\n');	//append this content line
		}
		return stringBuffer.toString();	//return the string we constructed
	}
*/
}
