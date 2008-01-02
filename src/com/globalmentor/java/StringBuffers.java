/* Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>
 * All Rights Reserved.
 * 
 * Use is subject to the BSD-style license at
 * <https://svn.globalmentor.com/java/src/com/globalmentor/license.txt>.
 */

package com.globalmentor.java;

/**Various methods that manipulate {@link StringBuffer} objects. These
	methods are fast relative to their {@link Strings}
	counterparts, because the {@link StringBuffer} objects on which they
	operate can be modified within the object instead of forcing a new object to
	be created. These methods furthermore modify the specified
	{@link StringBuffer} object rather than returning a new object.
@author Garret Wilson
@see Strings
*/
public class StringBuffers
{

	/**Concatenates the string representations of the objects
		in the array by appending them to the string buffer.
	@param stringBuffer The string buffer into which the result should be placed.
	@param objects The array of objects (such as strings) to be concatenated.
	@return A concatenation of string representations of all objects in the
		array.
	@see Object#toString
	*/
	public static StringBuffer append(final StringBuffer stringBuffer, final Object[] objects)
	{
		return append(stringBuffer, objects, null);	//append the objects with no separator
	}

	/**Concatenates the string representations of the objects
		in the array, separated by the given separator character, by appending
		them to the string buffer.
	@param stringBuffer The string buffer into which the result should be placed.
	@param objects The array of objects (such as strings) to be concatenated.
	@param separator The separator character to be inserted between the object
		strings. 
	@return A concatenation of string representations of all objects in the
		array, separted by the separator character.
	@see Object#toString
	*/
	public static StringBuffer append(final StringBuffer stringBuffer, final Object[] objects, final char separator)
	{
		return append(stringBuffer, objects, String.valueOf(separator));	//convert the separator to a string and append the objects
	}
	
	/**Concatenates the string representations of the objects
		in the array, separated by the given separator string, by appending
		them to the string buffer.
	@param stringBuffer The string buffer into which the result should be placed.
	@param objects The array of objects (such as strings) to be concatenated.
	@param separator The separator string to be inserted between the object
		strings, or <code>null</code> (if no separator should be used. 
	@return A concatenation of string representations of all objects in the
		array, separted by the separator.
	@see Object#toString
	*/
	public static StringBuffer append(final StringBuffer stringBuffer, final Object[] objects, final String separator)
	{
		for(int i=0; i<objects.length; ++i)	//look at each object
		{
			stringBuffer.append(objects[i].toString());	//add the string representation of this object to the string buffer
			if(i<objects.length-1 && separator!=null)	//if this isn't the last object, and there is a separator string
				stringBuffer.append(separator);	//append the separator character 			
		}
		return stringBuffer;	//return the string buffer, now containing the new information
	}	

	/**Collapses every run of any number of collapseChars to a single replaceString.
	@param stringBuffer The buffer in which the information will be collapsed.
	@param collapseChars The characters to be removed from the string.
	@param replaceString The string which will replace the collapseChars.
	*/
	public static void collapse(final StringBuffer stringBuffer, final String collapseChars, final String replaceString)
	{
//TODO del Debug.trace("ready to collapse: ", stringBuffer); //TODO del
//TODO shouldn't we trim the string, first? no, probably not
		final int replaceLength=replaceString.length(); //find the length of the replacement string
		int nextIndex=0;	//start at the beginning of the string
		while(nextIndex<stringBuffer.length())	//keep going until we reach the end of the string buffer
		{
//TODO del System.out.println(outString.length());	//TODO del
//TODO del Debug.trace("checking index: ", nextIndex); //TODO del
//TODO del Debug.trace("checking character: "+(int)stringBuffer.charAt(nextIndex)+" "+stringBuffer.charAt(nextIndex)); //TODO del
			if(collapseChars.indexOf(stringBuffer.charAt(nextIndex))>=0)	//if this character is one of our remove characters
			{
//TODO del Debug.trace("found collapse character at index: ", nextIndex);  //TODO del
				int removeEnd=nextIndex+1;	//start looking at the next character to see how long this run is
				while(removeEnd<stringBuffer.length() && collapseChars.indexOf(stringBuffer.charAt(removeEnd))>=0)	//while we still have more characters, and the characters we're finding are characters to remove
					++removeEnd;
//TODO del				final int removeLength=removeEnd-nextIndex;	//find out how many characters to remove
//TODO del System.out.println("Ready to remove stuff from: "+StringManipulator.replace(outString, '\n', "\\n"));	//TODO del
//TODO del				replace(stringBuffer, nextIndex, removeLength, replaceString);	//replace our characters with the given string
//TODO del Debug.trace("after collapse: ", stringBuffer); //TODO del
				stringBuffer.replace(nextIndex, removeEnd, replaceString);	//replace our characters with the given string
				nextIndex+=replaceLength; //move to the position after the replacement string
//TODO del System.out.println("New out string: "+StringManipulator.replace(outString, '\n', "\\n"));	//TODO del
			}
			else  //if this is not a character to replace
				++nextIndex;	//look at the next character in the string
		}
	}
	
	/**Returns the index of the first ocurrence of the given character in the
		string buffer.
	@param stringBuffer The string buffer to search.
	@param c The character to look for.
	@return The index in the string buffer of the given character, or -1 if no
		occurrence was found.
	*/
	public static int indexOf(final StringBuffer stringBuffer, final char c)
	{
		return indexOf(stringBuffer, c, 0); //start searching from the beginning
	}

	/**Returns the index of the first ocurrence of the given character in the
		string buffer from the given index.
	@param stringBuffer The string buffer to search.
	@param c The character to look for.
	@param fromIndex The index at which to start the search.
	@return The index in the string buffer of the given character, or -1 if no
		occurrence was found.
	*/
	public static int indexOf(final StringBuffer stringBuffer, final char c, final int fromIndex)
	{
		final int length=stringBuffer.length(); //see how many characters are in the string buffer
		for(int i=fromIndex; i<length; ++i) //look at each character in the string buffer
		{
		  if(stringBuffer.charAt(i)==c) //if this character matches the supplied character
				return i; //return the index at which the character occurs
		}
		return -1;  //show that the character was not found
	}

	/**Searches a string buffer and returns the first index of any character
		<em>not</em> in the specified string, starting from the beginning.
	@param stringBuffer The string buffer to be searched.
	@param notCharString The string of characters to check.
	@return The index of the first occurrence of one of the supplied characters,
		or -1 if none were found.
	*/
	public static int notCharIndexOf(final StringBuffer stringBuffer, final String notCharString)
	{
		return notCharIndexOf(stringBuffer, notCharString, 0);  //start looking from the beginning
	}

	/**Searches a string buffer and returns the first index of any character
		<em>not</em> in the specified string, starting at <code>fromIndex</code>.
	@param stringBuffer The string buffer to be searched.
	@param notCharString The string of characters to check.
	@param fromIndex The index to search from
	@return The index of the first occurrence of one of the supplied characters,
		or -1 if none were found.
	*/
	public static int notCharIndexOf(final StringBuffer stringBuffer, final String notCharString, final int fromIndex)
	{
		for(int i=fromIndex; i<stringBuffer.length(); ++i)	//look at each character in the string
		{
			if(notCharString.indexOf(stringBuffer.charAt(i))<0)	//if this character is not in our character string
				return i;	//return the index we're at
		}
		return -1;	//if we make it to here, we didn't find any characters which weren't in our character string
	}

	/**Removes all content after and including the first occurrence of a character
		appearing in the delimiter string.
	@param stringBuffer The characters to examine.
	@param delimiters A string of characters to search for.
	@return The string buffer after removal.
	*/
	public static StringBuffer removeFirstCharLength(final StringBuffer stringBuffer, final String delimiters)
	{
		final int index=CharSequences.charIndexOf(stringBuffer, delimiters);  //find the first occurence of the delimiters
		if(index>=0)  //if one of the delimiters was found
			stringBuffer.delete(index, stringBuffer.length());  //remove everything after and including the character
		return stringBuffer;  //return the string buffer
	}


	/**Removes every occurrence of a specified character.
	@param stringBuffer The string buffer from which the information will be removed.
	@param removeChar The character to remove from the string buffer.
	@return The string buffer after removal.
	*/
	public static StringBuffer removeEvery(final StringBuffer stringBuffer, final char removeChar)
	{
		for(int i=stringBuffer.length()-1; i>=0; --i)	//look at each character, from the last to the first
		{
			if(stringBuffer.charAt(i)==removeChar)	//if we have found a character to remove
			{
				stringBuffer.deleteCharAt(i);	//delete this character
			}
		}
		return stringBuffer;  //return the string buffer
	}

	/**Removes every occurrence of any of the given specified characters.
	@param stringBuffer The string buffer from which the information will be removed.
	@param removeChars The characters to be removed from the string buffer.
	@return The string buffer after removal.
	*/
	public static StringBuffer removeEveryChar(final StringBuffer stringBuffer, final String removeChars)
	{
		for(int i=stringBuffer.length()-1; i>=0; --i)	//look at each character, from the last to the first
		{
			if(removeChars.indexOf(stringBuffer.charAt(i))>=0)	//if this character is one of our remove characters
			{
				stringBuffer.deleteCharAt(i);	//delete this character
			}
		}
		return stringBuffer;  //return the string buffer
	}

	/**Converts all characters in the string buffer to corresponding characters in
		a given lookup table. Any characters not in the table will remain unchanged.
	@param stringBuffer The string buffer in which characters should be replaced.
	@param conversionTable A map of replacement characters each stored at the index
		of the matching chararacter (e.g. character code <em>x</em> will be replaced
		by <code>characterTable[<em>x</em>]</code>).
	@return The number of replacements made.
	*/
	public static int replace(final StringBuffer stringBuffer, final char[] conversionTable)
	{
		int replacementCount=0; //show that we have not yet made any replacements
		final int conversionTableLength=conversionTable.length; //find out how many characters we recognize
//TODO del		final int stringBufferLength=stringBuffer.length(); //find out how many characters there are to convert
		for(int i=stringBuffer.length()-1; i>=0; --i) //look at each character in the string buffer
		{
			final char c=stringBuffer.charAt(i);  //get the code of the character candidate for replacement
			if(c<conversionTableLength) //if this character  has a replacement
			{
				stringBuffer.setCharAt(i, conversionTable[c]);  //replace the character with the one at the index in the lookup table of its character code
				++replacementCount; //show that we replaced another character
			}
		}
		return replacementCount;  //show how many characters we replaced
	}

	/**Replaces each matching character with a given replacement.
	@param stringBuffer The buffer in which the replacements will be made.
	@param matchReplaceSetArray An array of two-character arrays, each of the
		latter representing a set of characters, the first being a match character
		and the second a replacement character.
	@return The number of replacements made.
	*/
	public static int replace(final StringBuffer stringBuffer, final char[][] matchReplaceSetArray)
	{
		int replacementCount=0; //show that we have not replaced any characters, yet
		for(int matchSetIndex=0; matchSetIndex<matchReplaceSetArray.length; ++matchSetIndex)  //look at every set of match/replace pairs
		{
			final char matchChar=matchReplaceSetArray[matchSetIndex][0]; //get the match chararacter
			final char replacementChar=matchReplaceSetArray[matchSetIndex][1]; //get the replacement chararacter
		  replacementCount+=replace(stringBuffer, matchChar, replacementChar);  //replace every occurrence of this character in the original string, and update our record of how many replacements were made
		}
		return replacementCount;  //show how many characters we replaced
	}

	/**Replaces each matching character with the given replacement character.
	@param stringBuffer The buffer in which the replacements will be made.
	@param matchChar The character to be replaced.
	@param replacementChar The character for replacing the match character.
	@return The number of replacements made.
	*/
	public static int replace(final StringBuffer stringBuffer, final char matchChar, final char replacementChar)
	{
		int replacementCount=0; //show that we have not replaced any characters, yet
		final int length=stringBuffer.length(); //get the length of the string buffer
		for(int i=0; i<length; ++i) //look at each character in the string buffer
		{
			if(matchChar==stringBuffer.charAt(i))  //if this character matches
			{
				stringBuffer.setCharAt(i, replacementChar); //replace the original character with its replacement
				++replacementCount; //show that we replaced a character
			}
		}
		return replacementCount;  //show how many characters we replaced
	}

	/**Replaces each matching character with the given replacement string.
	@param stringBuffer The buffer in which the replacements will be made.
	@param matchChar The character to be replaced.
	@param replacementString The string for replacing the match character.
	@return The number of replacements made.
	*/
	public static int replace(final StringBuffer stringBuffer, final char matchChar, final String replacementString)
	{
		int replacementCount=0; //show that we have not replaced any characters, yet
		int beginSearchIndex=0;	//we'll start searching from the beginning of the string
		int nextReplaceIndex;	//this will hold the next location of the replaceString each time we search for it
		while(beginSearchIndex<stringBuffer.length())	//while we haven't examined all the characters
		{
			nextReplaceIndex=indexOf(stringBuffer, matchChar, beginSearchIndex);	//search for another occurrence of the character
			if(nextReplaceIndex>=0)	//if there is another occurrence of the character to replace
			{
				stringBuffer.replace(nextReplaceIndex, nextReplaceIndex+1, replacementString);	//replace this character with the replacement string
				beginSearchIndex=nextReplaceIndex+replacementString.length();	//skip over the string we replaced in the input string
				++replacementCount; //show that we replaced a character
			}
			else	//if there are no more occurrences of the string to replace
			{
				break;	//stop searching for more matches
			}
		}
		return replacementCount;  //show how many characters we replaced
	}

	/**Replaces each matching character with the given replacement.
	@param stringBuffer The buffer in which the replacements will be made.
	@param matchChars The characters, each of which will to be replaced.
	@param replacementChar The character for replacing the match characters.
	@return The number of replacements made.
	*/
	public static int replace(final StringBuffer stringBuffer, final String matchChars, final char replacementChar)
	{
		int replacementCount=0; //show that we have not replaced any characters, yet
		final int length=stringBuffer.length(); //get the length of the string buffer
		for(int i=0; i<length; ++i) //look at each character in the string buffer
		{
			if(matchChars.indexOf(stringBuffer.charAt(i))>=0)  //if this character matches one of the match characters
			{
				stringBuffer.setCharAt(i, replacementChar); //replace the original character with its replacement
				++replacementCount; //show that we replaced a character
			}
		}
		return replacementCount;  //show how many characters we replaced
	}

	/**Replaces each matching character with the corresponding replacement string.
	@param stringBuffer The buffer in which the replacements will be made.
	@param matchChars An array of characters to be replaced.
	@param replacementStrings An array of strings to replace the characters
		appearing at the same indexes as those in <code>matchChars</code>.
	@return The number of replacements made.
	*/
	public static int replace(final StringBuffer stringBuffer, final char[] matchChars, final String[] replacementStrings)
	{
		int replacementCount=0; //show that we have not replaced any characters, yet
		for(int charIndex=0; charIndex<stringBuffer.length(); ++charIndex) //look at each character in the string buffer
		{
			final char c=stringBuffer.charAt(charIndex);  //get this character
			for(int matchIndex=matchChars.length-1; matchIndex>=0; --matchIndex)  //look at each of the characters to match
			{
				if(c==matchChars[matchIndex]) //if the character matches this match character
				{
						//replace this character with the replacement string
					stringBuffer.replace(charIndex, charIndex+1, replacementStrings[matchIndex]);
					charIndex+=replacementStrings[matchIndex].length()-1; //skip to the last character of the replaced string
					++replacementCount; //show that we replaced a character
					break;	//stop looking for matches for this character, and go on to the next character
				}
			}
		}
		return replacementCount;  //show how many characters we replaced
	}

	/**Replaces every occurrence of a specified string with another string.
	@param stringBuffer The string buffer in which the information will be replaced.
	@param replaceString The string to replace.
	@param withString The string that will replace replaceString.
	@return The number of replacements made.
	*/
	static public int replace(final StringBuffer stringBuffer, final String replaceString, final String withString)
	{
		int replacementCount=0; //show that we have not replaced any characters, yet
		int beginSearchIndex=0;	//we'll start searching from the beginning of the string
		int nextReplaceIndex;	//this will hold the next location of the replaceString each time we search for it
		while(beginSearchIndex<stringBuffer.length())	//while we haven't examined all the characters
		{
			nextReplaceIndex=stringBuffer.indexOf(replaceString, beginSearchIndex);	//search for another occurrence of the string
			if(nextReplaceIndex>=0)	//if there is another occurrence of the string to replace
			{
				stringBuffer.replace(nextReplaceIndex, nextReplaceIndex+replaceString.length(), withString);	//replace this string with the replacement string
				beginSearchIndex=nextReplaceIndex+replaceString.length();	//skip over the string we replaced in the input string
				++replacementCount; //show that we replaced a character
			}
			else	//if there are no more occurrences of the string to replace
			{
				break;	//stop searching for more matches
			}
		}
		return replacementCount;  //show how many characters we replaced
	}

	/**Replaces all runs of the given character with another character.
		A run is subsequent repeated characters.
		If a run is shorter than <code>minRunLength</code> or longer than
		<code>maxRunLength</code>, it will be ignored.
	@param stringBuffer The string buffer to search.
	@param matchChar The char runs of which to replace.
	@param minRunLength The minimum number of subsequent characters before
		replacement.
	@param maxRunLength The maximum number of subsequent characters before
		replacement.
	@param replaceChar The character that will replace all matching runs.
	@return The string buffer with all matching character runs replaced.
	*/
	public static StringBuffer replaceRuns(final StringBuffer stringBuffer, final char matchChar, final int minRunLength, final int maxRunLength, final char replaceChar)
	{
		int nextIndex=0;	//start at the beginning of the string
		while(nextIndex<stringBuffer.length())	//keep going until we reach the end of the string buffer
		{
			nextIndex=indexOf(stringBuffer, matchChar, nextIndex); //find the next character match
			if(nextIndex>=0)  //if we found the character
			{
				int runEndIndex=nextIndex+1;  //we'll use this variable to find the end of the run
					//advance the runEndIndex until we run out of string buffer or find a non-matching character
				while(runEndIndex<stringBuffer.length() && stringBuffer.charAt(runEndIndex)==matchChar)
					++runEndIndex;  //keep looking for a non-matching character
				final int runLength=runEndIndex-nextIndex;  //find the length of the run
				if(runLength>=minRunLength && runLength<=maxRunLength)  //if this run size is in our range
				{
					replace(stringBuffer, nextIndex, runEndIndex, replaceChar);	//replace the run with the given character
					++nextIndex;  //skip the character that replaced the run
				}
				else  //if the run size doesn't meet our criteria
				{
					nextIndex=runEndIndex;  //skip the run
				}
			}
			else  //if the character doesn't appear any more in the string
				break;  //stop replacing
		}
		return stringBuffer;  //return the string buffer
	}

	/**Removes several characters at the specified index and replaces them with
		the given character. This duplicates <code>StringBuffer.replace</code>
		functionality without the overhead of a string.
	@param stringBuffer The buffer in which the information will be replaced.
	@param startIndex The index of the information to remove.
	@param endIndex One character past the last of the information to remove; must
		be greater than <code>startIndex</code>.
	@param replaceChar The character to replace the removed characters.
	@return The string buffer with the specified contents replaced.
	@see StringBuffer#replace
	*/
	public static StringBuffer replace(final StringBuffer stringBuffer, final int startIndex, final int endIndex, final char replaceChar)
	{
		stringBuffer.setCharAt(startIndex, replaceChar);  //replace the character at the starting index
		if(endIndex-startIndex>1) //if we're replacing more than one character
			stringBuffer.delete(startIndex+1, endIndex);  //delete evertything after the starting character up to the end of our replacement section
		return stringBuffer;  //return the string buffer
	}

	/**Determines if the string buffer starts with the given string.
	@param stringBuffer The string buffer to examine.
	@param string The string to compare.
	@return <code>true</code> if the first characters of the string buffer match
		those of the given string.
	*/
	public static boolean startsWith(final StringBuffer stringBuffer, final String string)
	{
		if(string.length()>stringBuffer.length())  //if the substring is too long
			return false; //the substring is too big to start the string buffer
		for(int i=string.length()-1; i>=0; --i) //look at each character of the string
		{
			if(string.charAt(i)!=stringBuffer.charAt(i))  //if these characters don't match in the same position
				return false; //the string doens't match
		}
		return true;  //the string buffer starts with the string
	}

	/**Trims the specified delimiters from the beginning and end of the string buffer.
	@param stringBuffer The characters to be processed.
	@param delimiters The string containing delimiter characters.
	@return The trimmed string buffer.
	*/
	public static StringBuffer trim(final StringBuffer stringBuffer, final String delimiters)
	{
//TODO del System.out.println("trimming: "+stringBuffer);  //TODO del
		int beginIndex, endIndex;
		final int length=stringBuffer.length(); //get the length of the original string
		for(beginIndex=0; beginIndex<length && delimiters.indexOf(stringBuffer.charAt(beginIndex))>=0; ++beginIndex);	//find the first non-delimiter in the string buffer
		for(endIndex=length; endIndex>beginIndex && delimiters.indexOf(stringBuffer.charAt(endIndex-1))>=0; --endIndex);	//find the last non-delimiter in the string buffer
		if(endIndex<length)  //if we should trim the end
		  stringBuffer.delete(endIndex, length);  //remove the end of the buffer
//TODO del System.out.println("begin index: "+beginIndex); //TODO del
		if(beginIndex>0)  //if we should trim the beginning
		  stringBuffer.delete(0, beginIndex); //remove the beginning of the buffer
		return stringBuffer;  //return the resulting string buffer
	}

	/**Trims the specified delimiters from the end of the string buffer.
	@param stringBuffer The characters to be processed.
	@param delimiters The string containing delimiter characters.
	@return The trimmed string buffer.
	*/
	public static StringBuffer trimEnd(final StringBuffer stringBuffer, final String delimiters)  //TODO later call this method from trim()
	{
//TODO del System.out.println("trimming: "+stringBuffer);  //TODO del
		int endIndex;
		final int length=stringBuffer.length(); //get the length of the original string
		for(endIndex=length; endIndex>0 && delimiters.indexOf(stringBuffer.charAt(endIndex-1))>=0; --endIndex);	//find the last non-delimiter in the string buffer
		if(endIndex<length)  //if we should trim the end
		  stringBuffer.delete(endIndex, length);  //remove the end of the buffer
		return stringBuffer;  //return the resulting string buffer
	}

}