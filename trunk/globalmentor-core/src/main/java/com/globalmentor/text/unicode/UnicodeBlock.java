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

package com.globalmentor.text.unicode;

/**Describes a named range of Unicode characters.
@author Garret Wilson
*/
public class UnicodeBlock implements Comparable<UnicodeBlock>
{
	/**The name of the Unicode block.*/
	private final String name;

		/**@return The name of the Unicode block.*/
		public String getName() {return name;}

	/**The start code of the Unicode block.*/
	private final int startCode;

		/**@return The start code of the Unicode block.*/
		public int getStartCode() {return startCode;}

	/**The inclusive end code of the Unicode block.*/
	private final int endCode;

		/**@return The inclusive end code of the Unicode block.*/
		public int getEndCode() {return endCode;}

	/**Constructs a named Unicode block with a given name.
	@param newName The name of the Unicode block.
	@param newStartCode The start code.
	@param newEndCode The inclusive end code.
	*/
	public UnicodeBlock(final String newName, final int newStartCode, final int newEndCode)
	{
		name=newName; //set the name
		startCode=newStartCode; //set the start code
		endCode=newEndCode; //set the ending code
	}

	/**Determines if the given Unicode code point falls within the range of this Unicode block.
	@param codePoint The Unicode code point to check.
	@return <code>true</code> if the given code point falls within the range (<var>startCode</var>, <var>endCode</var>), inclusive. 
	*/
	public boolean contains(final int codePoint)
	{
		return codePoint>=getStartCode() && codePoint<=getEndCode();	//determine whether the code point falls within this block's range
	}
	
	/**If <code>object</code> is a <code>UnicodeBlock</code>, compares the start
		and end codes. Otherwise, compares the objects using the superclass functionality.
	@param object The object with which to compare this Unicode block; should be a
		<code>UnicodeBlock</code>
	@return <code>true<code> if this Unicode block equals that specified in
		<code>object</code>.
	@see #getStartCode
	@see #getEndCode
	*/
	public boolean equals(Object object)
	{
		if(object instanceof UnicodeBlock)	//if we're being compared with another Unicode block
		{
			return getStartCode()==((UnicodeBlock)object).getStartCode()
				  && getEndCode()==((UnicodeBlock)object).getEndCode(); //compare the starting and ending codes
		}
		else	//if we're being compared with anything else
			return super.equals(object);	//use the default compare
	}

	/**Compares this Unicode block to another Unicode block.
		This method determines order based upon the start of each character block.
	@param object The object with which to compare the component. This must be
		another <code>UnicodeCharacter</code> object.
	@return A negative integer, zero, or a positive integer as this block is
		less than, equal to, or greater than the specified block, respectively.
		If the blocks start at the same code, their ending codes are compared.
	@exception ClassCastException Thrown if the specified object's type is not
		an <code>UnicodeBlock</code>.
	@see #getStartCode
	*/
	public int compareTo(final UnicodeBlock object) throws ClassCastException
	{
		final int value=getStartCode()-object.getStartCode();	//compare the start codes
		return value!=0 ? value //return the difference in start codes if they aren't equal
				: getEndCode()-object.getEndCode();  //if the start codes are the same, see where they end
	}

	/**@return A string representation of this block in the format "name [0000-0000]".*/
	public String toString()
	{
		return getName()+" ["+UnicodeCharacter.getCodePointString(getStartCode())+'-'+UnicodeCharacter.getCodePointString(getEndCode())+']';
	}
}