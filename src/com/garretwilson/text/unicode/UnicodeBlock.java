package com.garretwilson.text.unicode;

import com.garretwilson.lang.IntegerUtilities;

/**Describes a named range of Unicode characters.
@author Garret Wilson
*/
public class UnicodeBlock implements Comparable
{
	/**The name of the Unicode block.*/
	private String name;

		/**@return The name of the Unicode block.*/
		private String getName() {return name;}

	/**The start code of the Unicode block.*/
	private char startCode;

		/**@return The start code of the Unicode block.*/
		public char getStartCode() {return startCode;}

	/**The inclusive end code of the Unicode block.*/
	private char endCode;

		/**@return The inclusive end code of the Unicode block.*/
		public char getEndCode() {return endCode;}

	/**Constructs a named Unicode block with a given name.
	@param newName The name of the Unicode block.
	@param newStartCode The start code.
	@param newEndCode The inclusive end code.
	*/
	public UnicodeBlock(final String newName, final char newStartCode, final char newEndCode)
	{
		name=newName; //set the name
		startCode=newStartCode; //set the start code
		endCode=newEndCode; //set the ending code
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
	public int compareTo(Object object) throws ClassCastException
	{
		return getStartCode()-((UnicodeBlock)object).getStartCode()!=0 ? //subtract the start codes
			getStartCode()-((UnicodeBlock)object).getStartCode() : //if the start codes aren't the same, we don't need to compare further
		  getEndCode()-((UnicodeBlock)object).getEndCode();  //if the start codes are the same, see where they end
	}


	/**@return A string representation of this block in the format "name [0000-0000]".*/
	public String toString()
	{
		return getName()+" ["+  //G***use UnicodeUtilities or something here instead of UnicodeData
			  IntegerUtilities.toHexString(getStartCode(), 4).toUpperCase()+"-"+
			  IntegerUtilities.toHexString(getEndCode(), 4).toUpperCase()+"]";
	}
}