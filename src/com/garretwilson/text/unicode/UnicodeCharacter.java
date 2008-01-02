package com.garretwilson.text.unicode;

import com.globalmentor.java.IntegerUtilities;

import static com.garretwilson.text.unicode.UnicodeConstants.*;

/**Represents a character in the Unicode database.
Created for Unicode 3.0.0.
@author Garret Wilson
@version 1.0
*/
public class UnicodeCharacter implements Comparable<UnicodeCharacter>
{

	/**The code value of the Unicode character.*/
	private int codeValue;

		/**@returns The code value of the Unicode character.*/
		public int getCodeValue() {return codeValue;}

		void setCodeValue(final int newCodeValue) {codeValue=newCodeValue;}	//TODO del and change the parsing routine to collect all data before construction

	/**The name of the character.*/
	private String characterName;

		/**@return The name of the character.*/
		public String getCharacterName() {return characterName;}

		/**Sets the name of the character.
		@param newCharacterName The name of the character.
		*/
		public void setCharacterName(final String newCharacterName) {characterName=newCharacterName;}

		/**If this is a control character (&lt;control&gt;), returns the Unicode 1.0 name.
		@return The unique name of the character, or the empty string if there
			is no unique name.
		@see UnicodeConstants#CONTROL_NAME
		@see #getCharacterName
		@see #getUnicode10Name
		*/
		public String getUniqueCharacterName()
		{
			String uniqueName=getCharacterName();	//get the character name
			if(CONTROL_NAME.equalsIgnoreCase(uniqueName))	//if this is a control character
				uniqueName=getUnicode10Name();	//use the Unicode 1.0 name
			return uniqueName;	//return the name we found (which still could be "")
		}

		/**@return <code>true</code> if this is a control character (the name is "&lt;control&gt;").
		@see #getCharacterName()
		*/
		public boolean isControl()
		{
			return CONTROL_NAME.equalsIgnoreCase(getCharacterName());	//return whether the name is "<control>"
		}

	/**The character's general category.*/
	private String generalCategory;

		/**@return The character's general category.*/
		public String getGeneralCategory() {return generalCategory;}

		/**Sets the character's general category.
		@param newGeneralCategory The character's general category.
		*/
		public void setGeneralCategory(final String newGeneralCategory) {generalCategory=newGeneralCategory;}

	/**The combining class of the character.*/
	private int canonicalCombiningClass=-1;

		/**@return The combining class of the character, or <code>-1</code> if the
			combining class has not been assigned.*/
		public int getCanonicalCombiningClass() {return canonicalCombiningClass;}

		/**Sets the combining class of the character.
		@param newCanonicalCombiningClass The canonical combining class of the character.
		*/
		public void setCanonicalCombiningClass(final int newCanonicalCombiningClass) {canonicalCombiningClass=newCanonicalCombiningClass;}

	/**The bidirectional category.*/
	private String bidirectionalCategory=LEFT_TO_RIGHT;

		/**@return The bidirectional category.*/
		public String getBidirectionalCategory() {return bidirectionalCategory;}

		/**Sets the bidirectional category.
		@param newBidirectionalCategory The new bidirectional category.
		*/
		public void setBidirectionalCategory(final String newBidirectionalCategory) {bidirectionalCategory=newBidirectionalCategory;}

	/**The character decomposition formatting tag, if this is a compatibility mapping
		and not a canonical mapping.*/
	private String characterDecompositionTag="";

		/**@return A string with the character formatting tag, if this is a compatibility mapping
			and not a canonical mapping.*/
		public String getCharacterDecompositionTag() {return characterDecompositionTag;}

		/**Sets the character decomposition formatting tag.
		@param newCharacterDecompositionTag The character decomposition formatting tag.
		*/
		public void setCharacterDecompositionTag(final String newCharacterDecompositionTag) {characterDecompositionTag=newCharacterDecompositionTag;}

	/**The character decomposition mappings, if any.*/
	private String characterDecompositionMappings="";

		/**@return A string with the character decomposition mappings, if any.*/
		public String getCharacterDecompositionMappings() {return characterDecompositionMappings;}

		/**Sets the character decomposition mappings.
		@param newCharacterDecompositionMappings A string with the character decomposition
			mappings as characters in the correct order.
		*/
		public void setCharacterDecompositionMappings(final String newCharacterDecompositionMappings) {characterDecompositionMappings=newCharacterDecompositionMappings;}

	/**The decimal digit value, or <code>-1</code> if there is no decimal digit value.*/
	private int decimalDigitValue=-1;

		/**@return The decimal digit value, or <code>-1</code> if there is no decimal digit value.*/
		public int getDecimalDigitValue() {return decimalDigitValue;}

		/**Sets the decimal digit value.
		@param newDecimalDigitValue The decimal digit value.
		*/
		public void setDecimalDigitValue(final int newDecimalDigitValue) {decimalDigitValue=newDecimalDigitValue;}

	/**The digit value, or <code>-1</code> if there is no digit value.*/
	private int digitValue=-1;

		/**@return The digit value, or <code>-1</code> if there is no digit value.*/
		public int getDigitValue() {return digitValue;}

		/**Sets the digit value.
		@param newDigitValue The digit value.
		*/
		public void setDigitValue(final int newDigitValue) {digitValue=newDigitValue;}

	/**The numeric value numerator of the character, or <code>-1</code> if the character
		has no numeric value.*/
	private int numericValueNumerator=-1;

		/**@return The numeric value numerator of the character, or <code>int</code> if the
			character has no numeric value.*/
		public int getNumericValueNumerator() {return numericValueNumerator;}

		/**Sets the numeric value numerator of the character.
		@param newNumericValueNumerator The numeric value numerator.
		*/
		public void setNumericValueNumerator(final int newNumericValueNumerator) {numericValueNumerator=newNumericValueNumerator;}

	/**The numeric value denominator of the character, or <code>1</code> if the
		character is not a fraction or has no numeric value.*/
	private int numericValueDenominator=1;

		/**@return The numeric value denominator of the character, or <code>1</code> if the
			character is not a fraction or has no numeric value.*/
		public int getNumericValueDenominator() {return numericValueDenominator;}

		/**Sets the numeric value denominator of the character.
		@param newNumericValueDenominator The numeric value.
		*/
		public void setNumericValueDenominator(final int newNumericValueDenominator) {numericValueDenominator=newNumericValueDenominator;}

		/**@return Whether the numeric value is a fraction (that is, the denominator
			is not <code>1</code>.
		@see #getNumericValueNumerator
		@see #getNumericValueDenominator
		*/
		public boolean isNumericValueFraction() {return getNumericValueDenominator()!=1;}

		/**@return The numeric value numerator of the character, or <code>Float.NaN</code> if the
			character has no numeric value.*/
		public float getNumericValue()
		{
			return isNumericValueFraction()/*G***del if we don't need && getNumericValueNumerator()!=Float.NaN*/ ? getNumericValueNumerator() : getNumericValueNumerator()/getNumericValueDenominator();
		}

		/**Sets the numeric value of the character. For setting a fraction, use
			<code>setNumericValueNumerator</code> and <code>setNumericValueDenominator</code>.
		@param newNumericValue The numeric value of the character.
		@see #setNumericValueNumerator
		@see #setNumericValueDenominator
		*/
		public void setNumericValue(final int newNumericValue)
		{
			setNumericValueNumerator(newNumericValue);	//set the numerator
			setNumericValueDenominator(1);	//set the denominator to one, since this isn't a fraction
		}

	/**Whether this is a mirrored character in bidirectional text.*/
	private boolean mirrored=false;

		/**@return <code>true</code> if this is a mirrored character in bidirectional text.*/
		public boolean isMirrored() {return mirrored;}

		/**Sets whether this is a mirrored character in bidirectional text.
		@param newMirrored <code>true</code>if this is a mirrored character.
		*/
		public void setMirrored(final boolean newMirrored) {mirrored=newMirrored;}

	/**The Unicode 1.0 name of the character, if it is significantly different than
		the Unicode 3.0 name.*/
	private String unicode10Name="";

		/**@return The Unicode 1.0 name of the character, if it is significantly different than
			the Unicode 3.0 name.
		@see #getCharacterName
		*/
		public String getUnicode10Name() {return unicode10Name;}

		/**Sets the Unicode 1.0 name of the character
		@param newUnicode10Name The Unicode 1.0 name of the character.
		@see #setCharacterName
		*/
		public void setUnicode10Name(final String newUnicode10Name) {unicode10Name=newUnicode10Name;}

	/**The ISO 10646 comment, if present.*/
	private String iso10646Comment="";

		/**@return The ISO 10646 comment, if present.*/
		public String getISO10646Comment() {return iso10646Comment;}

		/**Sets the ISO 10646 comment.
		@param newISO10646Comment The ISO 10646 comment.*/
		public void setISO10646Comment(final String newISO10646Comment) {iso10646Comment=newISO10646Comment;}

	/**The uppercase mapping of this character, or <code>0</code> if this character
		has no uppercase mapping.*/
	private char uppercaseMapping=0;

		/**@return The uppercase mapping of this character, or <code>0</code> if
			this character has no uppercase mapping.*/
		public char getUppercaseMapping() {return uppercaseMapping;}

		/**Sets the uppercase mapping of this character.
		@param newUppercaseMapping The uppercase mapping of the character.
		*/
		public void setUppercaseMapping(final char newUppercaseMapping) {uppercaseMapping=newUppercaseMapping;}

	/**The lowercase mapping of this character, or <code>0</code> if this character
		has no lowercase mapping.*/
	private char lowercaseMapping=0;

		/**@return The lowercase mapping of this character, or <code>0</code> if
			this character has no lowercase mapping.*/
		public char getLowercaseMapping() {return lowercaseMapping;}

		/**Sets the lowercase mapping of this character.
		@param newLowercaseMapping The lowercase mapping of the character.
		*/
		public void setLowercaseMapping(final char newLowercaseMapping) {lowercaseMapping=newLowercaseMapping;}

	/**The titlecase mapping of this character, or <code>0</code> if this character
		has no titlecase mapping.*/
	private char titlecaseMapping=0;

		/**@return The titlecase mapping of this character, or <code>0</code> if
			this character has no titlecase mapping.*/
		public char getTitlecaseMapping() {return titlecaseMapping;}

		/**Sets the titlecase mapping of this character.
		@param newTitlecaseMapping The titlecase mapping of the character.
		*/
		public void setTitlecaseMapping(final char newTitlecaseMapping) {titlecaseMapping=newTitlecaseMapping;}

	public UnicodeCharacter() {}	//TODO del and change the parsing routine to collect all data before construction
		
	/**Creates a new Unicode character.
	@param newCodeValue The code value of the Unicode character.
	*/
	public UnicodeCharacter(final int newCodeValue)	//TODO make a complete constructor and remove this constructor
	{
		codeValue=newCodeValue;	//save the code value
	}

	/**Creates a new Unicode character with a name.
	@param newCodeValue The code value of the Unicode character.
	@param newCharacterName The name of the Unicode character.
	*/
	public UnicodeCharacter(final int newCodeValue, final String newCharacterName)
	{
		this(newCodeValue);	//do the default construction
		setCharacterName(newCharacterName);	//set the character name
	}

	/**Creates a string representation of a given Unicode code point in the form "U+XXXX[XX]".
	@param codeValue The Unicode code point to represent.
	@return A string representation of the Unicode code point in the form "U+XXXX[XX]"
	 */
	public static String getCodePointString(final int codeValue)
	{
		final StringBuilder stringBuilder=new StringBuilder("U+");	//create a string buffer
			//append the code value, using six digits if needed
		stringBuilder.append(IntegerUtilities.toHexString(codeValue, codeValue<=0xFFFF ? 4 : 6).toUpperCase());
		return stringBuilder.toString();	//return the string we constructed		
	}

	/**Compares this object with the specified object for order.
	This implementation compares code values
	@param object The object to be compared.
	@return A negative integer, zero, or a positive integer as this object is less than, equal to, or greater than the specified object.
	@throws ClassCastException if the specified object's type prevents it from being compared to this object.
	*/
	public int compareTo(final UnicodeCharacter object)
	{
		return getCodeValue()-object.getCodeValue();	//compare code values
	}

	/**@return A string representation of the Unicode character in the form "U+XXXX[XX]".*/
	public String toString()
	{
		return getCodePointString(getCodeValue());	//return a string representation of the code point
	}

}