package com.garretwilson.text;

import java.io.UnsupportedEncodingException;
import java.util.Arrays;

/**Represents a character encoding and related information.
@author Garret Wilson
*/
public class CharacterEncoding implements CharacterEncodingConstants
{

	/**The encoding family (UTF-8, UTF-16, or UCS-4).*/
	private String family=UTF_8;

	/**@return The encoding family (UTF-8, UTF-16, or UCS-4).*/
	public String getFamily() {return family;}

	/**Sets the encoding family.
	@param newFamily The encoding family, one of the constants <code>UTF8</code>,
			<code>UTF16</code>, or <coode>UCS4</code>.*/
	public void setFamily(final String newFamily) {family=newFamily;}

	/**Whether the data is stored in littleEndian format.*/
	private boolean littleEndian=true;

	/**@return Whether the data is stored in littleEndian format.*/
	public boolean isLittleEndian() {return littleEndian;}	//G***change this to use java.nioByteOrder

	/**Sets whether the data is stored in littleEndian format.
	@param newLittleEndian <code>true</code> if the data is stored in little endian
			format, <code>false</code> if the data is stored in big endian format or
			if big/little-endian does not apply to the format (such as UTF-8).
	*/
	public void setLittleEndian(final boolean newLittleEndian) {littleEndian=newLittleEndian;}

	/**@return The specific character encoding used; UTF-16 little endian, for example,
		would return UTF-16LE.*/
	public String getEncoding()
	{
		final String encoding=getFamily();	//get the generic encoding type
		if(encoding.equalsIgnoreCase(UTF_16))	//if this is some type of UTF-16
		{
			if(isLittleEndian())	//if this is little-endian UTF-16
				return UTF_16LE;	//show that the actual encoding type is UTF-16LE
			else	//if this is big-endian UTF-16
				return UTF_16BE;	//show that the actual encoding type is UTF-16BE
		}
		else	//if this is some other type
			return encoding;	//the encoding and the family should match
	}

	/**@return A string representation of the specific character encoding used.
	@see #getEncoding
	*/
	public String toString()
	{
		return getEncoding();	//return the encoding
	}

	/**Default constructor which creates a character encoding with UTF-8 stored in
		little endian format.*/
	public CharacterEncoding()
	{
		this(UTF_8, true);	//create the object with the default values
	}

	/**Creates a new character encoding with a family and byte order.
	@param newFamily The encoding family, one of the constants <code>UTF8</code>,
			<code>UTF16</code>, or <coode>UCS4</code>.
	@param newLittleEndian <code>true</code> if the data is stored in little endian
			format, <code>false</code> if the data is stored in big endian format or
			if big/little-endian does not apply to the format (such as UTF-8).
	*/
	public CharacterEncoding(final String newFamily, final boolean newLittleEndian)
	{
		setFamily(newFamily);	//set the character encoding family
		setLittleEndian(newLittleEndian);	//set the byte order
	}

	/**@return The number of bytes used for each character for this encoding family.*/
	public int getBytesPerCharacter()
	{
		if(getFamily().equalsIgnoreCase(UTF_8))	//if the encoding family is UTF-8
			return 1;	//UTF-8 has one byte per character
		else if(getFamily().equalsIgnoreCase(UTF_16))	//if the encoding family is UTF-16
			return 2;	//UTF-16 has two bytes per character
		else if(getFamily().equalsIgnoreCase(UCS_4))	//if the encoding family is UCS-4
			return 4;	//UCS-4 has four bytes per character
		else	//there should be no other encoding families, but just in case
			return 1;	//the default is one byte per character
	}

	/**Creates a <code>CharacterEncoding</code> object from the given Byte Order Mark.
	If no valid Byte Order Mark is present, <code>null</code> is returned.
	//G***del	If no valid Byte Order Mark is present, UTF-8 is assumed.
	@param byteOrderMarkArray The array of bytes representing the Byte Order Mark.
	@return The character encoding object from the given Byte Order Mark, or
		<code>null</code> if no Byte Order Mark is present.
//G***del if not needed	@exception UnsupportedEncodingException Thrown if the Byte Order Mark is not recognized.
	*/
	public static CharacterEncoding create(final int[] byteOrderMarkArray) //G***del if not needed throws UnsupportedEncodingException
	{		//G***maybe eventually change the array to a byte array
		if(byteOrderMarkArray.length>=2)	//if there are at least two bytes in the array
		{
			if((byte)byteOrderMarkArray[0]==BOM_UTF_16_BIG_ENDIAN[0] && (byte)byteOrderMarkArray[1]==BOM_UTF_16_BIG_ENDIAN[1])	//FE FF: UTF-16, big endian
				return new CharacterEncoding(UTF_16, false);	//construct and return the correct character encoding object
			if((byte)byteOrderMarkArray[0]==BOM_UTF_16_LITTLE_ENDIAN[0] && (byte)byteOrderMarkArray[1]==BOM_UTF_16_LITTLE_ENDIAN[1])	//FF FE: UTF-16, little endian
				return new CharacterEncoding(UTF_16, true);	//construct and return the correct character encoding object
		}
		if(byteOrderMarkArray.length>=3)	//if there are at least two three in the array
		{
			if((byte)byteOrderMarkArray[0]==BOM_UTF_8[0] && (byte)byteOrderMarkArray[1]==BOM_UTF_8[1] && (byte)byteOrderMarkArray[2]==BOM_UTF_8[2])	//EF BB BF: UTF-8
				return new CharacterEncoding(UTF_8, true);	//construct and return the correct character encoding object G***change this to a null ByteOrder object
		}		
		return null;  //show that we didn't find a byte order mark, and thus cannot construct a character encoding object
//G***del when not needed		return new CharacterEncoding(UTF8, false);	//construct and return a default UTF-8 character encoding, since we don't recognize the Byte Order Mark (the big-endian/little-endian byte order flag is meaningless here)
//G***del if not needed		throw new UnsupportedEncodingException("Unrecognized Byte Order Mark");	//show that we don't recognize the bytes in the Byte Order Mark byte array
	}

	/**Creates a <code>CharacterEncoding</code> object from the given Byte Order Mark or,
			if a true Byte Order Mark is not present, by comparing the bytes to expected characters.
	@param byteOrderMarkArray The array of bytes representing the Byte Order Mark.
	@param expectedCharacters The characters expected, regardless of the encoding method used. At least four characters should included.
	@return The character encoding object from the given Byte Order Mark, or if no
		Byte Order Mark is present, the character encoding assumed by comparing bytes
		to the expected characters, or <code>null</code> if neither method can determine
		a character encoding.
//G***del if not needed	@exception UnsupportedEncodingException Thrown if the Byte Order Mark is not recognized.
	*/
	public static CharacterEncoding create(final int[] byteOrderMarkArray, final String expectedCharacters)	//G***del if not needed throws UnsupportedEncodingException
	{
		CharacterEncoding characterEncoding=create(byteOrderMarkArray);	//check the byte order mark by itself; if there is no Byte Order Mark, this will return null
		if(characterEncoding==null)  //if no byte order mark was found, try to compare characters with what is expected
		{
			if(byteOrderMarkArray.length>=4 && expectedCharacters.length()>=1)	//if there are at least four bytes in the array, and we have at least one character to compare them with
			{
				final char firstExpectedChar=expectedCharacters.charAt(0);	//find the first character they were expecting
				if(byteOrderMarkArray[0]==0x00 && byteOrderMarkArray[1]==0x00
						&& byteOrderMarkArray[2]==0x00 && byteOrderMarkArray[3]==(int)firstExpectedChar)	//00 00 00 X1: UCS-4, big-endian (1234 order)
					return new CharacterEncoding(UCS_4, false);	//construct and return the correct character encoding object
				if(byteOrderMarkArray[0]==(int)firstExpectedChar && byteOrderMarkArray[1]==0x00
						&& byteOrderMarkArray[2]==0x00 && byteOrderMarkArray[3]==0x00)	//X1 00 00 00: UCS-4, little-endian (4321 order)
					return new CharacterEncoding(UCS_4, true);	//construct and return the correct character encoding object
	/*G***maybe fix this later for unusual byte orders
				if(byteOrderMarkArray[0]==0x00 && byteOrderMarkArray[1]==0x00
						&& byteOrderMarkArray[2]==(int)firstExpectedChar && byteOrderMarkArray[3]==0x00)	//00 00 X1 00: UCS-4, 2143 order
				if(byteOrderMarkArray[0]==0x00 && byteOrderMarkArray[1]==(int)firstExpectedChar
						&& byteOrderMarkArray[2]==0x00 && byteOrderMarkArray[3]==0x00)	//00 X1 00 00 UCS-4, 3412 order
	*/
				if(expectedCharacters.length()>=2)	//if we have at least two character with which to compare the bytes in the array
				{
					final char secondExpectedChar=expectedCharacters.charAt(1);	//find the second character they were expecting
					if(byteOrderMarkArray[0]==0x00 && byteOrderMarkArray[1]==(int)firstExpectedChar
							&& byteOrderMarkArray[2]==0x00 && byteOrderMarkArray[3]==(int)secondExpectedChar)	//00 X1 00 X2: UTF-16, big-endian, no Byte Order Mark
						return new CharacterEncoding(UTF_16, false);	//construct and return the correct character encoding object
					if(byteOrderMarkArray[0]==(int)firstExpectedChar && byteOrderMarkArray[1]==0x00
							&& byteOrderMarkArray[2]==(int)secondExpectedChar && byteOrderMarkArray[3]==0x00)	//X1 00 X2 00: UTF-16, little-endian, no Byte Order Mark
						return new CharacterEncoding(UTF_16, true);	//construct and return the correct character encoding object
					if(expectedCharacters.length()>=4)	//if we have at least four character with which to compare the bytes in the array
					{
						final char thirdExpectedChar=expectedCharacters.charAt(2);	//find the third character they were expecting
						final char fourthExpectedChar=expectedCharacters.charAt(3);	//find the fourth character they were expecting
						if(byteOrderMarkArray[0]==(int)firstExpectedChar && byteOrderMarkArray[1]==(int)secondExpectedChar
								&& byteOrderMarkArray[2]==(int)thirdExpectedChar && byteOrderMarkArray[3]==(int)fourthExpectedChar)	//X1 X2 X3 X4: UTF-8 (or similar), no Byte Order Mark
							return new CharacterEncoding(UTF_8, false);	//construct and return the correct character encoding object (although the big-endian/little-endian byte order flag is meaningless here)
					}
				}
			}
		}
		return characterEncoding; //return whatever character encoding was found (which may be null)
//G***del		return create(byteOrderMarkArray);	//if no characters match, check the Byte Order Mark by itself	//G***del if not needed (this function will throw an exception if it doesn't recognize the Byte Order Mark)
	}

}
