package com.garretwilson.text;

import static com.garretwilson.lang.CharSequenceUtilities.*;
import static com.garretwilson.lang.ObjectUtilities.*;
import static com.garretwilson.text.CharacterEncodingConstants.*;

import com.garretwilson.lang.ObjectUtilities;

/**Represents a character encoding and related information.
@author Garret Wilson
@see http://www.unicode.org/faq/utf_bom.html#BOM
*/
public class CharacterEncoding
{

	/**Whether a character encoding is little-endiand or big-endian.*/
	public enum Endian{LE, BE};

	/**The encoding family (UTF-8, UTF-16, or UCS-4).*/
	private final String family;

		/**@return The encoding family (UTF-8, UTF-16, or UCS-4).*/
		public String getFamily() {return family;}

	/**Whether the character encoding is little-endian or big-endian, or <code>null</code> if endianness is not specified.*/
	private final Endian endian;

		/**@return Whether the character encoding is little-endian or big-endian, or <code>null</code> if endianness is not specified.*/
		public final Endian getEndian() {return endian;}

	/**The byte order mark, if any, that was used to determine this character encoding.*/
	private final byte[] bom;

		/**The non-<code>null</code> array of byte order mark bytes, if any, that
			were used to determine this character encoding.
		*/
		public byte[] getByteOrderMark() {return bom;}

	/**@return The specific character encoding used; UTF-16 little endian, for example, would return UTF-16LE.*/
	public String getEncoding()
	{
		final StringBuilder stringBuilder=new StringBuilder(getFamily());	//get the generic encoding type
		final Endian endian=getEndian();	//get the endianness
		if(endian!=null)	//if endianness is specified
		{
			stringBuilder.append(endian);	//add the endianness string
		}
		return stringBuilder.toString();	//return the constructed encoding
	}

	/**@return A string representation of the specific character encoding used.
	@see #getEncoding
	*/
	public String toString()
	{
		return getEncoding();	//return the encoding
	}

	/**Creates a new character encoding from an encoding string.
	The character encoding will be given the canonical byte order mark for this family and endianness, if appropriate.
	@param encoding The encoding string, specifying big or little-endian if appropriate.
	@see #getByteOrderMark(String, Endian)
	@exception NullPointerException if the encoding is <code>null</code>.
	*/
	public CharacterEncoding(final String encoding)
	{
		this(getFamily(encoding), getEndian(encoding));	//parse out the family and endianness and construct the class
	}

	/**Creates a new character encoding from an encoding string and a byte order mark.
	@param encoding The encoding string, specifying big or little-endian if appropriate.
	@param bom The non-<code>null</code> array of byte order mark bytes, if
		any, that were used to determine this character encoding.
	@exception NullPointerException if the encoding or byte order mark is <code>null</code>.
	*/
	public CharacterEncoding(final String encoding, final byte[] bom)
	{
		this(getFamily(encoding), getEndian(encoding), bom);	//parse out the family and endianness and construct the class
	}

	/**Creates a new character encoding with a family and byte order.
	The character encoding will be given the canonical byte order mark for this family and endianness, if appropriate.
	@param family The encoding family, one of the constants <code>UTF8</code>,
		<code>UTF16</code>, or <coode>UCS4</code>.
	@param endian Whether the character encoding is little-endian or big-endian,
		or <code>null</code> if endianness is not specified.
	@exception NullPointerException if the family is <code>null</code>.
	@see #getByteOrderMark(String, Endian)
	*/
	public CharacterEncoding(final String family, final Endian endian)
	{
		this(family, endian, getByteOrderMark(family, endian));	//construct a character encoding with no byte order mark
	}

	/**Creates a new character encoding with a family, byte order, and byte order mark.
	@param family The encoding family, one of the constants <code>UTF8</code>,
		<code>UTF16</code>, or <coode>UCS4</code>.
	@param endian Whether the character encoding is little-endian or big-endian,
		or <code>null</code> if endianness is not specified.
	@param bom The non-<code>null</code> array of byte order mark bytes, if
		any, that were used to determine this character encoding.
	@exception NullPointerException if the family or byte order mark is <code>null</code>.
	*/
	public CharacterEncoding(final String family, final Endian endian, final byte[] bom)
	{
		this.family=checkNull(family, "Character encoding family must be given.");	//set the character encoding family
		this.endian=endian;	//set the endianness
		this.bom=checkNull(bom, "Non-null byte order mark must be provided.");	//store the byte order mark used to determine this character encoding
	}

	/**Determines the family from a complete encoding name.
	@param encoding The encoding string.
	@return The family name, without specifying endianness.
	*/
	public static String getFamily(final String encoding)
	{
		final Endian endian=getEndian(encoding);	//get the endianness of the encoding
			//if endianness is specified, remove that part from the string
		return endian!=null ? encoding.substring(0, encoding.length()-endian.toString().length()) : encoding;
	}

	/**Determines the endianness of the given encoding.
	@param encoding The encoding string.
	@return Whether the character encoding is little-endian or big-endian,
		or <code>null</code> if endianness is not specified.
	*/
	public static Endian getEndian(final String encoding)
	{
		for(final Endian endian:Endian.values())	//for each possible endian value
		{
			if(endsWithIgnoreCase(encoding, endian.toString()))	//if the encoding ends with this endian value
			{
				return endian;	//return this endianness
			}
		}
		return null;	//show that the encoding does not specify and endianness
	}

	/**Determines the canonical byte order mark for representing this character encoding.
	For UTF-16 and UCS-4, if no byte order is present then little-endian is assumed.
	@param family The encoding family, such as one of the constants <code>UTF_8</code>,
		<code>UTF_16</code>, or <coode>UCS4</code>.
	@param endian Whether the character encoding is little-endian or big-endian,
		or <code>null</code> if endianness is not specified.
	@return The canonical byte order mark for representing this family and byte order,
	 	which can be an empty array if there is no preferred byte order mark for this encoding.
	@see CharacterEncodingConstants#NO_BOM
	*/ 
	public static byte[] getByteOrderMark(final String family, final Endian endian)
	{
		if(UTF_8.equalsIgnoreCase(family))	//UTF-8
		{
			return BOM_UTF_8;	//UTF-8 has only one BOM
		}
		else if(UTF_16.equalsIgnoreCase(family))	//UTF-16
		{
			return endian==Endian.BE ? BOM_UTF_16_BIG_ENDIAN : BOM_UTF_16_LITTLE_ENDIAN;	//return the correct BOM based upon endianness
		}
		else if(UCS_4.equalsIgnoreCase(family))	//UTF-32
		{
			return endian==Endian.BE ? BOM_UTF_32_BIG_ENDIAN : BOM_UTF_32_LITTLE_ENDIAN;	//return the correct BOM based upon endianness
		}
		else	//if we don't have a BOM for the encoding
		{
			return NO_BOM;	//return no BOM
		}
	}

	/**@return The number of bytes used for each character for this encoding family.*/
	public int getBytesPerCharacter()
	{
		final String family=getFamily();	//get the family of this encoding TODO should we store this in canonical form, or always compare case insensitively
		if(UTF_8.equalsIgnoreCase(family))	//if the encoding family is UTF-8
			return 1;	//UTF-8 has one byte per character
		else if(UTF_16.equalsIgnoreCase(family))	//if the encoding family is UTF-16
			return 2;	//UTF-16 has two bytes per character
		else if(UCS_4.equalsIgnoreCase(family))	//if the encoding family is UCS-4
			return 4;	//UCS-4 has four bytes per character
		else	//there should be no other encoding families, but just in case
			return 1;	//the default is one byte per character
	}

	/**Determines if the byte order mark of this character encoding corresponds
		to the specified byte order mark.
	@param bom The expected byte order mark.
	@return <code>true</code> if the first bytes of the byte order mark used
		to determine this character encoding correspond to the specified
		byte order mark bytes, else <code>false</code>.
	*/
	public boolean isByteOrderMark(final byte[] bom)
	{
		return isByteOrderMark(getByteOrderMark(), bom);	//see if our byte order mark matches the expected byte order mark
	}

	/**Determines if the given array of bytes begins with the specified byte order mark.
	@param array The array of bytes to compare with specified byte order mark.
	@param bom The expected byte order mark.
	@return <code>true</code> if the first bytes of the given byte array
		correspond to the specified byte order mark bytes, else <code>false</code>.
	*/
	protected static boolean isByteOrderMark(final byte[] array, final byte[] bom)
	{
		if(array.length<bom.length)	//if the array of bytes is not as long as the specified byte order mark
			return false;	//there aren't enough bytes to compare
		for(int i=bom.length-1; i>=0; --i)	//look at each of the byte order mark bytes
		{
			if(array[i]!=bom[i])	//if these bytes don't match
				return false;	//show that there is a mismatch
		}
		return true;	//the array of bytes passed all the tests
	}

	/**Creates a <code>CharacterEncoding</code> object from the given Byte Order Mark.
	If no valid Byte Order Mark is present, <code>null</code> is returned.
	@param byteOrderMarkArray The array of bytes representing the Byte Order Mark.
	@return The character encoding object from the given Byte Order Mark, or
		<code>null</code> if no Byte Order Mark is present.
	*/
	public static CharacterEncoding create(final byte[] byteOrderMarkArray)
	{
		if(isByteOrderMark(byteOrderMarkArray, BOM_UTF_16_BIG_ENDIAN))	//FE FF: UTF-16, big endian
			return new CharacterEncoding(UTF_16, Endian.BE, BOM_UTF_16_BIG_ENDIAN);	//construct and return the correct character encoding object
		else if(isByteOrderMark(byteOrderMarkArray, BOM_UTF_16_LITTLE_ENDIAN))	//FF FE: UTF-16, little endian
			return new CharacterEncoding(UTF_16, Endian.LE, BOM_UTF_16_LITTLE_ENDIAN);	//construct and return the correct character encoding object
		else if(isByteOrderMark(byteOrderMarkArray, BOM_UTF_8))	//EF BB BF: UTF-8
			return new CharacterEncoding(UTF_8, null, BOM_UTF_8);	//construct and return the correct character encoding object G***change this to a null ByteOrder object
		else	//if we don't recognize the byte order mark
			return null;  //show that we didn't find a byte order mark, and thus cannot construct a character encoding object
	}

	/**Creates a <code>CharacterEncoding</code> object from the given Byte Order Mark or,
	 	if a true Byte Order Mark is not present, by comparing the bytes to expected characters.
	@param byteOrderMarkArray The array of bytes representing the Byte Order Mark.
	@param expectedCharacters The characters expected, regardless of the encoding method used. At least four characters should included.
	@return The character encoding object from the given Byte Order Mark, or if no
		Byte Order Mark is present, the character encoding assumed by comparing bytes
		to the expected characters, or <code>null</code> if neither method can determine
		a character encoding.
	*/
	public static CharacterEncoding create(final byte[] byteOrderMarkArray, final String expectedCharacters)	//TODO maybe allow a default eight-bit encoding to be specified
	{
		CharacterEncoding characterEncoding=create(byteOrderMarkArray);	//check the byte order mark by itself; if there is no Byte Order Mark, this will return null
		if(characterEncoding==null)  //if no byte order mark was found, try to compare characters with what is expected
		{
			if(byteOrderMarkArray.length>=4 && expectedCharacters.length()>=1)	//if there are at least four bytes in the array, and we have at least one character to compare them with
			{
				final char firstExpectedChar=expectedCharacters.charAt(0);	//find the first character they were expecting
				if(byteOrderMarkArray[0]==0x00 && byteOrderMarkArray[1]==0x00
						&& byteOrderMarkArray[2]==0x00 && byteOrderMarkArray[3]==(byte)firstExpectedChar)	//00 00 00 X1: UCS-4, big-endian (1234 order)
					return new CharacterEncoding(UCS_4, Endian.BE, NO_BOM);	//construct and return the correct character encoding object; no byte order mark was found
				if(byteOrderMarkArray[0]==(byte)firstExpectedChar && byteOrderMarkArray[1]==0x00
						&& byteOrderMarkArray[2]==0x00 && byteOrderMarkArray[3]==0x00)	//X1 00 00 00: UCS-4, little-endian (4321 order)
					return new CharacterEncoding(UCS_4, Endian.LE, NO_BOM);	//construct and return the correct character encoding object; no byte order mark was found
	/*G***maybe fix this later for unusual byte orders
				if(byteOrderMarkArray[0]==0x00 && byteOrderMarkArray[1]==0x00
						&& byteOrderMarkArray[2]==(byte)firstExpectedChar && byteOrderMarkArray[3]==0x00)	//00 00 X1 00: UCS-4, 2143 order
				if(byteOrderMarkArray[0]==0x00 && byteOrderMarkArray[1]==(byte)firstExpectedChar
						&& byteOrderMarkArray[2]==0x00 && byteOrderMarkArray[3]==0x00)	//00 X1 00 00 UCS-4, 3412 order
	*/
				if(expectedCharacters.length()>=2)	//if we have at least two character with which to compare the bytes in the array
				{
					final char secondExpectedChar=expectedCharacters.charAt(1);	//find the second character they were expecting
					if(byteOrderMarkArray[0]==0x00 && byteOrderMarkArray[1]==(byte)firstExpectedChar
							&& byteOrderMarkArray[2]==0x00 && byteOrderMarkArray[3]==(byte)secondExpectedChar)	//00 X1 00 X2: UTF-16, big-endian, no Byte Order Mark
						return new CharacterEncoding(UTF_16, Endian.BE, NO_BOM);	//construct and return the correct character encoding object; no byte order mark was found
					if(byteOrderMarkArray[0]==(byte)firstExpectedChar && byteOrderMarkArray[1]==0x00
							&& byteOrderMarkArray[2]==(byte)secondExpectedChar && byteOrderMarkArray[3]==0x00)	//X1 00 X2 00: UTF-16, little-endian, no Byte Order Mark
						return new CharacterEncoding(UTF_16, Endian.LE, NO_BOM);	//construct and return the correct character encoding object; no byte order mark was found
					if(expectedCharacters.length()>=4)	//if we have at least four character with which to compare the bytes in the array
					{
						final char thirdExpectedChar=expectedCharacters.charAt(2);	//find the third character they were expecting
						final char fourthExpectedChar=expectedCharacters.charAt(3);	//find the fourth character they were expecting
						if(byteOrderMarkArray[0]==(byte)firstExpectedChar && byteOrderMarkArray[1]==(byte)secondExpectedChar
								&& byteOrderMarkArray[2]==(byte)thirdExpectedChar && byteOrderMarkArray[3]==(byte)fourthExpectedChar)	//X1 X2 X3 X4: UTF-8 (or similar), no Byte Order Mark
							return new CharacterEncoding(UTF_8, null, NO_BOM);	//construct and return the correct character encoding object; no byte order mark was found
					}
				}
			}
		}
		return characterEncoding; //return whatever character encoding was found (which may be null)
	}

	/**@return <code>true</code> if the given object is another character encoding with the same family and endianness.*/
	public boolean equals(final Object object)
	{
		if(object instanceof CharacterEncoding)	//if the object is a character encoding
		{
			final CharacterEncoding characterEncoding=(CharacterEncoding)object;	//get the encoding as a character encoding object			
			return getFamily().equals(characterEncoding.getFamily())	//compare encoding family
					&& ObjectUtilities.equals(getEndian(), characterEncoding.getEndian());	//compare endianness
		}
		else	//if the character is not a character encoding
		{
			return false;	//the object is not equal to this one
		}
	}


	/**@return A hashcode value constructed from the encoding string.*/
	public int hashCode()
	{
		return getEncoding().hashCode();  //return the hash code of the encoding
	}

}
