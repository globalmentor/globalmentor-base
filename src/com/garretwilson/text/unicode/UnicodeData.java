package com.garretwilson.text.unicode;

import java.io.IOException;
import java.io.Reader;
import java.io.LineNumberReader;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;
import com.garretwilson.lang.IntegerUtilities;

/**Represents the Unicode 3.0 file <code>UnicodeData.txt</code>.
@author Garret Wilson
@version 1.0
*/
public class UnicodeData implements UnicodeConstants
{
			//Unicode data file fields
	/**The number of fields in the file.*/
	public final static int MAX_FIELD=14;
	public final static int FIELD_CODE_VALUE=0;
	public final static int FIELD_CHARACTER_NAME=1;
	public final static int FIELD_GENERAL_CATEGORY=2;
	public final static int FIELD_CANONICAL_COMBINING_CLASS=3;
	public final static int FIELD_BIDIRECTIONAL_CATEGORY=4;
	public final static int FIELD_CHARACTER_DECOMPOSITION_MAPPINGS=5;
	public final static int FIELD_DECIMAL_DIGIT_VALUE=6;
	public final static int FIELD_DIGIT_VALUE=7;
	public final static int FIELD_NUMERIC_VALUE=8;
	public final static int FIELD_MIRRORED=9;
	public final static int FIELD_UNICODE10_NAME=10;
	public final static int FIELD_10646_COMMENT_FIELD=11;
	public final static int FIELD_UPPERCASE_MAPPING=12;
	public final static int FIELD_LOWERCASE_MAPPING=13;
	public final static int FIELD_TITLECASE_MAPPING=14;

	/**The <code>true</code> value for the mirrored field.*/
	public final static String MIRRORED_YES="Y";

	/**The <code>false</code> value for the mirrored field.*/
	public final static String MIRRORED_NO="N";

	/**The data file's field delimiter character.*/
	public final static char FIELD_DELIMITER=';';

	/**The delimiter between mapppings.*/
	public final static char MAPPING_DELIMITER=' ';

	/**The character which divides a fraction.*/
	public final static char FRACTION_DIVIDER='/';


	/**Parses an input reader which contains Unicode data, and creates and returns
		a list of Unicode characters.
	@param reader The reader which contains the data in the Unicode data format.
	@return A list of Unicode character objects.
	@exception IOException Thrown if there was an error parsing the Unicode data.
	@see UnicodeCharacter
	@see UnicodeConstants
	*/
	public static List parse(final Reader reader) throws IOException
	{
		final List unidataList=new ArrayList();	//create a list so that we can pass back the Unicode characters
		final LineNumberReader lineNumberReader=new LineNumberReader(reader);	//create a reader to allow us to read the file line by line
		try
		{
			String unidataLine=lineNumberReader.readLine();	//read the first line of text
			while(unidataLine!=null)	//while there are more lines left
			{
				unidataList.add(parseLine(unidataLine));	//parse this line and add the resulting Unicode character to our list
				unidataLine=lineNumberReader.readLine();	//read the next line of text
			}
			return unidataList;	//return our list of Unicode character objects
		}
		catch(Exception e)	//if any errors occur
		{
			throw new IOException("Error parsing line "+lineNumberReader.getLineNumber()+": "+e.toString());	//indicate the line we were trying to parse
		}
	}

	/**Parses a line in a Unicode data file and constructs and returns a Unicode character object.
	@param unidataLine The line of text to parse.
	@return The Unicode character object that represents the character the information
		for which was contained in the line.
	@exception IOException Thrown if there was an error parsing the Unicode data.
	*/
	protected static UnicodeCharacter parseLine(String unidataLine) throws IOException
	{
//G***del		UnicodeCharacter unicodeCharacter=null;	//we'll create this when we find the code value
		unidataLine+=FIELD_DELIMITER;	//since the Unicode data file doesn't have an ending delimiter, add one so that StringTokenizer will recognize the last field
		final UnicodeCharacter unicodeCharacter=new UnicodeCharacter();	//create a new Unicode character that will hold the data we parse from this line
/*G***del
		int codeValue=-1;	//we'll change this from -1 when we get a valid code value
		String characterName=null;
		String generalCategory=null;
		int canonicalCombiningClass=-1;
		String bidirectionalCategory=null;
		String characterDecompositionMappings="";
		int decimalDigitValue=-1;
		int digitValue=-1;
		float numericValue=Float.NaN;
		boolean mirrored=false;
		String unicode10Name=null;
		String iso10646Comment=null;
		char uppercaseMapping=0;
		char lowercaseMapping=0;
		char titlecaseMapping=0;
*/
		final StringTokenizer fieldTokenizer=new StringTokenizer(unidataLine, String.valueOf(FIELD_DELIMITER), true);	//create an object to tokenize the line of Unicode data, and return the delimiters as well (because otherwise two consecutive delimiters will be skipped)
		int fieldIndex=0;	//show which field we are processing
		boolean expectingToken=true;	//show that we're expecting a token first of all
//G***del		for(fieldIndex=0; fieldTokenizer.hasMoreTokens(); ++fieldIndex)	//keep getting more tokens as long as there are fields
		while(fieldTokenizer.hasMoreTokens())	//while there are more fields on this line
		{
			String fieldValue=fieldTokenizer.nextToken();	//get the next field value
			if(fieldValue.equals(String.valueOf(FIELD_DELIMITER)))	//if this is a delimiter
			{
				if(expectingToken)	//if we were expecting a token
					fieldValue="";	//show that there was a blank token; since we just received a delimiter, we're still expecting another token
				else	//if we were expecting this delimiter (that is, we weren't expecting a token)
				{
					expectingToken=true;	//show that we're now expecting a token, since we just received a delimiter
					continue;	//don't process the delimiter that we expected
				}
			}
			else	//if this is a token
				expectingToken=false;	//show that we're no longer expecting a token, since we just received one
			try
			{
//G***del System.out.println("Field "+fieldIndex+": \""+fieldValue+"\".");	//G***del
				switch(fieldIndex)	//see which field this is
				{
					case FIELD_CODE_VALUE:
						unicodeCharacter.setCodeValue((char)Integer.parseInt(fieldValue, 16));	//decode the hexadecimal code value
						break;
					case FIELD_CHARACTER_NAME:
						unicodeCharacter.setCharacterName(fieldValue);	//store the character name
						break;
					case FIELD_GENERAL_CATEGORY:
						unicodeCharacter.setGeneralCategory(fieldValue);	//store the general category
						break;
					case FIELD_CANONICAL_COMBINING_CLASS:
						unicodeCharacter.setCanonicalCombiningClass(Integer.parseInt(fieldValue));	//store the canonical combining class
						break;
					case FIELD_BIDIRECTIONAL_CATEGORY:
						unicodeCharacter.setBidirectionalCategory(fieldValue);	//store the bidirectional category
						break;
					case FIELD_CHARACTER_DECOMPOSITION_MAPPINGS:
						{
							final StringBuffer mappingsBuffer=new StringBuffer();	//create a buffer to hold our mappings
							final StringTokenizer mappingTokenizer=new StringTokenizer(fieldValue, String.valueOf(MAPPING_DELIMITER));	//create an object to tokenize the mappings in this field
							while(mappingTokenizer.hasMoreTokens())	//while there are more mappings in this field
							{
								final String characterDecompositionToken=mappingTokenizer.nextToken();	//get the next character decomposition token
//G***del System.out.println("  Character decomposition token: \""+characterDecompositionToken+"\".");	//G***del
								if(characterDecompositionToken.charAt(0)==CHARACTER_DECOMPOSITION_TAG_BEGIN)	//if this is the beginning of a character decomposition tag
								{
									if(unicodeCharacter.getCharacterDecompositionTag().length()>0)	//if we've already found a formatting tag
										throw new IOException("Multiple character decomposition formatting tags present.");	//show that we don't recognize multiple formatting tags
									else	//if this is the first formatting tag we've found
										unicodeCharacter.setCharacterDecompositionTag(characterDecompositionToken);		//set the character's tag
								}
								else	//if this is another mapping in the decomposition
									mappingsBuffer.append((char)Integer.parseInt(characterDecompositionToken, 16));	//convert the mapping from a hex string to an integer, cast it to a char, and add it to our list of decomposition mappings
							}
							unicodeCharacter.setCharacterDecompositionMappings(mappingsBuffer.toString());	//convert the mappings to a string and store it in our Unicode character object
						}
						break;
					case FIELD_DECIMAL_DIGIT_VALUE:
						if(fieldValue.length()>0)	//if there is a field value
							unicodeCharacter.setDecimalDigitValue(Integer.parseInt(fieldValue));	//get its integer value
						break;
					case FIELD_DIGIT_VALUE:
						if(fieldValue.length()>0)	//if there is a field value
							unicodeCharacter.setDigitValue(Integer.parseInt(fieldValue));	//get its integer value
						break;
					case FIELD_NUMERIC_VALUE:
						if(fieldValue.length()>0)	//if there is a field value
						{
							final int fractionDividerIndex=fieldValue.indexOf(FRACTION_DIVIDER);	//see where the fraction divider is, if there is one
							if(fractionDividerIndex!=-1)	//if this is a fraction
							{
								unicodeCharacter.setNumericValueNumerator(Integer.parseInt(fieldValue.substring(0, fractionDividerIndex)));	//get the float value of the numerator
								unicodeCharacter.setNumericValueDenominator(Integer.parseInt(fieldValue.substring(fractionDividerIndex+1)));	//get the integer value of the denominator
							}
							else	//if this is not a fraction
								unicodeCharacter.setNumericValue(Integer.parseInt(fieldValue));	//get its float value
						}
						break;
					case FIELD_MIRRORED:
						unicodeCharacter.setMirrored(fieldValue.equals(MIRRORED_YES));	//see if the field specifies mirrored
						break;
					case FIELD_UNICODE10_NAME:
						if(fieldValue.length()>0)	//if there is a field value
							unicodeCharacter.setUnicode10Name(fieldValue);	//set the Unicode 1.0 name
						break;
					case FIELD_10646_COMMENT_FIELD:
						if(fieldValue.length()>0)	//if there is a field value
							unicodeCharacter.setISO10646Comment(fieldValue);	//set the ISO 10646 comment
						break;
					case FIELD_UPPERCASE_MAPPING:
						if(fieldValue.length()>0)	//if there is a field value
								unicodeCharacter.setUppercaseMapping((char)Integer.parseInt(fieldValue, 16));	//convert the mapping from a hex string to an integer, and cast it to a char
						break;
					case FIELD_LOWERCASE_MAPPING:
						if(fieldValue.length()>0)	//if there is a field value
								unicodeCharacter.setLowercaseMapping((char)Integer.parseInt(fieldValue, 16));	//convert the mapping from a hex string to an integer, and cast it to a char
						break;
					case FIELD_TITLECASE_MAPPING:
						if(fieldValue.length()>0)	//if there is a field value
								unicodeCharacter.setTitlecaseMapping((char)Integer.parseInt(fieldValue, 16));	//convert the mapping from a hex string to an integer, and cast it to a char
						break;
					default:
						throw new IOException("Unrecognized field: "+fieldIndex);	//show that we don't recognize this field
				}
			}
			catch(Exception e)	//if any errors occur
			{
				throw new IOException("Error parsing field "+fieldIndex+": "+e.toString());	//indicate the field value we were trying to decode
			}
			++fieldIndex;	//show that we're going to the next field
		}
		if(fieldIndex<=MAX_FIELD)	//if this line didn't have enough fields
			throw new IOException("Missing fields; trying to process field "+fieldIndex+".");	//show that there weren't enough fields on this line
		return unicodeCharacter;	//return the character we constructed
	}

	/**Converts a value into a four-digit hex string.
	@param value The value to convert.
	@return four-digit hex version of the given value
	*/
/*G***del when works
	public static String convertValueToFourDigitHexString(final int value)  //G***move this to UnicodeUtilities or something
	{
		final String hexString="0000"+Integer.toHexString(value).toUpperCase();	//create a hex string in uppercase with leading zeros
		return hexString.substring(hexString.length()-4);	//return only the last four digits
	}
*/

	/**Converts the Unicode character into a string with the same format as
		that in the Unicode data file.
	@param unicodeCharacter The character to convert to a string.
	@return A Unicode data representation of the Unicode character.
	*/
	public static String toUnicodeDataLine(final UnicodeCharacter unicodeCharacter)
	{
		final StringBuffer stringBuffer=new StringBuffer();	//create a string buffer
		stringBuffer.append(IntegerUtilities.toHexString(unicodeCharacter.getCodeValue(), 4));	//append the code value
		stringBuffer.append(FIELD_DELIMITER);	//separate the fields
		stringBuffer.append(unicodeCharacter.getCharacterName());	//append the character name
		stringBuffer.append(FIELD_DELIMITER);	//separate the fields
		stringBuffer.append(unicodeCharacter.getGeneralCategory());	//append the general category
		stringBuffer.append(FIELD_DELIMITER);	//separate the fields
		stringBuffer.append(unicodeCharacter.getCanonicalCombiningClass());	//append the canonical combining class
		stringBuffer.append(FIELD_DELIMITER);	//separate the fields
		stringBuffer.append(unicodeCharacter.getBidirectionalCategory());	//append the bidirectional category
		stringBuffer.append(FIELD_DELIMITER);	//separate the fields
		if(unicodeCharacter.getCharacterDecompositionTag().length()>0)	//if there is a decomposition tag
			stringBuffer.append(unicodeCharacter.getCharacterDecompositionTag()+MAPPING_DELIMITER);	//append the decomposition tag
		for(int i=0; i<unicodeCharacter.getCharacterDecompositionMappings().length(); ++i)	//look at each of the decomposition mappings
		{
			stringBuffer.append(IntegerUtilities.toHexString(unicodeCharacter.getCharacterDecompositionMappings().charAt(i), 4));	//append the hex code for this decomposition mapping
			if(i<unicodeCharacter.getCharacterDecompositionMappings().length()-1)	//if this isn't the last mapping
				stringBuffer.append(MAPPING_DELIMITER);	//append the decomposition tag mapping delimiter to separate the mappings
		}
		stringBuffer.append(FIELD_DELIMITER);	//separate the fields
		if(unicodeCharacter.getDecimalDigitValue()!=-1)	//if there is a decimal digit value
			stringBuffer.append(unicodeCharacter.getDecimalDigitValue());	//append the decimal digit value
		stringBuffer.append(FIELD_DELIMITER);	//separate the fields
		if(unicodeCharacter.getDigitValue()!=-1)	//if there is a digit value
			stringBuffer.append(unicodeCharacter.getDigitValue());	//append the digit value
		stringBuffer.append(FIELD_DELIMITER);	//separate the fields
		if(unicodeCharacter.getNumericValue()!=-1)	//if there is a numeric value
		{
			if(unicodeCharacter.isNumericValueFraction())	//if the numeric value is a fraction
				stringBuffer.append(Integer.toString(unicodeCharacter.getNumericValueNumerator())+FRACTION_DIVIDER+unicodeCharacter.getNumericValueDenominator());	//append the fraction form of the numeric value
			else	//if the numeric value is not a fraction
				stringBuffer.append(Integer.toString(unicodeCharacter.getNumericValueNumerator()));	//append the numeric value
		}
		stringBuffer.append(FIELD_DELIMITER);	//separate the fields
		stringBuffer.append(unicodeCharacter.isMirrored() ? MIRRORED_YES : MIRRORED_NO);	//append the mirrored status
		stringBuffer.append(FIELD_DELIMITER);	//separate the fields
		stringBuffer.append(unicodeCharacter.getUnicode10Name());	//append the Unicode 1.0 name, if present
		stringBuffer.append(FIELD_DELIMITER);	//separate the fields
		stringBuffer.append(unicodeCharacter.getISO10646Comment());	//append the ISO 10646 comment, if present
		stringBuffer.append(FIELD_DELIMITER);	//separate the fields
		if(unicodeCharacter.getUppercaseMapping()!=0)	//if there is an uppercase mapping
			stringBuffer.append(IntegerUtilities.toHexString(unicodeCharacter.getUppercaseMapping(), 4));	//append the uppercase mapping
		stringBuffer.append(FIELD_DELIMITER);	//separate the fields
		if(unicodeCharacter.getLowercaseMapping()!=0)	//if there is an lowercase mapping
			stringBuffer.append(IntegerUtilities.toHexString(unicodeCharacter.getLowercaseMapping(), 4));	//append the lowercase mapping
		stringBuffer.append(FIELD_DELIMITER);	//separate the fields
		if(unicodeCharacter.getTitlecaseMapping()!=0)	//if there is an titlecase mapping
			stringBuffer.append(IntegerUtilities.toHexString(unicodeCharacter.getTitlecaseMapping(), 4));	//append the titlecase mapping
		return stringBuffer.toString();	//convert the buffer to a string and return it
	}

}