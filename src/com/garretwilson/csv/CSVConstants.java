package com.garretwilson.csv;

import com.garretwilson.lang.StringUtilities;
import com.garretwilson.text.CharacterConstants;

/**Constant values for Comma Separated Value (CSV) files.
@author Garret Wilson
*/
public interface CSVConstants
{
	/**The character that delimits CSV fields.*/
	public final static char FIELD_DELIMITER_CHAR=CharacterConstants.COMMA_CHAR;

	/**The character that delimits records.*/
	public final static char RECORD_DELIMITER_CHAR=CharacterConstants.LINE_FEED_CHAR;

	/**A quote character that has been escapted.*/
	public final static String ESCAPED_QUOTATION_MARK_STRING=StringUtilities.createString(CharacterConstants.QUOTATION_MARK_CHAR, 2);
}
