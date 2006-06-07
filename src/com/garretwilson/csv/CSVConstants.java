package com.garretwilson.csv;

import static com.garretwilson.text.CharacterConstants.*;

/**Constant values for Comma Separated Value (CSV) files.
@author Garret Wilson
@see <a href="http://www.rfc-editor.org/rfc/rfc4180.txt">RFC 4180: Common Format and MIME Type for Comma-Separated Values (CSV) Files</a>
*/
public class CSVConstants
{
	/**The character that delimits CSV fields.*/
	public final static char FIELD_DELIMITER_CHAR=COMMA_CHAR;

	/**The string that delimits records.*/
	public final static String RECORD_DELIMITER_STRING=""+CARRIAGE_RETURN_CHAR+LINE_FEED_CHAR;

	/**Characters that require a field to be quoted.*/
	public final static String RESTRICTED_CHARACTERS=RECORD_DELIMITER_STRING+FIELD_DELIMITER_CHAR+QUOTATION_MARK_CHAR;

	/**A quote character that has been escapted.*/
	public final static String ESCAPED_QUOTATION_MARK_STRING=""+QUOTATION_MARK_CHAR+QUOTATION_MARK_CHAR;
}
