package com.garretwilson.text;

/**Named constants representing several often-used characters. In most cases,
	names are derived from Unicode 3.0 names.
@author Garret Wilson
*/
public interface CharacterConstants
{
	/**The character with Unicode code point zero.*/
	public final static char NULL_CHAR=0x0000;
	/**A horizontal tab.*/
	public final static char HORIZONTAL_TABULATION_CHAR=0x0009;
	/**A linefeed.*/
	public final static char LINE_FEED_CHAR=0x000A;
	/**A vertical tab.*/
	public final static char VERTICAL_TABULATION_CHAR=0x000B;
	/**A formfeed.*/
	public final static char FORM_FEED_CHAR=0x000C;
	/**A carriage return.*/
	public final static char CARRIAGE_RETURN_CHAR=0x000D;
	/**A unit separator character.*/
	public final static char UNIT_SEPARATOR_CHAR=0x001F;
	/**A space character.*/
	public final static char SPACE_CHAR=0x0020;
	/**A quotation mark character.*/
	public final static char QUOTATION_MARK_CHAR=0x0022;
	/**An apostrophe character.*/
	public final static char APOSTROPHE_CHAR=0x0027;
	/**A comma character.*/
	public final static char COMMA_CHAR=0x002C;
	/**A hyphen or minus character.*/
	public final static char HYPHEN_MINUS_CHAR=0x002D;
	/**A colon character.*/
	public final static char COLON_CHAR=0x003A;
	/**A semicolon character.*/
	public final static char SEMICOLON_CHAR=0x003B;
	/**An equals sign character.*/
	public final static char EQUALS_SIGN_CHAR=0x003D;
	/**A grave accent character.*/
	public final static char GRAVE_ACCENT_CHAR=0x0060;
	/**A left-pointing guillemet character.*/
	public final static char LEFT_POINTING_DOUBLE_ANGLE_QUOTATION_MARK_CHAR=0x00AB;
	/**A right-pointing guillemet character.*/
	public final static char RIGHT_POINTING_DOUBLE_ANGLE_QUOTATION_MARK_CHAR=0x00BB;
	/**The copyright symbol.*/
	public final static char COPYRIGHT_SIGN=0x00A9;
	/**The pilcrow or paragraph sign.
	@see #PARAGRAPH_SIGN_CHAR
	*/
	public final static char PILCROW_SIGN_CHAR=0x00B6;
	/**The paragraph sign.
	@see #PILCROW_SIGN_CHAR
	*/
	public final static char PARAGRAPH_SIGN_CHAR=PILCROW_SIGN_CHAR;
	/**An uppercase oe ligature.*/
	public final static char LATIN_CAPITAL_LIGATURE_OE_CHAR=0x0152;
	/**A lowercase oe ligature.*/
	public final static char LATIN_SMALL_LIGATURE_OE_CHAR=0x0153;
	/**A Y umlaut.*/
	public final static char LATIN_CAPITAL_LETTER_Y_WITH_DIAERESIS_CHAR=0x0178;
	/**A zero-width space (ZWSP) that may expand during justification.*/
	public final static char ZERO_WIDTH_SPACE_CHAR=0x200B;
	/**A zero-width non-joiner (200C;ZERO WIDTH NON-JOINER;Cf;0;BN;;;;;N;;;;;).*/
	public final static char ZERO_WIDTH_NON_JOINER_CHAR=0x200C;
	/**A zero-width joiner (200D;ZERO WIDTH JOINER;Cf;0;BN;;;;;N;;;;;).*/
	public final static char ZERO_WIDTH_JOINER_CHAR=0x200D;
	/**A left-to-right mark (200E;LEFT-TO-RIGHT MARK;Cf;0;L;;;;;N;;;;;).*/
	public final static char LEFT_TO_RIGHT_MARK_CHAR=0x200E;
	/**A right-to-right mark (200F;RIGHT-TO-LEFT MARK;Cf;0;R;;;;;N;;;;;).*/
	public final static char RIGHT_TO_LEFT_MARK_CHAR=0x200F;
	/**A zero-width non-breaking space&mdash;word joiner (WJ).*/
	public final static char WORD_JOINER_CHAR=0x2060;
	/**A left single quote.*/
	public final static char LEFT_SINGLE_QUOTATION_MARK_CHAR=0x2018;
	/**A right single quote.*/
	public final static char RIGHT_SINGLE_QUOTATION_MARK_CHAR=0x2019;
	/**A single low-9 quotation mark.*/
	public final static char SINGLE_LOW_9_QUOTATION_MARK_CHAR=0x201A;
/*TODO fix and add to FORMAT_CHARS 
	202A;LEFT-TO-RIGHT EMBEDDING;Cf;0;LRE;;;;;N;;;;;
	202B;RIGHT-TO-LEFT EMBEDDING;Cf;0;RLE;;;;;N;;;;;
	202C;POP DIRECTIONAL FORMATTING;Cf;0;PDF;;;;;N;;;;;
	202D;LEFT-TO-RIGHT OVERRIDE;Cf;0;LRO;;;;;N;;;;;
	202E;RIGHT-TO-LEFT OVERRIDE;Cf;0;RLO;;;;;N;;;;;
206A;INHIBIT SYMMETRIC SWAPPING;Cf;0;BN;;;;;N;;;;;
206B;ACTIVATE SYMMETRIC SWAPPING;Cf;0;BN;;;;;N;;;;;
206C;INHIBIT ARABIC FORM SHAPING;Cf;0;BN;;;;;N;;;;;
206D;ACTIVATE ARABIC FORM SHAPING;Cf;0;BN;;;;;N;;;;;
206E;NATIONAL DIGIT SHAPES;Cf;0;BN;;;;;N;;;;;
206F;NOMINAL DIGIT SHAPES;Cf;0;BN;;;;;N;;;;;
FFF9;INTERLINEAR ANNOTATION ANCHOR;Cf;0;BN;;;;;N;;;;;
FFFA;INTERLINEAR ANNOTATION SEPARATOR;Cf;0;BN;;;;;N;;;;;
FFFB;INTERLINEAR ANNOTATION TERMINATOR;Cf;0;BN;;;;;N;;;;;
*/
	/**A single high-reversed-9 quotation mark.*/
	public final static char SINGLE_HIGH_REVERSED_9_QUOTATION_MARK_CHAR=0x201B;
	/**A left double quote.*/
	public final static char LEFT_DOUBLE_QUOTATION_MARK_CHAR=0x201C;
	/**A right double quote.*/
	public final static char RIGHT_DOUBLE_QUOTATION_MARK_CHAR=0x201D;
	/**A double low-9 quotation mark.*/
	public final static char DOUBLE_LOW_9_QUOTATION_MARK_CHAR=0x201E;
	/**A double high-reversed-9 quotation mark.*/
	public final static char DOUBLE_HIGH_REVERSED_9_QUOTATION_MARK_CHAR=0x201F;
	/**Unicode en dash character.*/
	public final static char EN_DASH_CHAR=0x2013;
	/**Unicode em dash character.*/
	public final static char EM_DASH_CHAR=0x2014;
	/**Unicode bullet character.*/
	public final static char BULLET_CHAR=0x2022;
	/**Unicode horizontal ellipsis.*/
	public final static char HORIZONTAL_ELLIPSIS_CHAR=0x2026;
	/**A left-pointing single guillemet character.*/
	public final static char SINGLE_LEFT_POINTING_ANGLE_QUOTATION_MARK_CHAR=0x2039;
	/**A right-pointing single guillemet character.*/
	public final static char SINGLE_RIGHT_POINTING_ANGLE_QUOTATION_MARK_CHAR=0x203A;
	/**Unicode trademark character.*/
	public final static char TRADE_MARK_SIGN_CHAR=0x2122;
	/**Unicode no-break space (NBSP).*/
	public final static char NO_BREAK_SPACE_CHAR=0x00A0;
	/**A reversed double prime quotation mark.*/
	public final static char REVERSED_DOUBLE_PRIME_QUOTATION_MARK_CHAR=0x301D;
	/**A double prime quotation mark.*/
	public final static char DOUBLE_PRIME_QUOTATION_MARK_CHAR=0x301E;
	/**A low double prime quotation mark.*/
	public final static char LOW_DOUBLE_PRIME_QUOTATION_MARK_CHAR=0x301F;
	/**A full width quotation mark.*/
	public final static char FULLWIDTH_QUOTATION_MARK_CHAR=0xFF02;
	/**A zero-width no-breaking space (ZWNBSP)&mdash;the Byte Order Mark (BOM) (FEFF;ZERO WIDTH NO-BREAK SPACE;Cf;0;BN;;;;;N;BYTE ORDER MARK;;;;).
	For non-breaking purposes, deprecated in favor of <code>WORD_JOINER_CHAR</code>.
	@see #WORD_JOINER_CHAR
	*/
	public final static char ZERO_WIDTH_NO_BREAK_SPACE_CHAR=0xFEFF;
	/**The Byte Order Mark (BOM).
	@see #ZERO_WIDTH_NO_BREAK_SPACE_CHAR
	*/
	public final static char BOM_CHAR=ZERO_WIDTH_NO_BREAK_SPACE_CHAR;
	/**A character for a placeholder in text for an otherwise unspecified object.*/
	public final static char OBJECT_REPLACEMENT_CHAR=0xFFFC;
	/**Represents a character that is unknown or unrepresentable in Unicode.*/
	public final static char REPLACEMENT_CHAR=0xFFFD;
	/**An invalid, undefined Unicode character.*/
	public final static char UNDEFINED_CHAR=0xFFFF;

	/**Unicode control characters (0x0000-0x001F, 0x007F-0x09F).*/
	public final static String CONTROL_CHARS=""+
/*G***del
		"\000\001\002\003\004\005\006\007\010\011\012\013\014\015\016\017\020\021\022\023\024\025\026\027\030\031"+
		"\127\128\129\130\131\132\133\134\135\136\137\138\139\140\141\142\143\144\145\146\147\148\149\150"+
		"\151\152\153\154\155\156\\157\159\159";
*/
		(char)0x0000+(char)0x0001+(char)0x0002+(char)0x0003+(char)0x0004+(char)0x0005+(char)0x0006+(char)0x0007+
		(char)0x0008+(char)0x0009+(char)0x000A+(char)0x000B+(char)0x000C+(char)0x000D+(char)0x000E+(char)0x000F+
		(char)0x0010+(char)0x0011+(char)0x0012+(char)0x0013+(char)0x0014+(char)0x0015+(char)0x0016+(char)0x0017+
		(char)0x0018+(char)0x0019+(char)0x001A+(char)0x001B+(char)0x001C+(char)0x001D+(char)0x001E+(char)0x001F+
		(char)0x007F+
		(char)0x0080+(char)0x0081+(char)0x0082+(char)0x0083+(char)0x0084+(char)0x0085+(char)0x0086+(char)0x0087+
		(char)0x0088+(char)0x0089+(char)0x008A+(char)0x008B+(char)0x008C+(char)0x008D+(char)0x008E+(char)0x008F+
		(char)0x0090+(char)0x0091+(char)0x0092+(char)0x0093+(char)0x0094+(char)0x0095+(char)0x0096+(char)0x0097+
		(char)0x0098+(char)0x0099+(char)0x009A+(char)0x009B+(char)0x009C+(char)0x009D+(char)0x009E+(char)0x009F;

	/**Characters considered to be whitespace for purposes of tokenization.
		The therefore include the <code>NO_BREAK_SPACE_CHAR</code>, 0x00A0.
		G***finish this to include things found in java.lang.Character.isWhiteSpace()
		G***does this need to be compatible with the Unicode whitespace characters? Unicode makes a distinction between ws, b, etc.
	*/	//TODO fix for Unicode; fix all the uses of this string; it should *not* have no-break-space character in it, and it should not have control characters
			//TODO most of the calling methods probably want to use TRIM_CHARS
	public final static String WHITESPACE_CHARS=CONTROL_CHARS+
		  SPACE_CHAR+
			NO_BREAK_SPACE_CHAR;
/*G***del
		  HORIZONTAL_TABULATION_CHAR+
			LINE_FEED_CHAR+
			VERTICAL_TABULATION_CHAR+
		  FORM_FEED_CHAR+
		  CARRIAGE_RETURN_CHAR+
			UNIT_SEPARATOR_CHAR+
			FORM_FEED_CHAR+
			SPACE_CHAR+
			NO_BREAK_SPACE_CHAR*/;

	/**Unicode formatting characters; Unicode characters marked with "Cf",
		such as <code>WORD_JOINER</code>.
	*/
	public final static String FORMAT_CHARS=""+ZERO_WIDTH_NON_JOINER_CHAR+ZERO_WIDTH_JOINER_CHAR
			+LEFT_TO_RIGHT_MARK_CHAR+RIGHT_TO_LEFT_MARK_CHAR
			+WORD_JOINER_CHAR+ZERO_WIDTH_NO_BREAK_SPACE_CHAR;

	/**Characters considered to be end-of-line markers (e.g. CR and LF).*/
	public final static String EOL_CHARS=""+CARRIAGE_RETURN_CHAR+LINE_FEED_CHAR;

	/**Characters that do not contain visible "content", and may be trimmed from ends of a string.
	These include whitespace, control characters, and formatting characters.
	*/
	public final static String TRIM_CHARS=WHITESPACE_CHARS+CONTROL_CHARS+FORMAT_CHARS;
	/**Characters used in Roman numbers.*/
//G***del	public final static String ROMAN_NUMERAL_CHARS="IVXCLM";  //G***make sure this is exhaustive

	/**Characters that could be considered the start of a quotation.*/
	public final static String LEFT_QUOTE_CHARS=""+
			QUOTATION_MARK_CHAR+
			APOSTROPHE_CHAR+
			REVERSED_DOUBLE_PRIME_QUOTATION_MARK_CHAR+
			DOUBLE_PRIME_QUOTATION_MARK_CHAR+
			LOW_DOUBLE_PRIME_QUOTATION_MARK_CHAR+
			FULLWIDTH_QUOTATION_MARK_CHAR+
			LEFT_SINGLE_QUOTATION_MARK_CHAR+
			LEFT_DOUBLE_QUOTATION_MARK_CHAR+
			LEFT_POINTING_DOUBLE_ANGLE_QUOTATION_MARK_CHAR+
			SINGLE_LOW_9_QUOTATION_MARK_CHAR+
			DOUBLE_LOW_9_QUOTATION_MARK_CHAR+
			SINGLE_LEFT_POINTING_ANGLE_QUOTATION_MARK_CHAR;

	/**Characters that could be considered the end of a quotation.*/
	public final static String RIGHT_QUOTE_CHARS=""+
			QUOTATION_MARK_CHAR+
			APOSTROPHE_CHAR+
			REVERSED_DOUBLE_PRIME_QUOTATION_MARK_CHAR+
			DOUBLE_PRIME_QUOTATION_MARK_CHAR+
			LOW_DOUBLE_PRIME_QUOTATION_MARK_CHAR+
			FULLWIDTH_QUOTATION_MARK_CHAR+
			RIGHT_SINGLE_QUOTATION_MARK_CHAR+
			RIGHT_DOUBLE_QUOTATION_MARK_CHAR+
			RIGHT_POINTING_DOUBLE_ANGLE_QUOTATION_MARK_CHAR+
			SINGLE_HIGH_REVERSED_9_QUOTATION_MARK_CHAR+
			DOUBLE_HIGH_REVERSED_9_QUOTATION_MARK_CHAR+
			SINGLE_RIGHT_POINTING_ANGLE_QUOTATION_MARK_CHAR;

	/**Characters that start or end quotations.*/
	public final static String QUOTE_CHARS=""+
			QUOTATION_MARK_CHAR+
			APOSTROPHE_CHAR+
			REVERSED_DOUBLE_PRIME_QUOTATION_MARK_CHAR+
			DOUBLE_PRIME_QUOTATION_MARK_CHAR+
			LOW_DOUBLE_PRIME_QUOTATION_MARK_CHAR+
			FULLWIDTH_QUOTATION_MARK_CHAR+
			LEFT_SINGLE_QUOTATION_MARK_CHAR+
			RIGHT_SINGLE_QUOTATION_MARK_CHAR+
			LEFT_DOUBLE_QUOTATION_MARK_CHAR+
			RIGHT_DOUBLE_QUOTATION_MARK_CHAR+
			LEFT_POINTING_DOUBLE_ANGLE_QUOTATION_MARK_CHAR+
			RIGHT_POINTING_DOUBLE_ANGLE_QUOTATION_MARK_CHAR+
			SINGLE_LOW_9_QUOTATION_MARK_CHAR+
			SINGLE_HIGH_REVERSED_9_QUOTATION_MARK_CHAR+
			DOUBLE_LOW_9_QUOTATION_MARK_CHAR+
			DOUBLE_HIGH_REVERSED_9_QUOTATION_MARK_CHAR+
			SINGLE_LEFT_POINTING_ANGLE_QUOTATION_MARK_CHAR+
			SINGLE_RIGHT_POINTING_ANGLE_QUOTATION_MARK_CHAR;

	/**Characters used to punctuate phrases and sentences.*/
	public final static String PHRASE_PUNCTUATION_CHARS=".,:;?!";	//G***use constants here

	/**Punctuation that expects a character to follow at some point.*/
	public final static String DEPENDENT_PUNCTUATION_CHARS=""+
		  COLON_CHAR+';'+COMMA_CHAR+HYPHEN_MINUS_CHAR+EM_DASH_CHAR+EN_DASH_CHAR/*G***del; this probably hurts more then helps+'='*/;  //G***use a constant

	/**Left punctuation used to group characters.*/
	public final static String LEFT_GROUP_PUNCTUATION_CHARS="([{<"; //G***use constants

	/**Right punctuation used to group characters.*/
	public final static String RIGHT_GROUP_PUNCTUATION_CHARS=")]}>"; //G***use constants

	/**Punctuation used to group characters.*/
	public final static String GROUP_PUNCTUATION_CHARS=LEFT_GROUP_PUNCTUATION_CHARS+RIGHT_GROUP_PUNCTUATION_CHARS;

	/**Characters used to punctuate phrases and sentences, as well as general
		punctuation such as quotes.*/
	public final static String PUNCTUATION_CHARS=
		  PHRASE_PUNCTUATION_CHARS+
			GROUP_PUNCTUATION_CHARS+
			QUOTE_CHARS+
			HYPHEN_MINUS_CHAR+EM_DASH_CHAR+EN_DASH_CHAR;

	/**Characters that separate words.*/
	public final static String WORD_DELIMITER_CHARS=WHITESPACE_CHARS+PUNCTUATION_CHARS;	//G***this needs fixed

	/**Characters that allow words to wrap.*/
	public final static String WORD_WRAP_CHARS=WHITESPACE_CHARS+"-/";	//G***use constants

}