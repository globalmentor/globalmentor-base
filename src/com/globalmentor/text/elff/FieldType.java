package com.globalmentor.text.elff;

/**The type of an individual field in an entry of the Extended Log File Format (ELFF).
@author Garret Wilson
@see <a href="file:///D:/reference/W3C/ELFF/WD-logfile.html">W3C Extended Log File Format</a>
*/
public enum FieldType
{

	/**An Internet address and port.*/
	ADDRESS,
	/**The data in GMT in the format <code>YYYY-MM-DD</code>.*/
	DATE,
	/**A fixed format float.*/
	FIXED,
	/**A sequence of digits.*/
	INTEGER,
	/**A sequence of characters surrounded by the quote ('"') character, with that literal character escaped by doubling.*/
	STRING,
	/**The time in GMT in the format <code>HH:MM</code>, <code>HH:MM:SS</code> or <code>HH:MM:SS.S</code>.*/
	TIME,
	/**An absolute or relative URI.*/
	URI;
}
