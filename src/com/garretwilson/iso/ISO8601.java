package com.garretwilson.iso;

/**Definitions related to ISO 8601-2004, "Data elements and interchange formats — Information interchange — Representation of dates and times".
@author Garret Wilson
*/
public class ISO8601
{

	/**The delimiter that introduces a time component in a temporal.*/
	public final static char TIME_BEGIN='T';
	/**The delimiter that separates year components in a temporal.*/
	public final static char DATE_DELIMITER='-';
	/**The delimiter that separates time components in a temporal.*/
	public final static char TIME_DELIMITER=':';
	/**The UTC designator for "Zulu Time", 'Z'.*/
	public final static char UTC_DESIGNATOR='Z';

}
