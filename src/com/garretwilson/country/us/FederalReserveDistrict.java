package com.garretwilson.country.us;

/**Districts of the Federal Reserve banking system in the United States.
The districts are in increasing order of number/letter.
@author Garret Wilson
@see <a href="http://www.federalreserve.gov/otherfrb.htm">Federal Reserve Board: Federal Reserve Districts and Banks</a>
@see <a href="http://en.wikipedia.org/wiki/Federal_Reserve_Bank">Wikipedia: Federal Reserve</a>
*/ 
public enum FederalReserveDistrict
{

	BOSTON,
	
	NEW_YORK,
	
	PHILADELPHIA,
	
	CLEVELAND,
	
	RICHMOND,
	
	ATLANTA,
	
	CHICAGO,
	
	ST_LOUIS,
	
	MINNEAPOLIS,
	
	KANSAS_CITY,
	
	DALLAS,
	
	SAN_FRANCISCO;
	
	/**@return The identifying number of this federal reserve district.*/
	public int getNumber()
	{
		return 1+ordinal();	//the bank numbers are in order starting with 1
	}

	/**@return The identifying letter of this federal reserve district.*/
	public char getLetter()
	{
		return (char)('A'+ordinal());	//the bank letters are in order starting with 'A'
	}
}
