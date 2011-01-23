/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.country.us;

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
