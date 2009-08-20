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

package com.globalmentor.model;

import java.util.Calendar;

/**The resolution of time in a calendar in the context of some operation.
@author Garret Wilson
*/
public enum CalendarResolution
{

	/**The era (e.g. AD or BC) in the Julian calendar.*/
	ERA(Calendar.ERA),
	
	/**The calendar year.*/
	YEAR(Calendar.YEAR),

	/**The calendar month.*/
	MONTH(Calendar.MONTH),

	/**The week within the given year.*/
	WEEK(Calendar.WEEK_OF_YEAR),

	/**A day on the calendar.*/
	DAY(Calendar.DAY_OF_YEAR),

	/**Whether the hour is before or after noon.*/
	AM_PM(Calendar.AM_PM),

	/**The hour within a day.*/
	HOUR(Calendar.HOUR_OF_DAY),

	/**A minute of time.*/
	MINUTE(Calendar.MINUTE),
	
	/**A second of time.*/
	SECOND(Calendar.SECOND),

	/**A millisecond of time.*/
	MILLISECOND(Calendar.MILLISECOND);

	/**The related {@link Calendar} field.*/ 
	final int calendarField;

		/**@return The related {@link Calendar} field.*/ 
		public int getCalendarField() {return calendarField;}

	/**Constructs a calendar resolution.
	@param calendarField The related {@link Calendar} field.
	*/
	private CalendarResolution(final int calendarField)
	{
		this.calendarField=calendarField;	//save the calendar field
	}
	
}
