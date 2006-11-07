package com.garretwilson.util;

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
