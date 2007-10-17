package com.garretwilson.urf;

import java.util.*;

/**The abstract base type for <code>urf.Date</code> and <code>urf.DateTime</code> types.
If there is no explicit UTC offset (i.e. this is a floating value), the time is stored internally in terms of UTC.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public abstract class AbstractURFDateTime extends Date implements URFTemporal
{

	/**The year.*/
	private final int year;

		/**@return The year.*/
		public final int getYear() {return year;}

	/**The month.*/
	private final int month;

		/**@return The month.*/
		public final int getMonth() {return month;}

	/**The day.*/
	private final int day;

		/**@return The day.*/
		public final int getDay() {return day;}

	/**The hours.*/
	private final int hours;

		/**@return The hours.*/
		public final int getHours() {return hours;}

	/**The minutes.*/
	private final int minutes;

		/**@return The minutes.*/
		public final int getMinutes() {return minutes;}

	/**The seconds.*/
	private final int seconds;

		/**@return The seconds.*/
		public final int getSeconds() {return seconds;}

	/**The microseconds.*/
	private final int microseconds;

		/**@return The microseconds.*/
		public final int getMicroseconds() {return microseconds;}

	/**The UTC offset, or <code>null</code> if no UTC offset is known.*/
	private final URFUTCOffset utcOffset;

		/**@return The UTC offset, or <code>null</code> if no UTC offset is known.*/
		public URFUTCOffset getUTCOffset() {return utcOffset;}

	/**Full Constructor.
	@param year The year, 0-9999.
	@param month The month, 1-12.
	@param day The day, 1-31.
	@param hours The hours, 0-23.
	@param minutes The minutes, 0-59.
	@param seconds The seconds, 0-59.
	@param microseconds The microseconds, 0-999999
	@param utcOffset The UTC offset, or <code>null</code> if no UTC offset is known.
	*/
	protected AbstractURFDateTime(final int year, final int month, final int day, final int hours, final int minutes, final int seconds, final int microseconds, final URFUTCOffset utcOffset)	//TODO validate the ranges, or at least the minimum bounds
	{
		super(URFTemporalComponents.createCalendar(year, month, day, hours, minutes, seconds, microseconds, utcOffset).getTimeInMillis());	//construct the parent class with the time in milleconds the given information represents
		this.year=year;	//save the given information
		this.month=month;
		this.day=day;
		this.hours=hours;
		this.minutes=minutes;
		this.seconds=seconds;
		this.microseconds=microseconds;
		this.utcOffset=utcOffset;
	}

}
