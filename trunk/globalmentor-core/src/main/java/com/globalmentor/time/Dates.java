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

package com.globalmentor.time;

import java.util.Date;
import java.util.GregorianCalendar;

/**
 * Utilities for dealing with dates.
 * @author Garret Wilson
 */
public class Dates {

	/** The origin of Microsoft Excel date serial values, 31 December 1899. */
	protected static final Date EXCEL_EPOCH_DATE = new GregorianCalendar(1899, GregorianCalendar.DECEMBER, 31).getTime();

	/**
	 * Converts a Java date to Microsoft Excel representation, defined as the fractional number of days past 31 December 1899.
	 * @param date The date to convert.
	 * @return A Microsoft Excel representation
	 */
	public static double toExcelDate(final Date date) {
		//TODO 		final Calendar calendar=new GregorianCalendar();	//create a calendar
		final long dayLength = 1000 * 60 * 60 * 24; //TODO does this take into account leap seconds and such?
		final long excelTime = date.getTime() - EXCEL_EPOCH_DATE.getTime(); //convert from the Java epoch to the Excel epoch TODO should we simply store the Excel milliseconds in our constant?
		final long excelDays = excelTime / dayLength + 1; //get the number of days TODO does this take into account leap seconds and such? and why do we have to add one?
		final long excelDaysRemainder = excelTime % dayLength; //get remainder milliseconds TODO does this take into account leap seconds and such?
		return excelDays + ((double)excelDaysRemainder / (double)dayLength); //return the fractional number of Excel days
		/*TODO fix or del
				//get the start of the day
				final Calendar dayCalendar=new GregorianCalendar(calendar.get(Calendar.YEAR), calendar.get(Calendar.MONTH), calendar.get(Calendar.DATE)).getTime()
				final long dayStart=dayStartCalendar.getTimeInMillis();	//find the start of the day
				dayCalendar.add(Calendar.DATE, 1);	//go to the start of the next day
				final long dayEnd=dayStartCalendar.getTimeInMillis();	//find the end of the current day
				final long dayLength=dayEnd-dayStart;	//find the length of the day in milliseconds
				final Date dayEnd=
				calendar.setTime(date);	//set the calendar to the given date
		*/
	}
}
