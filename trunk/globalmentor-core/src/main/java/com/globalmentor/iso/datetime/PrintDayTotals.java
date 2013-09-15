/*
 * Copyright © 2013 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.iso.datetime;

import static com.globalmentor.iso.datetime.ISODates.*;
import static com.globalmentor.java.Conditions.*;

import java.io.*;
import java.util.*;

import com.globalmentor.io.BOMInputStreamReader;
import com.globalmentor.model.*;

/**
 * A console application to print the totals of days overlapping some ranges. This is useful, for example, in calculating the number of days in a country for
 * compliance with visa restrictions.
 * 
 * <p>
 * <code>PrintDayTotals <var>date</var> <var>dayCount</var> [<var>maxDays</var>]</code>
 * </p>
 * 
 * <p>
 * Ranges are in the form <code><var>from</var>,<var>to</var></code>, e.g.:
 * </p>
 * 
 * <blockquote><code>2010-01-02,2010-01-05<br/>2010-03-10,2010-04-04</code></blockquote>
 * 
 * <p>
 * Output has three or four columns, depending on whether a maximum number of days was indicated:
 * <code><var>date</var>,<var>count</var>,<var>total</var>,<var>difference</var></code>, e.g.:
 * </p>
 * <blockquote><code>2013-02-18,1,136,44<br/>2013-02-19,1,136,44<br/>2013-02-20,0,135,45</code></blockquote> <h2>Examples:</h2>
 * 
 * <p>
 * Print totals from 2010-02-04 to 2011-02-03 from the file <code>ranges.txt</code>:
 * </p>
 * <blockquote><code>PrintDayTotals <var>2011-02-03</var> <var>365</var> &lt; ranges.txt</code></blockquote> <blockquote>
 * <p>
 * Print totals from 2010-02-04 to 2011-02-03 from the file <code>ranges.txt</code>, indicating the difference of each from 180:
 * </p>
 * <code>PrintDayTotals <var>2011-02-03</var> <var>365</var> <var>180</var> &lt; ranges.txt</code></blockquote>
 * 
 * @author Garret Wilson
 * 
 */
public class PrintDayTotals
{

	public static void main(final String[] args) throws UnsupportedEncodingException, IOException
	{
		checkArgument(args.length >= 2 && args.length <= 3, "Must have at least a date and number of days with an optional maximum number of days.");
		//parse the parameters
		final ISODate date = ISODate.valueOf(args[0]);
		final int dayCount = Integer.parseInt(args[1]);
		final long maxDays;
		if(args.length == 3)
		{
			maxDays = Long.parseLong(args[2]);
		}
		else
		{
			maxDays = -1;
		}
		//parse the ranges form System.in
		final Set<Range<ISODate>> ranges = new HashSet<Range<ISODate>>();
		@SuppressWarnings("resource")
		//we shouldn't close the input stream
		final LineNumberReader reader = new LineNumberReader(new BOMInputStreamReader(System.in));
		String line;
		while((line = reader.readLine()) != null)
		{
			final String[] lineComponents = line.split(",");
			checkArgument(lineComponents.length == 2, "Expected two components on line {0}: {1}", reader.getLineNumber(), line);
			ranges.add(new Range<ISODate>(ISODate.valueOf(lineComponents[0]), ISODate.valueOf(lineComponents[1]))); //parse and store the range
		}
		//count the days
		final Map<ISODate, Count> dayCounts = getDayCounts(ranges);
		//calculate the totals
		final Map<ISODate, Long> dayTotals = getDayTotals(date, dayCount, dayCounts);
		//print the results
		for(final Map.Entry<ISODate, Long> dayTotal : dayTotals.entrySet())
		{
			final ISODate day = dayTotal.getKey();
			final Count count = dayCounts.get(day);
			final long total = dayTotal.getValue().longValue();
			System.out.print(day + "," + (count != null ? count : "0") + "," + total); //e.g. 2011-02-03,1,170
			if(maxDays >= 0) //if we know the maximum number of days, include the days remaining
			{
				System.out.print("," + (maxDays - total)); //e.g. 2011-02-03,1,170,10
			}
			System.out.println();
		}
	}

}
