/*
 * Copyright © 1996-2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.event;

import java.util.EventObject;

/**
 * An event used to notify interested parties that progress has been made.
 * @author Garret Wilson
 * @see ProgressListener
 */
public class ProgressEvent extends EventObject
{

	/** The amount of recent progress, or <code>-1</code> if not known. */
	private final long delta;

	/** @return The amount of recent progress, or <code>-1</code> if not known. */
	public long getDelta()
	{
		return delta;
	}

	/** The total progress to this point, or <code>-1</code> if not known. */
	private final long value;

	/** @return The total progress to this point, or <code>-1</code> if not known. */
	public long getValue()
	{
		return value;
	}

	/** The goal, or <code>-1</code> if not known. */
	private final long maximum;

	/** @return The goal, or <code>-1</code> if not known. */
	public long getMaximum()
	{
		return maximum;
	}

	/**
	 * Value constructor.
	 * @param source The object on which the event initially occurred.
	 * @param value The total progress to this point, or <code>-1</code> if not known.
	 */
	public ProgressEvent(final Object source, final long value)
	{
		this(source, value, -1);
	}

	/**
	 * Value and maximum constructor.
	 * @param source The object on which the event initially occurred.
	 * @param value The total progress to this point, or <code>-1</code> if not known.
	 * @param maximum The goal, or <code>-1</code> if not known.
	 */
	public ProgressEvent(final Object source, final long value, final long maximum)
	{
		this(source, -1, value, maximum);
	}

	/**
	 * Delta, value, and maximum constructor.
	 * @param source The object on which the event initially occurred.
	 * @param delta The amount of recent progress, or <code>-1</code> if not known.
	 * @param value The total progress to this point, or <code>-1</code> if not known.
	 * @param maximum The goal, or <code>-1</code> if not known.
	 */
	public ProgressEvent(final Object source, final long delta, final long value, final long maximum)
	{
		super(source); //let the parent class initialize
		this.delta = delta;
		this.value = value;
		this.maximum = maximum;
	}

	/**@return A string representing a progress bar.*/
	public String getProgressBarString()
	{
		
	}

	/**{@inheritDoc}
	 * This implementation returns a string indicating the current progress in the form <code>123/1000</code>.
	 */
	public String toString()
	{
		final StringBuilder stringBuilder=new StringBuilder();
		if(value>=0)
		{
			stringBuilder.append(value);
		}
		if(maximum>=0)
		{
			stringBuilder.append('/').append(maximum);
		}
		return stringBuilder.toString();
	}
}
