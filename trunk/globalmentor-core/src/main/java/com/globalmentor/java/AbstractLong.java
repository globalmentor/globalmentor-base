/*
 * Copyright © 2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.java;

/**
 * Abstract base class for number that represent long values.
 * 
 * <p>
 * This class provides an {@link #equals(Object)} implementation that requires the compared object be an instance of the concrete subclass of this class.
 * </p>
 * 
 * @param <L> The type of long represented.
 * 
 * @author Garret Wilson
 */
public abstract class AbstractLong<L extends Number> extends Number implements Comparable<L>
{

	/** {@inheritDoc} This version delegates to {@link #longValue()}. */
	@Override
	public int intValue()
	{
		return (int)longValue();
	}

	/** {@inheritDoc} This version delegates to {@link #doubleValue()}. */
	@Override
	public float floatValue()
	{
		return (float)doubleValue();
	}

	/** {@inheritDoc} This version delegates to {@link #longValue()}. */
	@Override
	public double doubleValue()
	{
		return longValue();
	}

	/** {@inheritDoc} This returns a hash code of {@link #longValue()}. */
	@Override
	public int hashCode()
	{
		return Longs.hashCode(longValue());
	}

	/** {@inheritDoc} This compares the value of {@link #longValue()} if the given object is an instance of the concrete subclass of this class. */
	@Override
	public boolean equals(final Object object)
	{
		if(object == this)
		{
			return true;
		}
		if(!(getClass().isInstance(object)))
		{
			return false;
		}
		return longValue() == ((Number)object).longValue();
	}

	@Override
	public int compareTo(final L l)
	{
		return Longs.compare(longValue(), l.longValue());
	}

}
