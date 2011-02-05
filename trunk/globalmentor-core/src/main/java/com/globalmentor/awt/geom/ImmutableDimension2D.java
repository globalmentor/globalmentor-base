/*
 * Copyright © 2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.awt.geom;

import java.awt.geom.Dimension2D;

import com.globalmentor.java.Objects;

/**
 * Immutable class for storing a 2D dimension as a read-only value.
 * 
 * @author Garret Wilson
 * 
 */
public final class ImmutableDimension2D extends Dimension2D
{

	private static final long serialVersionUID = 1L;

	/** The width of the dimension in double precision. */
	private final double width;

	/** The height of the dimension in double precision. */
	private final double height;

	/** {@inheritDoc} */
	public double getWidth()
	{
		return width;
	}

	/** {@inheritDoc} */
	public double getHeight()
	{
		return height;
	}

	/**
	 * Creates a dimension with a with of zero and a height of zero.
	 */
	public ImmutableDimension2D()
	{
		this(0, 0);
	}

	/**
	 * Creates a dimension with the same width and height as the given dimension.
	 * @param dimension The dimension the values of which will be copied.
	 */
	public ImmutableDimension2D(final Dimension2D dimension)
	{
		this(dimension.getWidth(), dimension.getHeight());
	}

	/**
	 * Constructs a dimension initialized with the specified width and specified height.
	 * @param width The specified width.
	 * @param height The specified height.
	 */
	public ImmutableDimension2D(final double width, final double height)
	{
		this.width = width;
		this.height = height;
	}

	/**
	 * {@inheritDoc} This version throws an exception, as this class is immutable.
	 * @throws UnsupportedOperationException to indicate that this value class is immutable.
	 */
	@Override
	public void setSize(double width, double height)
	{
		throw new UnsupportedOperationException("Class " + getClass() + " is immutable.");
	}

	/**
	 * {@inheritDoc} This version throws an exception, as this class is immutable.
	 * @throws UnsupportedOperationException to indicate that this value class is immutable.
	 */
	@Override
	public void setSize(Dimension2D d)
	{
		throw new UnsupportedOperationException("Class " + getClass() + " is immutable.");
	}

	/** {@inheritDoc} */
	@Override
	public int hashCode()
	{
		return Objects.hashCode(width, height);
	}

	/**
	 * Determines if this object is equal to another object. The two objects are considered equal if they are both
	 * instances of {@link Dimension2D} with the same width and height.
	 */
	public boolean equals(final Object object)
	{
		if (!(object instanceof Dimension2D))
		{
			return false;
		}
		final Dimension2D dimension2D = (Dimension2D) object;
		return getWidth() == dimension2D.getWidth() && getHeight() == dimension2D.getHeight();
	}
}
