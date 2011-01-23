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

package com.globalmentor.math;

/**Represents a numerator and a denominator.
@author Garret Wilson
@param <N> The numerator type of the fraction.
@param <D> The denominator type of the fraction.
*/
public class Fraction<N extends Number, D extends Number>
{

	/**The numerator of the fraction.*/
	private final N numerator;

		/**@return The numerator of the fraction.*/
		public N getNumerator() {return numerator;}

	/**The denominator of the fraction.*/
	private final D denominator;

		/**@return The denominator of the fraction.*/
		public D getDenominator() {return denominator;}

	/**Constructor
	@param numerator The numerator of the fraction.
	@param denominator The denominator of the fraction.
	*/
	public Fraction(final N numerator, final D denominator)
	{
		this.numerator=numerator;
		this.denominator=denominator;
	}
	
	/**@return A string representation of the fraction in the form <var>numerator</var>/<var>denominator</var>.*/
	public String toString()
	{
		return getNumerator().toString()+"/"+getDenominator().toString();	//don't format the numbers; this general method lets each number format itself
	}
}
