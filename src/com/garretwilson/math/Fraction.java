package com.garretwilson.math;

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
