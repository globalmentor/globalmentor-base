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

package com.globalmentor.itu;

import java.math.*;
import java.text.NumberFormat;
import java.util.Locale;

import static com.globalmentor.java.Objects.*;

/**A unit of the International System of Units codified by ISO 30 and related units.
For consistency this enum considers the unit for mass to be "gram" rather than "kilogram".
@author Garret Wilson
@see <a href="http://en.wikipedia.org/wiki/International_System_of_Units">International System of Units</a>
@see <a href="http://en.wikipedia.org/wiki/SI_derived_unit">SI derived unit</a>
@see <a href="http://en.wikipedia.org/wiki/Non-SI_units_accepted_for_use_with_SI">Non-SI units accepted for use with SI</a>
*/
public enum SIUnit	//TODO units at add http://en.wikipedia.org/wiki/SI_derived_unit and http://en.wikipedia.org/wiki/Non-SI_units_accepted_for_use_with_SI
{

	/**Unit for length.*/
	METRE("m"),
	
	/**Unit for mass.*/
	GRAM("g"),

	/**Unit for time.*/
	SECOND("s"),

	/**Unit for electric current.*/
	AMPERE("A"),

	/**Unit for thermodynamic temperature.*/
	KELVIN("K"),

	/**Unit for amount of substance.*/
	MOLE("mol"),

	/**Unit for luminous intensity.*/
	CANDELA("cd"),

	/**Unit for computer memory.*/
	BYTE("B"),

	/**Unit for computer byte subdivision.*/
	BIT("b");

		/**A prefix of the International System of Units codified by ISO 30.
		The prefixes appear in the order of smallest to largest factor.
		@author Garret Wilson
		@see <a href="http://en.wikipedia.org/wiki/SI_prefixes">SI prefix</a>
		@see <a href="http://en.wikipedia.org/wiki/International_System_of_Units">International System of Units</a>
		*/
		public enum Prefix
		{
	
			YOCTO("y", -24),
			ZEPTO("z", -21),
			ATTO("a", -18),
			FEMTO("f", -15),
			PICO("p", -12),
			NANO("n", -9),
			MICRO(String.valueOf((char)0x03bc), -6),
			MILLI("m", -3),
			CENTI("c", -2),
			DECI("d", -1),
			NONE("", 0),
			DECA("da", 1),
			HECTO("h", 2),
			KILO("k", 3),
			MEGA("M", 6),
			GIGA("G", 9),
			TERA("T", 12),
			PETA("P", 15),
			EXA("E", 18),
			ZETTA("Z", 21),
			YOTTA("Y", 24);
	
			/**The prefix symbol.*/
			private final String symbol;
	
				/**@return The prefix symbol.*/
				public String getSymbol() {return symbol;}
	
			/**The power of the prefix factor with 10 as the base.*/
			private final int factorPower;
				
				/**@return The power of the prefix factor with 10 as the base.*/
				public int getFactorPower() {return factorPower;}
	
			/**The power of the prefix.*/
			private final BigDecimal factor;
				
				/**@return The power of the prefix.*/
				public BigDecimal getFactor() {return factor;}
	
			/**Symbol and factor power constructor.
			@param symbol The prefix symbol.
			@param factorPower The power of the prefix factor with 10 as the base.
			@exception NullPointerException if the given symbol is <code>null</code>.
			*/
			private Prefix(final String symbol, final int factorPower)
			{
				this.symbol=checkInstance(symbol, "Symbol cannot be null.");	//save the symbol
				this.factorPower=factorPower;	//save the base 10 power of the factor
				this.factor=factorPower>=0 ? BigDecimal.TEN.pow(factorPower) : BigDecimal.ONE.divide(BigDecimal.TEN.pow(-factorPower));	//calculate the factor, compensating for BigDecimal's not supporting negative powers
			}
	
			/**Returns a string representation of the prefix.
			This version returns the prefix symbol.
			@see #getSymbol()
			*/
			public String toString()
			{
				return getSymbol();
			}
		}

	/**The unit symbol, or <code>null</code> if this unit has no symbol.*/
	private final String symbol;

		/**@return The unit symbol, or <code>null</code> if this unit has no symbol.*/
		public String getSymbol() {return symbol;}

	/**Symbol constructor.
	@param symbol The unit symbol, or <code>null</code> if this unit has no symbol.
	*/
	private SIUnit(final String symbol)
	{
		this.symbol=symbol;
	}

	/**Returns a string representation of the unit.
	This version returns the unit symbol.
	@see #getSymbol()
	*/
	public String toString()
	{
		return getSymbol();
	}

	/**Formats the given value in the default locale for this unit.
	@param value The value to format.
	@return A string representation of the given value in the default local in this unit with the highest available prefix.
	*/
	public String format(final BigDecimal value)
	{
		return format(value, Locale.getDefault());	//format using the default locale
	}

	/**Formats the given value in the given locale for this unit.
	@param value The value to format.
	@param locale The locale used for formatting.
	@return A string representation of the given value in the given local in this unit with the highest available prefix.
	*/
	public String format(final BigDecimal value, final Locale locale)
	{
		return format(value, locale, null);	//format with the no minimum prefix
	}

	/**Formats the given value in the default locale for this unit.
	If a minimum prefix is given and the appropriate prefix is smaller than the given prefix, a prefix of {@link Prefix#NONE} is used.
	For example, formatting a {@link #METRE} quantity of -100 would yield "1cm",
	but a {@link #BYTE} quantity of 100 with a minimum prefix of {@link Prefix#KILO} would yield "100b". 
	@param value The value to format.
	@param minPrefix The minimum prefix to use without resorting to {@link Prefix#NONE}, or <code>null</code> if there is no minimum prefix.
	@return A string representation of the given value in the default local in this unit with the highest available prefix.
	*/
	public String format(final BigDecimal value, final Prefix minPrefix)
	{
		return format(value, Locale.getDefault(), minPrefix);	//format using the default locale
	}

	/**Formats the given value in the given locale for this unit.
	If a minimum prefix is given and the appropriate prefix is smaller than the given prefix, a prefix of {@link Prefix#NONE} is used.
	For example, formatting a {@link #METRE} quantity of -100 would yield "1cm",
	but a {@link #BYTE} quantity of 100 with a minimum prefix of {@link Prefix#KILO} would yield "100b". 
	@param value The value to format.
	@param locale The locale used for formatting.
	@param minPrefix The minimum prefix to use without resorting to {@link Prefix#NONE}, or <code>null</code> if there is no minimum prefix.
	@return A string representation of the given value in the given local in this unit with the highest available prefix.
	*/
	public String format(final BigDecimal value, final Locale locale, final Prefix minPrefix) //TODO fix to find the highest absolute prefix
	{
		final Prefix[] prefixes=Prefix.values();	//get the prefix values
		int prefixIndex=prefixes.length-1;
		Prefix prefix=prefixes[prefixIndex];
		while(prefixIndex>0 && prefix.getFactor().compareTo(value)>0)	//look for the first prefix whose factor is less than the given value
		{
			prefix=prefixes[--prefixIndex];	//look at the previous prefix
		}
		if(minPrefix!=null && prefix.ordinal()<minPrefix.ordinal())	//if there is a minimum prefix and we're below it
		{
			prefix=Prefix.NONE;	//don't use a prefix
		}
		final NumberFormat numberFormat=NumberFormat.getNumberInstance(locale);	//create a number formatter
		numberFormat.setMaximumFractionDigits(2);	//use at most two fraction digits
		numberFormat.setRoundingMode(RoundingMode.DOWN);	//always round down so that the new rounded value will not change the prefix used
		return numberFormat.format(value.divide(prefix.getFactor()))+prefix.getSymbol()+getSymbol();	//divide the value by the factor and append the prefix and unit
	}
}
