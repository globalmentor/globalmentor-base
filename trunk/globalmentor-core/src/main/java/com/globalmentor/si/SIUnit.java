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

package com.globalmentor.si;

import java.math.*;
import java.text.NumberFormat;
import java.util.Locale;

/**
 * A unit of the International System of Units codified by ISO 30 and related units. For consistency this enum considers the unit for mass to be "gram" rather
 * than "kilogram".
 * @author Garret Wilson
 * @see <a href="http://en.wikipedia.org/wiki/International_System_of_Units">International System of Units</a>
 * @see <a href="http://en.wikipedia.org/wiki/SI_derived_unit">SI derived unit</a>
 * @see <a href="http://en.wikipedia.org/wiki/Non-SI_units_accepted_for_use_with_SI">Non-SI units accepted for use with SI</a>
 */
public enum SIUnit //TODO add units at http://en.wikipedia.org/wiki/SI_derived_unit and http://en.wikipedia.org/wiki/Non-SI_units_accepted_for_use_with_SI
{

	/** Unit for length. */
	METRE("m"),

	/** Unit for mass. */
	GRAM("g"),

	/** Unit for time. */
	SECOND("s"),

	/** Unit for electric current. */
	AMPERE("A"),

	/** Unit for thermodynamic temperature. */
	KELVIN("K"),

	/** Unit for amount of substance. */
	MOLE("mol"),

	/** Unit for luminous intensity. */
	CANDELA("cd"),

	/** Unit for computer memory. */
	BYTE("B"),

	/** Unit for computer byte subdivision. */
	BIT("b");

	/** The unit symbol, or <code>null</code> if this unit has no symbol. */
	private final String symbol;

	/** @return The unit symbol, or <code>null</code> if this unit has no symbol. */
	public String getSymbol()
	{
		return symbol;
	}

	/**
	 * Symbol constructor.
	 * @param symbol The unit symbol, or <code>null</code> if this unit has no symbol.
	 */
	private SIUnit(final String symbol)
	{
		this.symbol = symbol;
	}

	/**
	 * Returns a string representation of the unit. This version returns the unit symbol.
	 * @see #getSymbol()
	 */
	public String toString()
	{
		return getSymbol();
	}

	/**
	 * Formats the given value in the default locale for this unit.
	 * <p>
	 * Rounding is performed down so as not to cause discrepancies between the value and the determined prefix.
	 * </p>
	 * @param value The value to format.
	 * @return A string representation of the given value in the default local in this unit with the highest available prefix.
	 */
	public String format(final long value)
	{
		return format(value, Locale.getDefault()); //format using the default locale
	}

	/**
	 * Formats the given value in the given locale for this unit.
	 * <p>
	 * Rounding is performed down so as not to cause discrepancies between the value and the determined prefix.
	 * </p>
	 * @param value The value to format.
	 * @param locale The locale used for formatting.
	 * @return A string representation of the given value in the given local in this unit with the highest available prefix.
	 */
	public String format(final long value, final Locale locale)
	{
		return format(value, locale, null); //format with the no minimum prefix
	}

	/**
	 * Formats the given value in the default locale for this unit.
	 * <p>
	 * If a minimum prefix is given and the appropriate prefix is smaller than the given prefix, a prefix of {@link SIPrefix#NONE} is used. For example,
	 * formatting a {@link #METRE} quantity of -100 would yield "1cm", but a {@link #BYTE} quantity of 100 with a minimum prefix of {@link SIPrefix#KILO} would
	 * yield "100b".
	 * </p>
	 * <p>
	 * Rounding is performed down so as not to cause discrepancies between the value and the determined prefix.
	 * </p>
	 * @param value The value to format.
	 * @param minPrefix The minimum prefix to use without resorting to {@link SIPrefix#NONE}, or <code>null</code> if there is no minimum prefix.
	 * @return A string representation of the given value in the default local in this unit with the highest available prefix.
	 */
	public String format(final long value, final SIPrefix minPrefix)
	{
		return format(value, Locale.getDefault(), minPrefix); //format using the default locale
	}

	/**
	 * Formats the given value in the given locale for this unit.
	 * <p>
	 * If a minimum prefix is given and the appropriate prefix is smaller than the given prefix, a prefix of {@link SIPrefix#NONE} is used. For example,
	 * formatting a {@link #METRE} quantity of -100 would yield "1cm", but a {@link #BYTE} quantity of 100 with a minimum prefix of {@link SIPrefix#KILO} would
	 * yield "100b".
	 * </p>
	 * <p>
	 * Rounding is performed down so as not to cause discrepancies between the value and the determined prefix.
	 * </p>
	 * @param value The value to format.
	 * @param locale The locale used for formatting.
	 * @param minPrefix The minimum prefix to use without resorting to {@link SIPrefix#NONE}, or <code>null</code> if there is no minimum prefix.
	 * @return A string representation of the given value in the given local in this unit with the highest available prefix.
	 */
	public String format(final long value, final Locale locale, final SIPrefix minPrefix)
	{
		return format(BigDecimal.valueOf(value), locale, minPrefix);
	}

	/**
	 * Formats the given value in the default locale for this unit.
	 * <p>
	 * Rounding is performed down so as not to cause discrepancies between the value and the determined prefix.
	 * </p>
	 * @param value The value to format.
	 * @return A string representation of the given value in the default local in this unit with the highest available prefix.
	 */
	public String format(final BigDecimal value)
	{
		return format(value, Locale.getDefault()); //format using the default locale
	}

	/**
	 * Formats the given value in the given locale for this unit.
	 * <p>
	 * Rounding is performed down so as not to cause discrepancies between the value and the determined prefix.
	 * </p>
	 * @param value The value to format.
	 * @param locale The locale used for formatting.
	 * @return A string representation of the given value in the given local in this unit with the highest available prefix.
	 */
	public String format(final BigDecimal value, final Locale locale)
	{
		return format(value, locale, null); //format with the no minimum prefix
	}

	/**
	 * Formats the given value in the default locale for this unit.
	 * <p>
	 * If a minimum prefix is given and the appropriate prefix is smaller than the given prefix, a prefix of {@link SIPrefix#NONE} is used. For example,
	 * formatting a {@link #METRE} quantity of -100 would yield "1cm", but a {@link #BYTE} quantity of 100 with a minimum prefix of {@link SIPrefix#KILO} would
	 * yield "100b".
	 * </p>
	 * <p>
	 * Rounding is performed down so as not to cause discrepancies between the value and the determined prefix.
	 * </p>
	 * @param value The value to format.
	 * @param minPrefix The minimum prefix to use without resorting to {@link SIPrefix#NONE}, or <code>null</code> if there is no minimum prefix.
	 * @return A string representation of the given value in the default local in this unit with the highest available prefix.
	 */
	public String format(final BigDecimal value, final SIPrefix minPrefix)
	{
		return format(value, Locale.getDefault(), minPrefix); //format using the default locale
	}

	/**
	 * Formats the given value in the given locale for this unit.
	 * <p>
	 * If a minimum prefix is given and the appropriate prefix is smaller than the given prefix, a prefix of {@link SIPrefix#NONE} is used. For example,
	 * formatting a {@link #METRE} quantity of -100 would yield "1cm", but a {@link #BYTE} quantity of 100 with a minimum prefix of {@link SIPrefix#KILO} would
	 * yield "100b".
	 * </p>
	 * <p>
	 * Rounding is performed down so as not to cause discrepancies between the value and the determined prefix.
	 * </p>
	 * @param value The value to format.
	 * @param locale The locale used for formatting.
	 * @param minPrefix The minimum prefix to use without resorting to {@link SIPrefix#NONE}, or <code>null</code> if there is no minimum prefix.
	 * @return A string representation of the given value in the given local in this unit with the highest available prefix.
	 */
	public String format(final BigDecimal value, final Locale locale, final SIPrefix minPrefix) //TODO fix to find the highest absolute prefix
	{
		final SIPrefix[] prefixes = SIPrefix.values(); //get the prefix values
		int prefixIndex = prefixes.length - 1;
		SIPrefix prefix = prefixes[prefixIndex];
		while(prefixIndex > 0 && prefix.getFactor().compareTo(value) > 0) //look for the first prefix whose factor is less than the given value
		{
			prefix = prefixes[--prefixIndex]; //look at the previous prefix
		}
		if(minPrefix != null && prefix.ordinal() < minPrefix.ordinal()) //if there is a minimum prefix and we're below it
		{
			prefix = SIPrefix.NONE; //don't use a prefix
		}
		final NumberFormat numberFormat = NumberFormat.getNumberInstance(locale); //create a number formatter
		numberFormat.setMaximumFractionDigits(2); //use at most two fraction digits
		numberFormat.setRoundingMode(RoundingMode.DOWN); //always round down so that the new rounded value will not change the prefix used
		return numberFormat.format(value.divide(prefix.getFactor())) + prefix.getSymbol() + getSymbol(); //divide the value by the factor and append the prefix and unit
	}
}
