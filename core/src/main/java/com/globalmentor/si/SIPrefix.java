/*
 * Copyright © 1996-2011 GlobalMentor, Inc. <https://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.si;

import java.math.BigDecimal;

import static java.util.Objects.*;

import com.globalmentor.model.Named;

/**
 * A prefix of the International System of Units codified by ISO 30. The prefixes appear in the order of smallest to largest factor.
 * @author Garret Wilson
 * @see <a href="http://en.wikipedia.org/wiki/SI_prefixes">SI prefix</a>
 * @see <a href="http://en.wikipedia.org/wiki/International_System_of_Units">International System of Units</a>
 */
public enum SIPrefix implements Named<String> {

	/** Prefix for septillionth. */
	YOCTO("yocto", "y", -24),
	/** Prefix for sextillionth. */
	ZEPTO("zepto", "z", -21),
	/** Prefix for quintillionth. */
	ATTO("atto", "a", -18),
	/** Prefix for quadrillionth. */
	FEMTO("femto", "f", -15),
	/** Prefix for trillionth. */
	PICO("pico", "p", -12),
	/** Prefix for billionth. */
	NANO("nano", "n", -9),
	/** Prefix for millionth. */
	MICRO("micro", String.valueOf((char)0x03bc), -6),
	/** Prefix for thousandth. */
	MILLI("milli", "m", -3),
	/** Prefix for hundredth. */
	CENTI("centi", "c", -2),
	/** Prefix for tenth. */
	DECI("deci", "d", -1),
	/** No prefix. */
	NONE("", "", 0),
	/** Prefix for ten. */
	DECA("deca", "da", 1),
	/** Prefix for hundred. */
	HECTO("hecto", "h", 2),
	/** Prefix for thousand. */
	KILO("kilo", "k", 3),
	/** Prefix for million. */
	MEGA("mega", "M", 6),
	/** Prefix for billion. */
	GIGA("giga", "G", 9),
	/** Prefix for trillion. */
	TERA("tera", "T", 12),
	/** Prefix for quadrillion. */
	PETA("peta", "P", 15),
	/** Prefix for quintillion. */
	EXA("exa", "E", 18),
	/** Prefix for sextillion. */
	ZETTA("zetta", "Z", 21),
	/** Prefix for septillion. */
	YOTTA("yotta", "Y", 24);

	/** The prefix name. */
	private final String name;

	/**
	 * Returns the prefix name.
	 * @return The prefix name.
	 */
	public String getName() {
		return name;
	}

	/** The prefix symbol. */
	private final String symbol;

	/**
	 * Returns the prefix symbol.
	 * @return The prefix symbol.
	 */
	public String getSymbol() {
		return symbol;
	}

	/** The power of the prefix factor with 10 as the base. */
	private final int factorPower;

	/**
	 * Returns the power of the prefix factor with 10 as the base.
	 * @return The power of the prefix factor with 10 as the base.
	 */
	public int getFactorPower() {
		return factorPower;
	}

	/** The power of the prefix. */
	private final BigDecimal factor;

	/**
	 * Returns the power of the prefix.
	 * @return The power of the prefix.
	 */
	public BigDecimal getFactor() {
		return factor;
	}

	/**
	 * Symbol and factor power constructor.
	 * @param name The prefix name.
	 * @param symbol The prefix symbol.
	 * @param factorPower The power of the prefix factor with 10 as the base.
	 * @throws NullPointerException if the given symbol is <code>null</code>.
	 */
	private SIPrefix(final String name, final String symbol, final int factorPower) {
		this.name = requireNonNull(name, "Name cannot be null."); //save the name
		this.symbol = requireNonNull(symbol, "Symbol cannot be null."); //save the symbol
		this.factorPower = factorPower; //save the base 10 power of the factor
		this.factor = factorPower >= 0 ? BigDecimal.TEN.pow(factorPower) : BigDecimal.ONE.divide(BigDecimal.TEN.pow(-factorPower)); //calculate the factor, compensating for BigDecimal's not supporting negative powers
	}

	/**
	 * Returns a string representation of the prefix. This version returns the prefix name.
	 * @see #getName()
	 */
	public String toString() {
		return getName();
	}

}
