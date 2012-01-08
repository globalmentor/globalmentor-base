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

package com.globalmentor.text.directory.vcard;

import static com.globalmentor.collections.Sets.*;
import static com.globalmentor.text.TextFormatter.*;

import java.util.EnumSet;
import java.util.Set;

import com.globalmentor.itu.*;
import com.globalmentor.text.ArgumentSyntaxException;

/**
 * An object representing the "TEL" type of a vCard <code>text/directory</code> profile as defined in <a href="http://www.ietf.org/rfc/rfc2426.txt">RFC
 * 2426</a>, "vCard MIME Directory Profile".
 * @author Garret Wilson
 */
public class Telephone extends TelephoneNumber
{

	/**
	 * The type of telephone.
	 * @author Garret Wilson
	 */
	public enum Type
	{
		/** A telephone number associated with a residence. */
		HOME,
		/** A telephone number that has voice messaging support. */
		MSG,
		/** A telephone number associated with a place of work. */
		WORK,
		/** A preferred-use telephone number. */
		PREF,
		/** A voice telephone number. */
		VOICE,
		/** A facsimile telephone number. */
		FAX,
		/** A cellular telephone number. */
		CELL,
		/** A video conferencing telephone number. */
		VIDEO,
		/** A paging device telephone number. */
		PAGER,
		/** A bulletin board system telephone number. */
		BBS,
		/** A modem-connected telephone number. */
		MODEM,
		/** A car-phone telephone number. */
		CAR,
		/** An ISDN service telephone number. */
		ISDN,
		/** A personal communication services telephone number. */
		PCS;
	}

	/** The default telephone type. */
	public final static Type DEFAULT_TYPE = Type.VOICE;

	/** The intended use. */
	private final Set<Type> types;

	/** @return The intended uses. */
	public Set<Type> getTypes()
	{
		return types;
	}

	/**
	 * Telephone number and type constructor.
	 * @param telephoneNumber The telephone number from which values should be used for initialization.
	 * @param types The intended use.
	 * @exception ArgumentSyntaxException Thrown if the values violate ITU-T E.164.
	 */
	public Telephone(final TelephoneNumber telephoneNumber, final Set<Type> types) throws ArgumentSyntaxException
	{
		this(telephoneNumber.toString(), types); //TODO check
	}

	/**
	 * Full constructor from separate telephone number components.
	 * @param cc The country code for geographic areas.
	 * @param ndc The national destination code
	 * @param sn The subscriber number.
	 * @param types The intended use.
	 * @exception ArgumentSyntaxException Thrown if the values violate ITU-T E.164.
	 */
	public Telephone(final String cc, final String ndc, final String sn, final Set<Type> types) throws ArgumentSyntaxException
	{
		super(cc, ndc, sn); //construct the parent class
		this.types = immutableSetOf(types); //set the telephone types
	}

	/**
	 * Constructs a telephone by parsing the given string and assigning a telephone type. Expects the country code to begin with '+' and accepts code field
	 * delimiters of '-' and ' '.
	 * <p>
	 * This constructor uses a default type of {@value Telephone#DEFAULT_TYPE}.
	 * </p>
	 * @param string The string to be parsed into a telephone number.
	 * @exception ArgumentSyntaxException Thrown if the value violates ITU-T E.164.
	 */
	public Telephone(final String string) throws ArgumentSyntaxException
	{
		this(string, EnumSet.of(DEFAULT_TYPE));
	}

	/**
	 * Constructs a telephone by parsing the given string and assigning a telephone type. Expects the country code to begin with '+' and accepts code field
	 * delimiters of '-' and ' '.
	 * @param string The string to be parsed into a telephone number.
	 * @param types The intended use.
	 * @exception ArgumentSyntaxException Thrown if the value violates ITU-T E.164.
	 */
	public Telephone(final String string, final Set<Type> types) throws ArgumentSyntaxException
	{
		super(string); //construct the parent class
		this.types = immutableSetOf(types); //set the telephone types
	}

	/** @return A string to represent the telephone type. */
	public String getTypeString()
	{
		return getTypeString(getTypes()); //return a string for our telephone type
	}

	/**
	 * Constructs a string to represent the given telephone types.
	 * @param types The intended use.
	 */
	public static String getTypeString(final Set<Type> types)
	{
		return formatList(',', types);
	}

	/** @return A string representation of the telephone. */
	public String toString()	//TODO remove this, so that the telephone always returns just the telephone number; make super method final
	{
		return super.toString() + " (" + getTypeString() + ")"; //return the telephone type appended to the telephone number 
	}
}
