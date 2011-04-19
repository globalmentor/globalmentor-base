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

import static com.globalmentor.java.Objects.*;
import static com.globalmentor.java.Strings.*;
import static java.util.Collections.*;

import java.util.*;

import com.globalmentor.collections.Sets;

/**
 * An object representing the "EMAIL" type of a vCard <code>text/directory</code> profile as defined in <a href="http://www.ietf.org/rfc/rfc2426.txt">RFC
 * 2426</a>, "vCard MIME Directory Profile".
 * @author Garret Wilson
 */
public class Email
{

	/**
	 * The type of email.
	 * @author Garret Wilson
	 */
	public enum Type
	{

		/** An Internet email address. */
		INTERNET,
		/** An X.400 addressing type. */
		X400,
		/** A preferred-use email address. */
		PREF
	}

	/** The default email type. */
	public final static Type DEFAULT_TYPE = Type.INTERNET;

	/** The email address. */
	private final String address;

	/** @return The email address. */
	public String getAddress()
	{
		return address;
	}

	/** The email addressing type. */
	private final Set<Type> types;

	/** @return The email addressing type. */
	public Set<Type> getTypes()
	{
		return unmodifiableSet(types);
	}

	/** The locale that represents the language of the text, or <code>null</code> if no language is indicated. */
	private final Locale locale;

	/** @return The locale that represents the language of the text, or <code>null</code> if no language is indicated. */
	public Locale getLocale()
	{
		return locale;
	}

	/**
	 * Email address constructor with default telephone type of {@value #DEFAULT_TYPE}.
	 * @param address The email address.
	 * @throws NullPointerException if the address and/or types is <code>null</code>.
	 */
	public Email(final String address)
	{
		this(address, EnumSet.of(DEFAULT_TYPE)); //construct an email with the default email type
	}

	/**
	 * Email address and types constructor.
	 * @param address The email address.
	 * @param types The email addressing types.
	 * @throws NullPointerException if the address and/or types is <code>null</code>.
	 */
	public Email(final String address, final Set<Type> types)
	{
		this(address, types, null); //construct an email with no locale	
	}

	/**
	 * Email address, types, and locale constructor.
	 * @param address The email address.
	 * @param types The email addressing types.
	 * @param locale The locale that represents the language of the text, or <code>null</code> if no language should be indicated.
	 * @throws NullPointerException if the address and/or types is <code>null</code>.
	 */
	public Email(final String address, final Set<Type> types, final Locale locale)
	{
		this.address = checkInstance(address, "Email address cannot be null.");
		this.types = Sets.immutableSetOf(types);
		this.locale = locale;
	}

	/** @return A string to represent the email addressing type. */
	public String getEmailTypeString()
	{
		return getEmailTypeString(getTypes()); //return a string for our email type
	}

	/**
	 * Constructs a string to represent the given email types.
	 * @param types The email addressing types.
	 */
	public static String getEmailTypeString(final Set<Type> types)
	{
		return concat(types, ",");
	}

	/** @return A string representation of the email address. */
	public String toString()
	{
		return getAddress() + " (" + getEmailTypeString() + ")"; //return the telephone type appended to the telephone number 
	}
}
