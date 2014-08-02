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

import java.util.*;
import com.globalmentor.model.LocaledText;

/**
 * An object representing the "LABEL" type of a vCard <code>text/directory</code> profile as defined in <a href="http://www.ietf.org/rfc/rfc2426.txt">RFC
 * 2426</a>, "vCard MIME Directory Profile".
 * @author Garret Wilson
 */
public class Label extends LocaledText
{

	/** The delivery address types. */
	private final Set<Address.Type> addressTypes;

	/** @return The delivery address types. */
	public Set<Address.Type> getAddressTypes()
	{
		return addressTypes;
	}

	/** Default constructor. */
	public Label()
	{
		this(""); //construct a default label with no information
	}

	/**
	 * Text constructor with no locale specified and default address types of {@value Address#DEFAULT_TYPES}.
	 * @param text The non-<code>null</code> text string to represent.
	 * @throws NullPointerException Thrown if <code>text</code> is <code>null</code>.
	 */
	public Label(final String text) throws NullPointerException
	{
		this(text, (Locale)null); //create label with no locale
	}

	/**
	 * Constructor with default address types of {@value Address#DEFAULT_TYPES}.
	 * @param localeText The non-<code>null</code> object containing the text and optional locale.
	 * @throws NullPointerException Thrown if <code>localeText</code> is <code>null</code>.
	 */
	public Label(final LocaledText localeText) throws NullPointerException
	{
		this(localeText, Address.DEFAULT_TYPES); //construct a label with the default address type
	}

	/**
	 * Constructor with default address types of {@value Address#DEFAULT_TYPES}.
	 * @param text The non-<code>null</code> text string to represent.
	 * @param locale The locale that represents the language of the text, or <code>null</code> if no language should be indicated.
	 * @throws NullPointerException Thrown if <code>text</code> is <code>null</code>.
	 */
	public Label(final String text, final Locale locale) throws NullPointerException
	{
		this(text, Address.DEFAULT_TYPES, locale); //construct a label with the default address type
	}

	/**
	 * Address type constructor with no locale.
	 * @param text The non-<code>null</code> text string to represent.
	 * @param addressTypes The delivery address types.
	 * @throws NullPointerException Thrown if <code>text</code> and/or the types is <code>null</code>.
	 */
	public Label(final String text, final Set<Address.Type> addressTypes) throws NullPointerException
	{
		this(text, addressTypes, null); //construct a label with the given address types and no locale
	}

	/**
	 * Full locale text constructor
	 * @param localeText The non-<code>null</code> object containing the text and optional locale.
	 * @param addressTypes The delivery address types.
	 * @throws NullPointerException Thrown if <code>localeText</code> and/or types is <code>null</code>.
	 */
	public Label(final LocaledText localeText, final Set<Address.Type> addressTypes) throws NullPointerException
	{
		super(localeText); //construct the locale text
		this.addressTypes = immutableSetOf(addressTypes);
	}

	/**
	 * Full constructor
	 * @param text The non-<code>null</code> text string to represent.
	 * @param addressTypes The delivery address types.
	 * @param locale The locale that represents the language of the text, or <code>null</code> if no language should be indicated.
	 * @throws NullPointerException Thrown if <code>text</code> and/or types is <code>null</code>.
	 */
	public Label(final String text, final Set<Address.Type> addressTypes, final Locale locale) throws NullPointerException
	{
		super(text, locale); //construct the locale text
		this.addressTypes = immutableSetOf(addressTypes);
	}

}
