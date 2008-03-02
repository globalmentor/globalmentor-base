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

package com.globalmentor.text.directory.vcard;

import java.util.*;
import com.globalmentor.java.*;
import com.globalmentor.util.*;

/**An object representing the "LABEL" type of a vCard <code>text/directory</code>
	profile as defined in <a href="http://www.ietf.org/rfc/rfc2426.txt">RFC 2426</a>,
	"vCard MIME Directory Profile".
@author Garret Wilson
*/
public class Label extends LocaledText implements AddressConstants
{

	/**The delivery address type.*/
	private int addressType;

		/**@return The delivery address type; defaults to
			<code>INTERNATIONTAL_ADDRESS_TYPE</code> | </code>POSTAL_ADDRESS_TYPE</code>
			| <code>PARCEL_ADDRESS_TYPE</code> | <code>WORK_ADDRESS_TYPE</code>.
		*/
		public int getAddressType() {return addressType;}

		/**Sets the delivery address type.
		@param addressType The delivery address type, one or more of the
			<code>XXX_ADDRESS_TYPE</code> constants ORed together.
		*/
		public void setAddressType(final int addressType) {this.addressType=addressType;}

	/**Default constructor.*/
	public Label()
	{
		this("");	//construct a default label with no information
	}

	/**Text constructor with no locale specified and a default address type of
		<code>INTERNATIONTAL_ADDRESS_TYPE</code> | </code>POSTAL_ADDRESS_TYPE</code>
		| <code>PARCEL_ADDRESS_TYPE</code> | <code>WORK_ADDRESS_TYPE</code>.
	@param text The non-<code>null</code> text string to represent.
	@exception NullPointerException Thrown if <code>text</code> is <code>null</code>.
	*/
	public Label(final String text) throws NullPointerException
	{
		this(text, null);	//create label with no locale
	}

	/**Constructor with default address type of
		<code>INTERNATIONTAL_ADDRESS_TYPE</code> | </code>POSTAL_ADDRESS_TYPE</code>
		| <code>PARCEL_ADDRESS_TYPE</code> | <code>WORK_ADDRESS_TYPE</code>.
	@param localeText The non-<code>null</code> object containing the text and
		optional locale.
	@exception NullPointerException Thrown if <code>localeText</code> is <code>null</code>.
	*/
	public Label(final LocaledText localeText) throws NullPointerException
	{
		this(localeText, DEFAULT_ADDRESS_TYPE);	//construct a label with the default address type
	}

	/**Constructor with default address type of
		<code>INTERNATIONTAL_ADDRESS_TYPE</code> | </code>POSTAL_ADDRESS_TYPE</code>
		| <code>PARCEL_ADDRESS_TYPE</code> | <code>WORK_ADDRESS_TYPE</code>.
	@param text The non-<code>null</code> text string to represent.
	@param locale The locale that represents the language of the text, or
		<code>null</code> if no language should be indicated.
	@exception NullPointerException Thrown if <code>text</code> is <code>null</code>.
	*/
	public Label(final String text, final Locale locale) throws NullPointerException
	{
		this(text, DEFAULT_ADDRESS_TYPE, locale);	//construct a label with the default address type
	}

	/**Address type constructor with no locale.
	@param text The non-<code>null</code> text string to represent.
	@param addressType The delivery address type, one or more of the
		<code>XXX_ADDRESS_TYPE</code> constants ORed together.
	@exception NullPointerException Thrown if <code>text</code> is <code>null</code>.
	*/
	public Label(final String text, final int addressType) throws NullPointerException
	{
		this(text, addressType, null);	//construct a label with the given address type and no locale
	}

	/**Full locale text constructor
	@param localeText The non-<code>null</code> object containing the text and
		optional locale.
	@param addressType The delivery address type, one or more of the
		<code>XXX_ADDRESS_TYPE</code> constants ORed together.
	@exception NullPointerException Thrown if <code>localeText</code> is <code>null</code>.
	*/
	public Label(final LocaledText localeText, final int addressType) throws NullPointerException
	{
		super(localeText);	//construct the locale text
		this.addressType=addressType;	//save the address type
	}

	/**Full constructor
	@param text The non-<code>null</code> text string to represent.
	@param locale The locale that represents the language of the text, or
		<code>null</code> if no language should be indicated.
	@param addressType The delivery address type, one or more of the
		<code>XXX_ADDRESS_TYPE</code> constants ORed together.
	@exception NullPointerException Thrown if <code>text</code> is <code>null</code>.
	*/
	public Label(final String text, final int addressType, final Locale locale) throws NullPointerException
	{
		super(text, locale);	//construct the locale text
		this.addressType=addressType;	//save the address type
	}

}
