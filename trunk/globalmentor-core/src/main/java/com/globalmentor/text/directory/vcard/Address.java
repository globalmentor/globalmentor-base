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

import static com.globalmentor.collections.Lists.*;
import static com.globalmentor.collections.Sets.*;
import static com.globalmentor.java.Strings.*;
import static java.util.Arrays.*;

import java.util.*;

import com.globalmentor.java.*;

/**
 * An object representing the "ADR" type of a vCard <code>text/directory</code> profile as defined in <a href="http://www.ietf.org/rfc/rfc2426.txt">RFC
 * 2426</a>, "vCard MIME Directory Profile".
 * @author Garret Wilson
 */
public class Address
{

	/**
	 * The type of address.
	 * @author Garret Wilson
	 */
	public enum Type
	{
		/** A domestic delivery address. */
		DOM,
		/** An international delivery address. */
		INTL,
		/** A postal delivery address. */
		POSTAL,
		/** A parcel delivery address. */
		PARCEL,
		/** A delivery address for a residence. */
		HOME,
		/** A delivery address for a place of work. */
		WORK,
		/** The preferred delivery address. */
		PREF
	}

	/** The default delivery address type. */
	public final static Set<Type> DEFAULT_TYPES = EnumSet.of(Type.INTL, Type.POSTAL, Type.PARCEL, Type.WORK);

	/** The delivery address types. */
	private final Set<Type> types;

	/** @return The delivery address types. */
	public Set<Type> getTypes()
	{
		return types;
	}

	/** The post office box. */
	private final String postOfficeBox;

	/** @return The post office box. */
	public String getPostOfficeBox()
	{
		return postOfficeBox;
	}

	/** The read-only list of extended addresses. */
	private List<String> extendedAddresses;

	/** @return The extended addresses. */
	public List<String> getExtendedAddresses()
	{
		return extendedAddresses;
	}

	/** @return The first extended address, or <code>null</code> if there are no extended addresses. */
	public String getExtendedAddress()
	{
		return !extendedAddresses.isEmpty() ? extendedAddresses.get(0) : null;
	}

	/** The read-only list of street addresses. */
	private final List<String> streetAddresses;

	/** @return The street addresses. */
	public List<String> getStreetAddresses()
	{
		return streetAddresses;
	}

	/** @return The first street address, or <code>null</code> if there are no street addresses. */
	public String getStreetAddress()
	{
		return !streetAddresses.isEmpty() ? streetAddresses.get(0) : null;
	}

	/** The locality (e.g. city). */
	private final String locality;

	/** @return The locality (e.g. city). */
	public String getLocality()
	{
		return locality;
	}

	/** The region (e.g. state or province). */
	private final String region;

	/** @return The region (e.g. state or province). */
	public String getRegion()
	{
		return region;
	}

	/** The postal code. */
	private final String postalCode;

	/** @return The postal code. */
	public String getPostalCode()
	{
		return postalCode;
	}

	/** The country name. */
	private final String countryName;

	/** @return The country name. */
	public String getCountryName()
	{
		return countryName;
	}

	/** The locale that represents the language of the text, or <code>null</code> if no language is indicated. */
	private final Locale locale;

	/** @return The locale that represents the language of the text, or <code>null</code> if no language is indicated. */
	public Locale getLocale()
	{
		return locale;
	}

	/** Default constructor. */
	/*TODO del
		public Address()
		{
			this((String)null, (String)null, (String)null, (String)null, (String)null, (String)null, (String)null); //construct a default address with no information
		}
	*/

	/**
	 * Constructor with default address types of {@value #DEFAULT_TYPES}.
	 * @param postOfficeBox The post office box, or <code>null</code> for no post office box.
	 * @param extendedAddresses The extended addresses.
	 * @param streetAddresses The street addresses.
	 * @param locality The locality (e.g. city), or <code>null</code> for no locality.
	 * @param region The region (e.g. state or province), or <code>null</code> for no region.
	 * @param postalCode The postal code, or <code>null</code> for no postal code.
	 * @param countryName The country name, or <code>null</code> for no country name.
	 */
	public Address(final String postOfficeBox, final List<String> extendedAddresses, final List<String> streetAddresses, final String locality,
			final String region, final String postalCode, final String countryName)
	{
		this(postOfficeBox, extendedAddresses, streetAddresses, locality, region, postalCode, countryName, DEFAULT_TYPES); //construct an address with the default address types
	}

	/**
	 * Address type constructor.
	 * @param postOfficeBox The post office box, or <code>null</code> for no post office box.
	 * @param extendedAddresses The extended addresses.
	 * @param streetAddresses The street addresses.
	 * @param locality The locality (e.g. city), or <code>null</code> for no locality.
	 * @param region The region (e.g. state or province), or <code>null</code> for no region.
	 * @param postalCode The postal code, or <code>null</code> for no postal code.
	 * @param countryName The country name, or <code>null</code> for no country name.
	 * @param types The delivery address types.
	 */
	public Address(final String postOfficeBox, final List<String> extendedAddresses, final List<String> streetAddresses, final String locality,
			final String region, final String postalCode, final String countryName, final Set<Type> types)
	{
		this(postOfficeBox, extendedAddresses, streetAddresses, locality, region, postalCode, countryName, types, null); //construct an address with no locale		
	}

	/**
	 * Full constructor.
	 * @param postOfficeBox The post office box, or <code>null</code> for no post office box.
	 * @param extendedAddresses The extended addresses.
	 * @param streetAddresses The street addresses.
	 * @param locality The locality (e.g. city), or <code>null</code> for no locality.
	 * @param region The region (e.g. state or province), or <code>null</code> for no region.
	 * @param postalCode The postal code, or <code>null</code> for no postal code.
	 * @param countryName The country name, or <code>null</code> for no country name.
	 * @param types The delivery address types.
	 * @param locale The locale that represents the language of the text, or <code>null</code> if no language should be indicated.
	 */
	public Address(final String postOfficeBox, final List<String> extendedAddresses, final List<String> streetAddresses, final String locality,
			final String region, final String postalCode, final String countryName, final Set<Type> types, final Locale locale)
	{
		this.postOfficeBox = postOfficeBox;
		this.extendedAddresses = immutableListOf(extendedAddresses);
		this.streetAddresses = immutableListOf(streetAddresses);
		this.locality = locality;
		this.region = region;
		this.postalCode = postalCode;
		this.countryName = countryName;
		this.types = immutableSetOf(types);
		this.locale = locale;
	}

	/**
	 * Single constructor with default address types of {@value #DEFAULT_TYPES}.
	 * @param postOfficeBox The post office box, or <code>null</code> for no post office box.
	 * @param extendedAddress The extended address, or <code>null</code> for no extended address.
	 * @param streetAddress The street address, or <code>null</code> for no street address.
	 * @param locality The locality (e.g. city), or <code>null</code> for no locality.
	 * @param region The region (e.g. state or province), or <code>null</code> for no region.
	 * @param postalCode The postal code, or <code>null</code> for no postal code.
	 * @param countryName The country name, or <code>null</code> for no country name.
	 */
	public Address(final String postOfficeBox, final String extendedAddress, final String streetAddress, final String locality, final String region,
			final String postalCode, final String countryName)
	{
		this(postOfficeBox, extendedAddress, streetAddress, locality, region, postalCode, countryName, DEFAULT_TYPES); //construct the address with the default address types
	}

	/**
	 * Single address type constructor.
	 * @param postOfficeBox The post office box, or <code>null</code> for no post office box.
	 * @param extendedAddress The extended address, or <code>null</code> for no extended address.
	 * @param streetAddress The street address, or <code>null</code> for no street address.
	 * @param locality The locality (e.g. city), or <code>null</code> for no locality.
	 * @param region The region (e.g. state or province), or <code>null</code> for no region.
	 * @param postalCode The postal code, or <code>null</code> for no postal code.
	 * @param countryName The country name, or <code>null</code> for no country name.
	 * @param types The delivery address types.
	 */
	public Address(final String postOfficeBox, final String extendedAddress, final String streetAddress, final String locality, final String region,
			final String postalCode, final String countryName, final Set<Type> types)
	{
		this(postOfficeBox, extendedAddress, streetAddress, locality, region, postalCode, countryName, types, null); //construct the address with no locale	
	}

	/**
	 * Full single constructor.
	 * @param postOfficeBox The post office box, or <code>null</code> for no post office box.
	 * @param extendedAddress The extended address, or <code>null</code> for no extended address.
	 * @param streetAddress The street address, or <code>null</code> for no street address.
	 * @param locality The locality (e.g. city), or <code>null</code> for no locality.
	 * @param region The region (e.g. state or province), or <code>null</code> for no region.
	 * @param postalCode The postal code, or <code>null</code> for no postal code.
	 * @param countryName The country name, or <code>null</code> for no country name.
	 * @param types The delivery address types.
	 * @param locale The locale that represents the language of the text, or <code>null</code> if no language should be indicated.
	 */
	public Address(final String postOfficeBox, final String extendedAddress, final String streetAddress, final String locality, final String region,
			final String postalCode, final String countryName, final Set<Type> types, final Locale locale)
	{
		this(postOfficeBox, asList(extendedAddress), asList(streetAddress), locality, region, postalCode, countryName, types, locale);
	}

	/** @return A string to represent the delivery address types. */
	public String getTypeString()
	{
		return getTypeString(getTypes()); //return a string for our telephone type
	}

	/**
	 * Constructs a string to represent the given delivery address types.
	 * @param types The delivery address types.
	 */
	public static String getTypeString(final Set<Type> types)
	{
		return concat(types, ",");
	}

	/** @return A string representation of the address. */
	public String toString()
	{
		final StringBuilder stringBuilder = new StringBuilder(); //create a new string buffer to hold the string we'll construct
		if(postOfficeBox != null) //if there is a post office box
		{
			stringBuilder.append("PO Box ").append(postOfficeBox); //append the post office box TODO i18n
		}
		if(postOfficeBox != null
				&& (!extendedAddresses.isEmpty() || !streetAddresses.isEmpty() || locality != null || region != null || postalCode != null || countryName != null)) //if we added information and there is more information following
		{
			stringBuilder.append('\n'); //append a newline
		}
		StringBuilders.append(stringBuilder, extendedAddresses, '\n'); //append the extended addresses, separated by a newline
		if(!extendedAddresses.isEmpty() && (!streetAddresses.isEmpty() || locality != null || region != null || postalCode != null || countryName != null)) //if we added information and there is more information following
		{
			stringBuilder.append('\n'); //append a newline
		}
		StringBuilders.append(stringBuilder, streetAddresses, '\n'); //append the street addresses, separated by a newline
		if(!streetAddresses.isEmpty() && (locality != null || region != null || postalCode != null || countryName != null)) //if we added information and there is more information following
		{
			stringBuilder.append('\n'); //append a newline
		}
		if(locality != null) //if there is a locality
		{
			stringBuilder.append(locality); //append the locality
		}
		if(locality != null && (region != null || postalCode != null || countryName != null)) //if we added information and there is more information following
		{
			stringBuilder.append(", "); //append a comma and a space
		}
		if(region != null) //if there is a region
		{
			stringBuilder.append(region); //append the region
		}
		if(region != null && (postalCode != null || countryName != null)) //if we added information and there is more information following
		{
			stringBuilder.append('\n'); //append a newline
		}
		if(postalCode != null) //if there is a postal code
		{
			stringBuilder.append(postalCode); //append the postal code
		}
		if(postalCode != null && (countryName != null)) //if we added information and there is more information following
		{
			stringBuilder.append(' '); //append a space
		}
		if(countryName != null) //if there is a country name
		{
			stringBuilder.append(countryName); //append the country name
		}
		return stringBuilder.toString(); //return the string we constructed
	}
}
