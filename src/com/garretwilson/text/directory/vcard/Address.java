package com.garretwilson.text.directory.vcard;

import java.util.*;
import com.garretwilson.lang.*;

/**An object representing the "ADR" type of a vCard <code>text/directory</code>
	profile as defined in <a href="http://www.ietf.org/rfc/rfc2426.txt">RFC 2426</a>,
	"vCard MIME Directory Profile".
@author Garret Wilson
*/
public class Address
{

	/**Indicates no address type is specified.*/
	public final static int NO_ADDRESS_TYPE=0;
	/**A domestic delivery address.*/
	public final static int DOMESTIC_ADDRESS_TYPE=1<<0;
	/**An international delivery address.*/
	public final static int INTERNATIONAL_ADDRESS_TYPE=1<<1;
	/**A postal delivery address.*/
	public final static int POSTAL_ADDRESS_TYPE=1<<2;
	/**A parcel delivery address.*/
	public final static int PARCEL_ADDRESS_TYPE=1<<3;
	/**A delivery address for a residence.*/
	public final static int HOME_ADDRESS_TYPE=1<<4;
	/**A delivery address for a place of work.*/
	public final static int WORK_ADDRESS_TYPE=1<<5;
	/**The preferred delivery address.*/
	public final static int PREFERRED_ADDRESS_TYPE=1<<6;

	/**The default delivery address type.*/
	public final static int DEFAULT_ADDRESS_TYPE=INTERNATIONAL_ADDRESS_TYPE|POSTAL_ADDRESS_TYPE|PARCEL_ADDRESS_TYPE|WORK_ADDRESS_TYPE;

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
   	
	/**The post office box.*/
	private String postOfficeBox;

		/**@return The post office box.*/
		public String getPostOfficeBox() {return postOfficeBox;}
		
		/**Sets the post office box.
		@param postOfficeBox The post office box, or <code>null</code> for no post office box.
		*/
		public void setPostOfficeBox(final String postOfficeBox) {this.postOfficeBox=postOfficeBox;}

	/**The extended addresses.*/
	private String[] extendedAddresses;

		/**@return The extended addresses.*/
		public String[] getExtendedAddresses() {return extendedAddresses;}

		/**@return The first extended address, or <code>null</code> if there are no extended addresses.*/
		public String getExtendedAddress() {return extendedAddresses.length>0 ? extendedAddresses[0] : null;}
		
		/**Sets the extended adresses.
		@param extendedAddresses The extended addresses.
		*/
		public void setExtendedAddresses(final String[] extendedAddresses) {this.extendedAddresses=extendedAddresses;}
		
		/**Sets the extended address.
		@param extendedAddress The extended address, or <code>null</code> for no extended address.
		*/
		public void setExtendedAddress(final String extendedAddress) {setExtendedAddresses(StringUtilities.createArray(extendedAddress));}

	/**The street addresses.*/
	private String[] streetAddresses;

		/**@return The street addresses.*/
		public String[] getStreetAddresses() {return streetAddresses;}

		/**@return The first street address, or <code>null</code> if there are no street addresses.*/
		public String getStreetAddress() {return streetAddresses.length>0 ? streetAddresses[0] : null;}
		
		/**Sets the street adresses.
		@param streetAddresses The street addresses.
		*/
		public void setStreetAddresses(final String[] streetAddresses) {this.streetAddresses=streetAddresses;}
		
		/**Sets the street address.
		@param streetAddress The street address, or <code>null</code> for no street address.
		*/
		public void setStreetAddress(final String streetAddress) {setStreetAddresses(StringUtilities.createArray(streetAddress));}

	/**The locality (e.g. city).*/
	private String locality;

		/**@return The locality (e.g. city).*/
		public String getLocality() {return locality;}
		
		/**Sets the locality (e.g. city).
		@param locality The locality (e.g. city), or <code>null</code> for no locality.
		*/
		public void setLocality(final String locality) {this.locality=locality;}

	/**The region (e.g. state or province).*/
	private String region;

		/**@return The region (e.g. state or province).*/
		public String getRegion() {return region;}
		
		/**Sets the region (e.g. state or province).
		@param region The region (e.g. state or province), or <code>null</code> for no region.
		*/
		public void setRegion(final String region) {this.region=region;}

	/**The postal code.*/
	private String postalCode;

		/**@return The postal code.*/
		public String getPostalCode() {return region;}
		
		/**Sets the postal code.
		@param postalCode The postal code, or <code>null</code> for no postal code.
		*/
		public void setPostalCode(final String postalCode) {this.postalCode=postalCode;}

	/**The country name.*/
	private String countryName;

		/**@return The country name.*/
		public String getCountryName() {return countryName;}
		
		/**Sets the country name.
		@param countryName The country name, or <code>null</code> for no country name.
		*/
		public void setCountryName(final String countryName) {this.countryName=countryName;}

	/**The locale that represents the language of the text, or <code>null</code>
		if no language is indicated.
	*/
	private Locale locale;

		/**@return The locale that represents the language of the text, or
			<code>null</code> if no language is indicated.
		*/
		public Locale getLocale() {return locale;}

		/**Sets the language used by the text.
		@param locale The locale that represents the language of the text, or
			<code>null</code> if no language should be indicated.
		*/
		public void setLocale(final Locale locale) {this.locale=locale;}

	/**Default constructor.*/
	public Address()
	{
		this((String)null, (String)null, (String)null, (String)null, (String)null, (String)null, (String)null);	//construct a default address with no information
	}

	/**Constructor with default address type of
		<code>INTERNATIONTAL_ADDRESS_TYPE</code> | </code>POSTAL_ADDRESS_TYPE</code>
		| <code>PARCEL_ADDRESS_TYPE</code> | <code>WORK_ADDRESS_TYPE</code>.
	@param postOfficeBox The post office box, or <code>null</code> for no post office box.
	@param extendedAddresses The extended addresses.
	@param streetAddresses The street addresses.
	@param locality The locality (e.g. city), or <code>null</code> for no locality.
	@param region The region (e.g. state or province), or <code>null</code> for no region.
	@param postalCode The postal code, or <code>null</code> for no postal code.
	@param countryName The country name, or <code>null</code> for no country name.
	*/
	public Address(final String postOfficeBox, final String[] extendedAddresses, final String[] streetAddresses, final String locality, final String region, final String postalCode, final String countryName)
	{
		this(postOfficeBox, extendedAddresses, streetAddresses, locality, region, postalCode, countryName, DEFAULT_ADDRESS_TYPE);	//construct an address with the default address type
	}

	/**Address type constructor.
	@param postOfficeBox The post office box, or <code>null</code> for no post office box.
	@param extendedAddresses The extended addresses.
	@param streetAddresses The street addresses.
	@param locality The locality (e.g. city), or <code>null</code> for no locality.
	@param region The region (e.g. state or province), or <code>null</code> for no region.
	@param postalCode The postal code, or <code>null</code> for no postal code.
	@param countryName The country name, or <code>null</code> for no country name.
	@param addressType The delivery address type, one or more of the
		<code>XXX_ADDRESS_TYPE</code> constants ORed together.
	*/
	public Address(final String postOfficeBox, final String[] extendedAddresses, final String[] streetAddresses, final String locality, final String region, final String postalCode, final String countryName, final int addressType)
	{
		this(postOfficeBox, extendedAddresses, streetAddresses, locality, region, postalCode, countryName, addressType, null);	//construct an address with no locale		
	}

	/**Full constructor.
	@param postOfficeBox The post office box, or <code>null</code> for no post office box.
	@param extendedAddresses The extended addresses.
	@param streetAddresses The street addresses.
	@param locality The locality (e.g. city), or <code>null</code> for no locality.
	@param region The region (e.g. state or province), or <code>null</code> for no region.
	@param postalCode The postal code, or <code>null</code> for no postal code.
	@param countryName The country name, or <code>null</code> for no country name.
	@param addressType The delivery address type, one or more of the
		<code>XXX_ADDRESS_TYPE</code> constants ORed together.
	@param locale The locale that represents the language of the text, or
		<code>null</code> if no language should be indicated.
	*/
	public Address(final String postOfficeBox, final String[] extendedAddresses, final String[] streetAddresses, final String locality, final String region, final String postalCode, final String countryName, final int addressType, final Locale locale)
	{
		setPostOfficeBox(postOfficeBox);
		setExtendedAddresses(extendedAddresses);
		setStreetAddresses(streetAddresses);
		setLocality(locality);
		setRegion(region);
		setPostalCode(postalCode);
		setCountryName(countryName);
		setAddressType(addressType);
		setLocale(locale);
	}

	/**Single constructor with default address type of
		<code>INTERNATIONTAL_ADDRESS_TYPE</code> | </code>POSTAL_ADDRESS_TYPE</code>
		| <code>PARCEL_ADDRESS_TYPE</code> | <code>WORK_ADDRESS_TYPE</code>.
	@param postOfficeBox The post office box, or <code>null</code> for no post office box.
	@param extendedAddress The extended address, or <code>null</code> for no extended address.
	@param streetAddress The street address, or <code>null</code> for no street address.
	@param locality The locality (e.g. city), or <code>null</code> for no locality.
	@param region The region (e.g. state or province), or <code>null</code> for no region.
	@param postalCode The postal code, or <code>null</code> for no postal code.
	@param countryName The country name, or <code>null</code> for no country name.
	*/
	public Address(final String postOfficeBox, final String extendedAddress, final String streetAddress, final String locality, final String region, final String postalCode, final String countryName)
	{
		this(postOfficeBox, extendedAddress, streetAddress, locality, region, postalCode, countryName, DEFAULT_ADDRESS_TYPE);	//construct the address with the default address type
	} 

	/**Address type constructor.
	@param postOfficeBox The post office box, or <code>null</code> for no post office box.
	@param extendedAddress The extended address, or <code>null</code> for no extended address.
	@param streetAddress The street address, or <code>null</code> for no street address.
	@param locality The locality (e.g. city), or <code>null</code> for no locality.
	@param region The region (e.g. state or province), or <code>null</code> for no region.
	@param postalCode The postal code, or <code>null</code> for no postal code.
	@param countryName The country name, or <code>null</code> for no country name.
	@param addressType The delivery address type, one or more of the
		<code>XXX_ADDRESS_TYPE</code> constants ORed together.
	*/
	public Address(final String postOfficeBox, final String extendedAddress, final String streetAddress, final String locality, final String region, final String postalCode, final String countryName, final int addressType)
	{
		this(postOfficeBox, extendedAddress, streetAddress, locality, region, postalCode, countryName, addressType, null);	//construct the address with no locale	
	}
	
	/**Full single constructor.
	@param postOfficeBox The post office box, or <code>null</code> for no post office box.
	@param extendedAddress The extended address, or <code>null</code> for no extended address.
	@param streetAddress The street address, or <code>null</code> for no street address.
	@param locality The locality (e.g. city), or <code>null</code> for no locality.
	@param region The region (e.g. state or province), or <code>null</code> for no region.
	@param postalCode The postal code, or <code>null</code> for no postal code.
	@param countryName The country name, or <code>null</code> for no country name.
	@param locale The locale that represents the language of the text, or
		<code>null</code> if no language should be indicated.
	*/
	public Address(final String postOfficeBox, final String extendedAddress, final String streetAddress, final String locality, final String region, final String postalCode, final String countryName, final int addressType, final Locale locale)
	{
		setPostOfficeBox(postOfficeBox);
		setExtendedAddress(extendedAddress);
		setStreetAddress(streetAddress);
		setLocality(locality);
		setRegion(region);
		setPostalCode(postalCode);
		setCountryName(countryName);
		setAddressType(addressType);
		setLocale(locale);
	}

	/**The string for separating the components of the string representation of
		the address type.
	*/
	protected final static String ADDRESS_TYPE_SEPARATOR=", ";

	/**@return A string to represent the delivery address type.*/
	public String getAddressTypeString()
	{
		return getAddressTypeString(getAddressType());	//return a string for our address type
	}

	/**Constructs a string to represent the given delivery address type.
	@param addressType The intended use, one or more of the
		<code>XXX_ADDRESS_TYPE</code> constants ORed together.
	 */
	public static String getAddressTypeString(final int addressType)	//G***i18n
	{
		final StringBuffer stringBuffer=new StringBuffer();
		if((addressType&PREFERRED_ADDRESS_TYPE)!=0)
		{
			if(stringBuffer.length()>0)
				stringBuffer.append(ADDRESS_TYPE_SEPARATOR);
			stringBuffer.append("preferred");
		}		
		if((addressType&WORK_ADDRESS_TYPE)!=0)
		{
			if(stringBuffer.length()>0)
				stringBuffer.append(ADDRESS_TYPE_SEPARATOR);
			stringBuffer.append("work");
		}		
		if((addressType&HOME_ADDRESS_TYPE)!=0)
		{
			if(stringBuffer.length()>0)
				stringBuffer.append(ADDRESS_TYPE_SEPARATOR);
			stringBuffer.append("home");
		}		
		if((addressType&INTERNATIONAL_ADDRESS_TYPE)!=0)
		{
			if(stringBuffer.length()>0)
				stringBuffer.append(ADDRESS_TYPE_SEPARATOR);
			stringBuffer.append("international");
		}		
		if((addressType&DOMESTIC_ADDRESS_TYPE)!=0)
		{
			if(stringBuffer.length()>0)
				stringBuffer.append(ADDRESS_TYPE_SEPARATOR);
			stringBuffer.append("domestic");
		}		
		if((addressType&PARCEL_ADDRESS_TYPE)!=0)
		{
			if(stringBuffer.length()>0)
				stringBuffer.append(ADDRESS_TYPE_SEPARATOR);
			stringBuffer.append("parcel");
		}		
		if((addressType&POSTAL_ADDRESS_TYPE)!=0)
		{
			if(stringBuffer.length()>0)
				stringBuffer.append(ADDRESS_TYPE_SEPARATOR);
			stringBuffer.append("postal");
		}		
		return stringBuffer.toString();		
	}

	/**@return A string representation of the address.*/
	public String toString()
	{
		final StringBuffer stringBuffer=new StringBuffer();	//create a new string buffer to hold the string we'll construct
		if(postOfficeBox!=null)	//if there is a post office box
			stringBuffer.append("PO Box ").append(postOfficeBox);	//append the post office box G***i18n
		if(postOfficeBox!=null && (extendedAddresses.length>0 || streetAddresses.length>0 || locality!=null || region!=null || postalCode!=null ||countryName!=null))	//if we added information and there is more information following
			stringBuffer.append('\n');	//append a newline
		StringBufferUtilities.append(stringBuffer, extendedAddresses, '\n');	//append the extended addresses, separated by a newline
		if(extendedAddresses.length>0 && (streetAddresses.length>0 || locality!=null || region!=null || postalCode!=null ||countryName!=null))	//if we added information and there is more information following
			stringBuffer.append('\n');	//append a newline
		StringBufferUtilities.append(stringBuffer, streetAddresses, '\n');	//append the street addresses, separated by a newline
		if(streetAddresses.length>0 && (locality!=null || region!=null || postalCode!=null ||countryName!=null))	//if we added information and there is more information following
			stringBuffer.append('\n');	//append a newline
		if(locality!=null)	//if there is a locality
			stringBuffer.append(locality);	//append the locality
		if(locality!=null && (region!=null || postalCode!=null ||countryName!=null))	//if we added information and there is more information following
			stringBuffer.append(", ");	//append a comma and a space
		if(region!=null)	//if there is a region
			stringBuffer.append(region);	//append the region
		if(region!=null && (postalCode!=null ||countryName!=null))	//if we added information and there is more information following
			stringBuffer.append('\n');	//append a newline
		if(postalCode!=null)	//if there is a postal code
			stringBuffer.append(postalCode);	//append the postal code
		if(postalCode!=null && (countryName!=null))	//if we added information and there is more information following
			stringBuffer.append(' ');	//append a space
		if(countryName!=null)	//if there is a country name
			stringBuffer.append(countryName);	//append the country name
		return stringBuffer.toString();	//return the string we constructed
	}
}