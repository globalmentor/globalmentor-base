package com.garretwilson.text.directory.vcard;

import com.garretwilson.lang.*;

/**An object representing the "ADR" type of a vCard <code>text/directory</code>
	profile as defined in <a href="http://www.ietf.org/rfc/rfc2426.txt">RFC 2426</a>,
	"vCard MIME Directory Profile".
@author Garret Wilson
*/
public class Address
{
	
//TODO create INTL_ADDRESS_TYPE, WORK_ADDRESS_TYPE, etc.
	
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

	/**Full constructor.
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
		setPostOfficeBox(postOfficeBox);
		setExtendedAddresses(extendedAddresses);
		setStreetAddresses(streetAddresses);
		setLocality(locality);
		setRegion(region);
		setPostalCode(postalCode);
		setCountryName(countryName);
	}

	/**Full single constructor.
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
		setPostOfficeBox(postOfficeBox);
		setExtendedAddress(extendedAddress);
		setStreetAddress(streetAddress);
		setLocality(locality);
		setRegion(region);
		setPostalCode(postalCode);
		setCountryName(countryName);
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
