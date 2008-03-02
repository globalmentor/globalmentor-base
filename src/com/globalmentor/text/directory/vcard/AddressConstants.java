package com.globalmentor.text.directory.vcard;

/**Constants useful for working with "ADR" and "LABEL" types of a vCard
	<code>text/directory</code> profile as defined in
	<a href="http://www.ietf.org/rfc/rfc2426.txt">RFC 2426</a>,
	"vCard MIME Directory Profile".
@author Garret Wilson
@see Address
@see Label
*/
public interface AddressConstants
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
}
