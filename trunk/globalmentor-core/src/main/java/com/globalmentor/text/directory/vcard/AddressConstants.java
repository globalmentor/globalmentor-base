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
