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

package com.globalmentor.iso.idcard;

/**Identifies common products used by the ISO ID card specification.
@author Garret Wilson
*/
public enum Product
{
/*TODO fix
		AUSTRALIAN_BANKCARD,
		
		DELTA,
		
		DINERS_CLUB,
		
		ENROUTE,
*/

	/**American Express card.
	@see <a href="http://www.americanexpress.com/">American Express</a>
	@see <a href="http://www125.americanexpress.com/merchant/oam/resources/FPHANDcvr.pdf">American Express Fraud Prevention Handbook</a>
	*/
	AMERICAN_EXPRESS(15),

	/**Discover card.
	@see <a href="http://www.discovercard.com/discover/">Discover Card</a>
	*/
	DISCOVER(16),

	/**JCB card.
	@see <a href="https://www.jcbinternational.com/">JCB International</a>
	*/
	JCB(16),
	
	/**MasterCard.
	@see <a href="http://www.mastercard.com/">MasterCard International</a>
	*/
	MASTERCARD(16),

	/**MasterCard debit card.
	This implementation includes 18 and 19-digit UK Maestro cards.
	@see <a href="http://www.maestrocard.com/uk/">MasterCard Maestro</a>
	@see <a href="http://www.barclaycardmerchantservices.co.uk/existing_customers/operational/pdf/binranges.pdf">Card Identification and Validation (January 2006)</a>
	 */
	MAESTRO(16, 18, 19),
	
	/**Solo debit card of Switch.
	@see <a href="http://www.solocard.co.uk/">Switch Card Services</a>
	@see <a href="http://www.barclaycardmerchantservices.co.uk/existing_customers/operational/pdf/binranges.pdf">Card Identification and Validation (January 2006)</a>
	 */
	SOLO(16, 18, 19),
	
	/**Visa card.
	@see <a href="http://www.visa.com/">Visa International</a>
	*/
	VISA(16);

	/**The array of valid PAN lengths.*/
	private final int[] panLengths;

		/**@return The array of valid PAN lengths.*/
		public int[] getPANLengths() {return panLengths.clone();}

	/**Constructs the product with the valid PAN lengths.
	@param panLengths The valid PAN lengths for this product.
	@throws NullPointerException if the given array is <code>null</code>.
	*/
	Product(final int... panLengths)
	{
		this.panLengths=panLengths.clone();	//save a cloned copy of the PAN lengths
	}
}