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

package com.globalmentor.net;

import java.net.URI;
import java.util.regex.*;

import com.globalmentor.java.Objects;
import static com.globalmentor.java.Objects.*;
import static com.globalmentor.net.URIs.*;
import com.globalmentor.text.ArgumentSyntaxException;

/**The encapsulation of an email address in the form specified by <a href="http://www.ietf.org/rfc/rfc2822.txt">RFC 2822, "Internet Message Format"</a>.
@author Garret Wilson
*/
public class EmailAddress implements Resource, Comparable<EmailAddress>
{

	/**The delimiter separating the local part from the domain.*/	
	public final static char LOCAL_PART_DOMAIN_DELIMITER='@';

	/**A regular expression pattern for matching the local part of an email addresses according to RFC 2822.
	This pattern is derived from the regular expression provided at <a href="http://www.email-unlimited.com/stuff/email_address_validator.htm">Email Address Validation</a>.
	@see <a href="http://www.email-unlimited.com/stuff/email_address_validator.htm">Email Address Validation</a>
	*/
	public final static Pattern LOCAL_PART_PATTERN=Pattern.compile("[-!#$%&'*+/0-9=?A-Z^_a-z{|}~](?:\\.?[-!#$%&'*+/0-9=?A-Z^_a-z{|}~])*");

	/**A regular expression pattern for matching the domain of an email addresses according to RFC 2822.
	This pattern is derived from the regular expression provided at <a href="http://www.email-unlimited.com/stuff/email_address_validator.htm">Email Address Validation</a>.
	@see <a href="http://www.email-unlimited.com/stuff/email_address_validator.htm">Email Address Validation</a>
	*/
	public final static Pattern DOMAIN_PATTERN=Pattern.compile("[a-zA-Z](?:-?[a-zA-Z0-9])*(?:\\.[a-zA-Z](?:-?[a-zA-Z0-9])*)+");

	/**A regular expression pattern for matching email addresses according to RFC 2822.
	The pattern has two matching groups: the local part, and the domain, in that order.
	This pattern is derived from the regular expression provided at <a href="http://www.email-unlimited.com/stuff/email_address_validator.htm">Email Address Validation</a>.
	@see <a href="http://www.email-unlimited.com/stuff/email_address_validator.htm">Email Address Validation</a>
	@see #LOCAL_PART_PATTERN
	@see #LOCAL_PART_DOMAIN_DELIMITER
	@see #DOMAIN_PATTERN
	*/
	public final static Pattern EMAIL_ADDRESS_PATTERN=Pattern.compile("("+LOCAL_PART_PATTERN+")"+LOCAL_PART_DOMAIN_DELIMITER+"("+DOMAIN_PATTERN+")");

	/**The local part of the email address.*/
	private final String localPart;

		/**@return The local part of the email address.*/
		public String getLocalPart() {return localPart;}

	/**The domain of the email address.*/
	private final String domain;

		/**@return The domain of the email address.*/
		public String getDomain() {return domain;}
		
	/**Constructs an email address from its separate components.
	@param localPart The local part of the email address.
	@param domain The domain of the email address.
	@exception NullPointerException if the given local part and/or domain is <code>null</code>.
	@exception ArgumentSyntaxException if the given local part and/or domain violates RFC 2822.
	*/
	public EmailAddress(final String localPart, final String domain) throws ArgumentSyntaxException	//TODO resolve encoding differences between this class and URIUtilities.createMailtoURI(); decide if we want the parameters to be encoded or raw
	{
		if(!LOCAL_PART_PATTERN.matcher(checkInstance(localPart, "Local part cannot be null.")).matches())	//if the local part does not match the pattern
		{
			throw new ArgumentSyntaxException("Local part "+localPart+" is syntactically incorrect.");
		}
		if(!DOMAIN_PATTERN.matcher(checkInstance(domain, "Domain cannot be null.")).matches())	//if the domain does not match the pattern
		{
			throw new ArgumentSyntaxException("Domain "+domain+" is syntactically incorrect.");
		}
		this.localPart=localPart;
		this.domain=domain;
	}

	/**Constructs an email address from a string.
	@param input The character sequence to be parsed as an email address.
	@exception NullPointerException if the given character sequence is <code>null</code>.
	@exception ArgumentSyntaxException if the input string violates RFC 2822.
	*/
	public EmailAddress(final CharSequence input) throws ArgumentSyntaxException
	{
		final Matcher matcher=EMAIL_ADDRESS_PATTERN.matcher(checkInstance(input, "Email address string cannot be null."));	//get a matcher for matching the given input string
		if(!matcher.matches())	//if the input string does not match the email address patter
		{
			throw new ArgumentSyntaxException("Email address "+input+" is syntactically incorrect.");
		}
		this.localPart=matcher.group(1);	//the first group contains the local part
		this.domain=matcher.group(2);	//the second group contains the domain
	}

	/**@return A hash code representing this object.*/
	public int hashCode()
	{
		return Objects.hashCode(getLocalPart(), getDomain());	//return a hash code for the local part and domain
	}

	/**Determines if this object is equivalent to another object.
	This method considers another object equivalent if it is another email address with the same local part and domain.
	@return <code>true</code> if the given object is an equivalent email address.
	*/
	public boolean equals(final Object object)
	{
		if(object instanceof EmailAddress)	//if the other object is an email address
		{
			final EmailAddress emailAddress=(EmailAddress)object;	//get the other object as an email address
			return getLocalPart().equals(emailAddress.getLocalPart()) && getDomain().equals(emailAddress.getDomain());	//compare local part and domain
		}
		else	//if the other object is not an email address
		{
			return false;	//the objects aren't equal
		}
	}

	/**Compares this object with the specified object for order.
	This implementation primarily by domain and secondarily by local part, ignoring case and locales.
	@param emailAddress The object to be compared.
	@return A negative integer, zero, or a positive integer as this object is less than, equal to, or greater than the specified object.
	@see #getDomain()
	@see #getLocalPart()
	*/
	public int compareTo(final EmailAddress emailAddress)
	{
		int result=getDomain().compareToIgnoreCase(emailAddress.getDomain());	//compare domains
		if(result==0)	//if domains are equal
		{
			result=getLocalPart().compareTo(emailAddress.getLocalPart());	//compare local parts
		}
		return result;	//return the result of the comparison
	}

	/**Constructs a string representation of the email address in its RFC 2822 format.
	This implementation returns the canonical version of the email address. 
	@return A string representation of the email address.
	*/
	public String toString()
	{
		return getLocalPart()+LOCAL_PART_DOMAIN_DELIMITER+getDomain();	//return "localPart@domain"
	}

	//Resource

	/**@return The resource identifier URI, or <code>null</code> if the identifier is not known.*/
	public URI getURI()
	{
		return URI.create(MAILTO_SCHEME+SCHEME_SEPARATOR+toString());	//construct and return the mailto URI
	}

}