/*
 * Copyright © 1996-2017 GlobalMentor, Inc. <https://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
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

import static java.util.Objects.*;

import com.globalmentor.java.Characters;
import static com.globalmentor.net.URIs.*;

import com.globalmentor.text.*;

/**
 * Value class for email addresses represented in the form specified by <a href="https://datatracker.ietf.org/doc/html/rfc5322"><cite>RFC 5322: Internet Message
 * Format</cite></a>.
 * @apiNote This class validates email addresses according to RFC 5322 syntax but does not perform normalization such as lowercasing domain names as recommended
 *          by other RFCs. Email addresses are stored and compared as provided.
 * @author Garret Wilson
 * @see <a href="https://datatracker.ietf.org/doc/html/rfc5322">RFC 5322</a>
 */
public final class EmailAddress implements Resource, Comparable<EmailAddress> {

	/**
	 * Email address <code>atext</code> characters as per <cite>RFC 5322</cite>. <blockquote>Printable US-ASCII characters not including specials. Used for
	 * atoms.</blockquote>
	 */
	public static final Characters ATEXT_CHARACTERS = ABNF.ALPHA_CHARACTERS.add(ABNF.DIGIT_CHARACTERS).add('!', '#', '$', '%', '&', '\'', '*', '+', '-', '/', '=',
			'?', '^', '_', '`', '{', '|', '}', '~');

	/**
	 * Email address <code>dtext</code> characters as per <cite>RFC 5322</cite>. <em>This definition does not include the obsolete <code>obs-dtext</code> from
	 * <cite>RFC 5322</cite>.</em> <blockquote>Printable US-ASCII characters not including "[", "]", or "\".</blockquote>
	 */
	public static final Characters DTEXT_CHARACTERS = Characters.ofRange((char)33, (char)90).addRange((char)94, (char)126);

	/** The delimiter separating the local part from the domain. */
	public static final char LOCAL_PART_DOMAIN_DELIMITER = '@';

	/** The beginning delimiter of a <code>domain-literal</code>. */
	public static final char DOMAIN_LITERAL_BEGIN = '[';

	/** The ending delimiter of a <code>domain-literal</code>. */
	public static final char DOMAIN_LITERAL_END = ']';

	/**
	 * A regular expression pattern for matching the local part of an email addresses according to <cite>RFC 5322</cite>.
	 * @apiNote According to RFC 5322, the local part can be either a dot-atom or a quoted-string. The dot-atom allows alphanumerics and certain special
	 *          characters separated by dots. The quoted-string allows spaces and other special characters when enclosed in double quotes, with backslash
	 *          escaping.
	 * @implNote This pattern is derived from the regular expression provided at <a href="http://stackoverflow.com/a/201378/421049">an answer to <cite>Using a
	 *           regular expression to validate an email address</cite></a>.
	 * @see <a href="http://stackoverflow.com/q/201323/421049">Using a regular expression to validate an email address</a>
	 */
	public static final Pattern LOCAL_PART_PATTERN = Pattern.compile("(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*" //dot-atom
			+ "|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x21\\x23-\\x5b\\x5d-\\x7f]" //quoted-string: printable chars except \ and " (now includes space \x20)
			+ "|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")", //quoted-pair: backslash followed by any char
			Pattern.CASE_INSENSITIVE); //

	/**
	 * A regular expression pattern for matching the domain of an email addresses according to <cite>RFC 5322</cite>.
	 * @apiNote According to RFC 5322, the domain can be either a dot-atom (standard domain name) or a domain-literal (IP address in square brackets). Domain
	 *          literals support both IPv4 addresses and IPv6 addresses (prefixed with "IPv6:"). Note that domain literals use simplified patterns for syntax
	 *          validation; they do not enforce strict octet ranges for IPv4 (0-255) or full RFC 4291 compliance for IPv6 addresses.
	 * @implNote This pattern is derived from the regular expression provided at <a href="http://stackoverflow.com/a/201378/421049">an answer to <cite>Using a
	 *           regular expression to validate an email address</cite></a>, with improvements for IPv6 support.
	 * @see <a href="http://stackoverflow.com/q/201323/421049">Using a regular expression to validate an email address</a>
	 */
	public static final Pattern DOMAIN_PATTERN = Pattern.compile("(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?" //domain name
			+ "|\\[(?:[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}" //IPv4 literal: simple digit pattern
			+ "|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x5c-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])", //IPv6 prefix and content
			Pattern.CASE_INSENSITIVE);

	/**
	 * Regular expression definition for matching an email address.
	 * @see <a href="http://stackoverflow.com/q/201323/421049">Using a regular expression to validate an email address</a>
	 */
	public enum EmailAddressRegEx implements RegularExpression.NumberedCapturingGroup {
		/** The group of the email address local part. */
		LOCAL_PART_GROUP,
		/** The group of the email address domain. */
		DOMAIN_GROUP;

		/**
		 * The pattern for matching an email address according to <cite>RFC 5322</cite>.
		 * @implNote This pattern is derived from the regular expression provided at <a href="http://stackoverflow.com/a/201378/421049">an answer to <cite>Using a
		 *           regular expression to validate an email address</cite></a>.
		 * @see <a href="http://stackoverflow.com/q/201323/421049">Using a regular expression to validate an email address</a>
		 * @see #LOCAL_PART_PATTERN
		 * @see #LOCAL_PART_DOMAIN_DELIMITER
		 * @see #DOMAIN_PATTERN
		 */
		public static final Pattern PATTERN = Pattern.compile("(" + LOCAL_PART_PATTERN + ")" + LOCAL_PART_DOMAIN_DELIMITER + "(" + DOMAIN_PATTERN + ")",
				Pattern.CASE_INSENSITIVE);
	}

	/**
	 * A regular expression pattern for matching email addresses according to <cite>RFC 5322</cite>.
	 * @implNote This pattern is derived from the regular expression provided at <a href="http://stackoverflow.com/a/201378/421049">an answer to <cite>Using a
	 *           regular expression to validate an email address</cite></a>.
	 * @deprecated for removal in favor of {@link EmailAddressRegEx#PATTERN}.
	 * @see <a href="http://stackoverflow.com/q/201323/421049">Using a regular expression to validate an email address</a>
	 * @see #EMAIL_ADDRESS_PATTERN_LOCAL_PART_GROUP
	 * @see #EMAIL_ADDRESS_PATTERN_DOMAIN_GROUP
	 * @see #LOCAL_PART_PATTERN
	 * @see #LOCAL_PART_DOMAIN_DELIMITER
	 * @see #DOMAIN_PATTERN
	 */
	@Deprecated(forRemoval = true)
	public static final Pattern EMAIL_ADDRESS_PATTERN = EmailAddressRegEx.PATTERN;
	/**
	 * The group for returning the local part of an email address from a matcher derived from {@link #EMAIL_ADDRESS_PATTERN}.
	 * @deprecated for removal in favor of {@link EmailAddressRegEx#LOCAL_PART_GROUP}.
	 */
	@Deprecated(forRemoval = true)
	public static final int EMAIL_ADDRESS_PATTERN_LOCAL_PART_GROUP = 1;
	/**
	 * The group for returning the domain of an email address from a matcher derived from {@link #EMAIL_ADDRESS_PATTERN}.
	 * @deprecated for removal in favor of {@link EmailAddressRegEx#DOMAIN_GROUP}.
	 */
	@Deprecated(forRemoval = true)
	public static final int EMAIL_ADDRESS_PATTERN_DOMAIN_GROUP = 2;

	/** The local part of the email address. */
	private final String localPart;

	/**
	 * Returns the local part of the email address.
	 * @return The local part of the email address.
	 */
	public String getLocalPart() {
		return localPart;
	}

	/** The domain of the email address. */
	private final String domain;

	/**
	 * Returns the domain of the email address.
	 * @return The domain of the email address.
	 */
	public String getDomain() {
		return domain;
	}

	/**
	 * Constructs an email address from its separate components.
	 * @param localPart The local part of the email address.
	 * @param domain The domain of the email address.
	 * @throws NullPointerException if the given local part and/or domain is <code>null</code>.
	 */
	private EmailAddress(final String localPart, final String domain) throws ArgumentSyntaxException {
		this.localPart = requireNonNull(localPart);
		this.domain = requireNonNull(domain);
	}

	/**
	 * Constructs an email address from its separate components.
	 * @param localPart The local part of the email address.
	 * @param domain The domain of the email address.
	 * @return An email address with the given local part and domain.
	 * @throws NullPointerException if the given local part and/or domain is <code>null</code>.
	 * @throws ArgumentSyntaxException if the given local part and/or domain violates <cite>RFC 5322</cite>.
	 */
	public static EmailAddress of(final String localPart, final String domain) throws ArgumentSyntaxException { //TODO resolve encoding differences between this class and URIUtilities.createMailtoURI(); decide if we want the parameters to be encoded or raw
		if(!LOCAL_PART_PATTERN.matcher(requireNonNull(localPart, "Local part cannot be null.")).matches()) { //if the local part does not match the pattern
			throw new ArgumentSyntaxException("Local part " + localPart + " is syntactically incorrect.");
		}
		if(!DOMAIN_PATTERN.matcher(requireNonNull(domain, "Domain cannot be null.")).matches()) { //if the domain does not match the pattern
			throw new ArgumentSyntaxException("Domain " + domain + " is syntactically incorrect.");
		}
		return new EmailAddress(localPart, domain);
	}

	/**
	 * Constructs an email address from a string.
	 * @param input The string to be parsed as an email address.
	 * @return An email address from the given string representation.
	 * @throws NullPointerException if the given character sequence is <code>null</code>.
	 * @throws ArgumentSyntaxException if the input string violates <cite>RFC 5322</cite>.
	 */
	public static EmailAddress fromString(final String input) throws ArgumentSyntaxException {
		final Matcher matcher = EmailAddressRegEx.PATTERN.matcher(requireNonNull(input, "Email address string cannot be null.")); //get a matcher for matching the given input string
		if(!matcher.matches()) { //if the input string does not match the email address patter
			throw new ArgumentSyntaxException("Email address " + input + " is syntactically incorrect.");
		}
		final String localPart = EmailAddressRegEx.LOCAL_PART_GROUP.findIn(matcher).orElseThrow(AssertionError::new); //the first group contains the local part
		final String domain = EmailAddressRegEx.DOMAIN_GROUP.findIn(matcher).orElseThrow(AssertionError::new); //the second group contains the domain
		return new EmailAddress(localPart, domain);
	}

	/** @return A hash code representing this object. */
	public int hashCode() {
		return hash(getLocalPart(), getDomain()); //return a hash code for the local part and domain
	}

	/**
	 * Determines if this object is equivalent to another object. This method considers another object equivalent if it is another email address with the same
	 * local part and domain.
	 * @return <code>true</code> if the given object is an equivalent email address.
	 */
	public boolean equals(final Object object) {
		if(this == object) {
			return true;
		}
		if(!(object instanceof EmailAddress)) { //if the other object is an email address
			return false;
		}
		final EmailAddress emailAddress = (EmailAddress)object; //get the other object as an email address
		return getLocalPart().equals(emailAddress.getLocalPart()) && getDomain().equals(emailAddress.getDomain()); //compare local part and domain
	}

	/**
	 * Compares this object with the specified object for order. This implementation primarily by domain and secondarily by local part, ignoring case and locales.
	 * @param emailAddress The object to be compared.
	 * @return A negative integer, zero, or a positive integer as this object is less than, equal to, or greater than the specified object.
	 * @see #getDomain()
	 * @see #getLocalPart()
	 */
	public int compareTo(final EmailAddress emailAddress) {
		int result = getDomain().compareToIgnoreCase(emailAddress.getDomain()); //compare domains
		if(result == 0) { //if domains are equal
			result = getLocalPart().compareTo(emailAddress.getLocalPart()); //compare local parts
		}
		return result; //return the result of the comparison
	}

	/**
	 * Constructs a string representation of the email address in its <cite>RFC 5322</cite> format. This implementation returns the canonical version of the email
	 * address.
	 * @return A string representation of the email address.
	 */
	public String toString() {
		return getLocalPart() + LOCAL_PART_DOMAIN_DELIMITER + getDomain(); //return "localPart@domain"
	}

	//Resource

	/** @return The resource identifier URI, or <code>null</code> if the identifier is not known. */
	public URI getURI() {
		return URI.create(MAILTO_SCHEME + SCHEME_SEPARATOR + toString()); //construct and return the mailto URI
	}

	/**
	 * Returns the local part of an email address from a string.
	 * @param input The character sequence to be parsed as an email address.
	 * @return The local part of the given email address.
	 * @throws NullPointerException if the given character sequence is <code>null</code>.
	 * @throws ArgumentSyntaxException if the input string violates <cite>RFC 5322</cite>.
	 */
	public static String getLocalPart(final CharSequence input) throws ArgumentSyntaxException {
		final Matcher matcher = EmailAddressRegEx.PATTERN.matcher(requireNonNull(input, "Email address string cannot be null.")); //get a matcher for matching the given input string
		if(!matcher.matches()) { //if the input string does not match the email address patter
			throw new ArgumentSyntaxException("Email address " + input + " is syntactically incorrect.");
		}
		return EmailAddressRegEx.LOCAL_PART_GROUP.findIn(matcher).orElseThrow(AssertionError::new); //the first group contains the local part
	}

	/**
	 * Returns the domain of an email address from a string.
	 * @param input The character sequence to be parsed as an email address.
	 * @return The domain of the given email address.
	 * @throws NullPointerException if the given character sequence is <code>null</code>.
	 * @throws ArgumentSyntaxException if the input string violates <cite>RFC 5322</cite>.
	 */
	public static String getDomain(final CharSequence input) throws ArgumentSyntaxException {
		final Matcher matcher = EmailAddressRegEx.PATTERN.matcher(requireNonNull(input, "Email address string cannot be null.")); //get a matcher for matching the given input string
		if(!matcher.matches()) { //if the input string does not match the email address patter
			throw new ArgumentSyntaxException("Email address " + input + " is syntactically incorrect.");
		}
		return EmailAddressRegEx.DOMAIN_GROUP.findIn(matcher).orElseThrow(AssertionError::new); //the second group contains the domain
	}

}
