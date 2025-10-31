/*
 * Copyright © 2025 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.*;

import com.globalmentor.text.ArgumentSyntaxException;

/**
 * Tests of {@link EmailAddress} to ensure compliance with RFC 5322 addr-spec format.
 * @implNote Many test examples are derived from <a href="https://en.wikipedia.org/wiki/Email_address">Wikipedia's Email Address</a> article.
 * @author Garret Wilson
 */
public final class EmailAddressTest {

	//## Basic valid email addresses

	/**
	 * Tests for {@link EmailAddress#fromString(String)}.
	 * <p>Basic valid email addresses.</p>
	 */
	@Test
	public void testFromStringBasicValid() throws ArgumentSyntaxException {
		final EmailAddress example = EmailAddress.fromString("jdoe@example.com");
		assertThat("local part", example.getLocalPart(), is("jdoe"));
		assertThat("domain", example.getDomain(), is("example.com"));
		assertThat("string form", example.toString(), is("jdoe@example.com"));
	}

	/**
	 * Tests for {@link EmailAddress#fromString(String)}.
	 * <p>Email address with dot in local part.</p>
	 */
	@Test
	public void testFromStringWithDot() throws ArgumentSyntaxException {
		final EmailAddress address = EmailAddress.fromString("jane.doe@example.com");
		assertThat("local part", address.getLocalPart(), is("jane.doe"));
		assertThat("domain", address.getDomain(), is("example.com"));
	}

	/**
	 * Tests for {@link EmailAddress#fromString(String)}.
	 * <p>Email address with plus sign (tag/subaddressing).</p>
	 */
	@Test
	public void testFromStringWithPlusTag() throws ArgumentSyntaxException {
		final EmailAddress address = EmailAddress.fromString("jane.doe+tag@example.com");
		assertThat("local part", address.getLocalPart(), is("jane.doe+tag"));
		assertThat("domain", address.getDomain(), is("example.com"));
	}

	/**
	 * Tests for {@link EmailAddress#fromString(String)}.
	 * <p>Email address with hyphen in local part.</p>
	 */
	@Test
	public void testFromStringWithDash() throws ArgumentSyntaxException {
		final EmailAddress address = EmailAddress.fromString("jane.doe-foo@example.com");
		assertThat("local part", address.getLocalPart(), is("jane.doe-foo"));
		assertThat("domain", address.getDomain(), is("example.com"));
	}

	/**
	 * Tests for {@link EmailAddress#fromString(String)}.
	 * <p>Single letter local part.</p>
	 */
	@Test
	public void testFromStringSingleLetterLocalPart() throws ArgumentSyntaxException {
		final EmailAddress address = EmailAddress.fromString("x@example.com");
		assertThat("local part", address.getLocalPart(), is("x"));
		assertThat("domain", address.getDomain(), is("example.com"));
	}

	/**
	 * Tests for {@link EmailAddress#fromString(String)}.
	 * <p>Email address with hyphens and subdomains.</p>
	 */
	@Test
	public void testFromStringDashedDomain() throws ArgumentSyntaxException {
		final EmailAddress address = EmailAddress.fromString("foo-bar@strange-example.com");
		assertThat("local part", address.getLocalPart(), is("foo-bar"));
		assertThat("domain", address.getDomain(), is("strange-example.com"));
	}

	/**
	 * Tests for {@link EmailAddress#fromString(String)}.
	 * <p>Email address with long TLD.</p>
	 */
	@Test
	public void testFromStringLongTLD() throws ArgumentSyntaxException {
		final EmailAddress address = EmailAddress.fromString("example@s.solutions");
		assertThat("local part", address.getLocalPart(), is("example"));
		assertThat("domain", address.getDomain(), is("s.solutions"));
	}

	/**
	 * Tests for {@link EmailAddress#fromString(String)}.
	 * <p>Email address with slashes in local part.</p>
	 */
	@Test
	public void testFromStringWithSlash() throws ArgumentSyntaxException {
		final EmailAddress address = EmailAddress.fromString("name/surname@example.com");
		assertThat("local part", address.getLocalPart(), is("name/surname"));
		assertThat("domain", address.getDomain(), is("example.com"));
	}

	/**
	 * Tests for {@link EmailAddress#fromString(String)}.
	 * <p>Email address with underscore at end of local part.</p>
	 */
	@Test
	public void testFromStringWithTrailingDash() throws ArgumentSyntaxException {
		final EmailAddress address = EmailAddress.fromString("user-@example.org");
		assertThat("local part", address.getLocalPart(), is("user-"));
		assertThat("domain", address.getDomain(), is("example.org"));
	}

	//## Quoted string email addresses

	/**
	 * Tests for {@link EmailAddress#fromString(String)}.
	 * <p>Email address with quoted string containing space.</p>
	 */
	@Test
	public void testFromStringQuotedSpace() throws ArgumentSyntaxException {
		final EmailAddress address = EmailAddress.fromString("\" \"@example.org");
		assertThat("local part", address.getLocalPart(), is("\" \""));
		assertThat("domain", address.getDomain(), is("example.org"));
	}

	/**
	 * Tests for {@link EmailAddress#fromString(String)}.
	 * <p>Email address with quoted string containing double dots.</p>
	 */
	@Test
	public void testFromStringQuotedDoubleDot() throws ArgumentSyntaxException {
		final EmailAddress address = EmailAddress.fromString("\"john..doe\"@example.org");
		assertThat("local part", address.getLocalPart(), is("\"john..doe\""));
		assertThat("domain", address.getDomain(), is("example.org"));
	}

	/**
	 * Tests for {@link EmailAddress#fromString(String)}.
	 * <p>Email address with quoted string containing unusual characters.</p>
	 */
	@Test
	public void testFromStringQuotedUnusualCharacters() throws ArgumentSyntaxException {
		final EmailAddress address = EmailAddress.fromString("\"much.more unusual\"@example.com");
		assertThat("local part", address.getLocalPart(), is("\"much.more unusual\""));
		assertThat("domain", address.getDomain(), is("example.com"));
	}

	/**
	 * Tests for {@link EmailAddress#fromString(String)}.
	 * <p>Email address with quoted string containing an at sign.</p>
	 */
	@Test
	public void testFromStringQuotedAtSign() throws ArgumentSyntaxException {
		final EmailAddress address = EmailAddress.fromString("\"very.unusual.@.unusual.com\"@example.com");
		assertThat("local part", address.getLocalPart(), is("\"very.unusual.@.unusual.com\""));
		assertThat("domain", address.getDomain(), is("example.com"));
	}

	/**
	 * Tests for {@link EmailAddress#fromString(String)}.
	 * <p>Email address with quoted string containing special characters and escaped quotes.</p>
	 */
	@Test
	public void testFromStringQuotedComplex() throws ArgumentSyntaxException {
		final EmailAddress address = EmailAddress.fromString("\"very.(),:;<>[]\\\".VERY.\\\"very@\\\\ \\\"very\\\".unusual\"@strange.example.com");
		assertThat("local part", address.getLocalPart(), is("\"very.(),:;<>[]\\\".VERY.\\\"very@\\\\ \\\"very\\\".unusual\""));
		assertThat("domain", address.getDomain(), is("strange.example.com"));
	}

	/**
	 * Tests for {@link EmailAddress#fromString(String)}.
	 * <p>Email address with quoted string containing various special characters.</p>
	 */
	@Test
	public void testFromStringQuotedSpecialCharacters() throws ArgumentSyntaxException {
		final EmailAddress address = EmailAddress.fromString("\"()<>[]:,;@\\\\\\\"#$%&'-/=?_`{}| ~.a\"@example.org");
		assertThat("local part with special characters", address.getLocalPart(), is("\"()<>[]:,;@\\\\\\\"#$%&'-/=?_`{}| ~.a\""));
		assertThat("domain", address.getDomain(), is("example.org"));
	}

	//## Special characters in unquoted local part

	/**
	 * Tests for {@link EmailAddress#fromString(String)}.
	 * <p>Email address with various special characters in unquoted local part.</p>
	 */
	@Test
	public void testFromStringSpecialCharactersUnquoted() throws ArgumentSyntaxException {
		final EmailAddress address = EmailAddress.fromString("#$%&'*+-/=?_`{}|~@example.org");
		assertThat("local part", address.getLocalPart(), is("#$%&'*+-/=?_`{}|~"));
		assertThat("domain", address.getDomain(), is("example.org"));
	}

	//## Domain literals (IP addresses)

	/**
	 * Tests for {@link EmailAddress#fromString(String)}.
	 * <p>Email address with IPv4 address literal.</p>
	 */
	@Test
	public void testFromStringIPv4Literal() throws ArgumentSyntaxException {
		final EmailAddress address = EmailAddress.fromString("postmaster@[123.123.123.123]");
		assertThat("local part", address.getLocalPart(), is("postmaster"));
		assertThat("domain with IPv4", address.getDomain(), is("[123.123.123.123]"));
	}

	/**
	 * Tests for {@link EmailAddress#fromString(String)}.
	 * <p>Email address with IPv6 address literal.</p>
	 */
	@Test
	public void testFromStringIPv6Literal() throws ArgumentSyntaxException {
		final EmailAddress address = EmailAddress.fromString("user@[IPv6:2001:DB8::1]");
		assertThat("local part", address.getLocalPart(), is("user"));
		assertThat("domain with IPv6", address.getDomain(), is("[IPv6:2001:DB8::1]"));
	}

	/**
	 * Tests for {@link EmailAddress#fromString(String)}.
	 * <p>Email address with IPv6 address literal and underscore prefix.</p>
	 */
	@Test
	public void testFromStringIPv6LiteralWithUnderscore() throws ArgumentSyntaxException {
		final EmailAddress address = EmailAddress.fromString("_test@[IPv6:2001:0db8:85a3:0000:0000:8a2e:0370:7334]");
		assertThat("local part", address.getLocalPart(), is("_test"));
		assertThat("domain with full IPv6", address.getDomain(), is("[IPv6:2001:0db8:85a3:0000:0000:8a2e:0370:7334]"));
	}

	//## Factory method with separate components

	/**
	 * Tests for {@link EmailAddress#of(String, String)}.
	 * <p>Creating email address from separate components.</p>
	 */
	@Test
	public void testOfSeparateComponents() throws ArgumentSyntaxException {
		final EmailAddress address = EmailAddress.of("jdoe", "example.com");
		assertThat("local part", address.getLocalPart(), is("jdoe"));
		assertThat("domain", address.getDomain(), is("example.com"));
		assertThat("string form", address.toString(), is("jdoe@example.com"));
	}

	/**
	 * Tests for {@link EmailAddress#of(String, String)}.
	 * <p>Creating email address with quoted local part and domain literal.</p>
	 */
	@Test
	public void testOfQuotedAndLiteral() throws ArgumentSyntaxException {
		final EmailAddress address = EmailAddress.of("\"test user\"", "[192.168.1.1]");
		assertThat("quoted local part", address.getLocalPart(), is("\"test user\""));
		assertThat("domain literal", address.getDomain(), is("[192.168.1.1]"));
	}

	//## Static utility methods

	/**
	 * Tests for {@link EmailAddress#getLocalPart(CharSequence)}.
	 * <p>Extracting local part from email address string.</p>
	 */
	@Test
	public void testGetLocalPart() throws ArgumentSyntaxException {
		assertThat("simple address", EmailAddress.getLocalPart("user@example.com"), is("user"));
		assertThat("with dot", EmailAddress.getLocalPart("first.last@example.com"), is("first.last"));
		assertThat("quoted", EmailAddress.getLocalPart("\"test\"@example.com"), is("\"test\""));
	}

	/**
	 * Tests for {@link EmailAddress#getDomain(CharSequence)}.
	 * <p>Extracting domain from email address string.</p>
	 */
	@Test
	public void testGetDomain() throws ArgumentSyntaxException {
		assertThat("simple domain", EmailAddress.getDomain("user@example.com"), is("example.com"));
		assertThat("subdomain", EmailAddress.getDomain("user@mail.example.com"), is("mail.example.com"));
		assertThat("IP literal", EmailAddress.getDomain("user@[192.168.1.1]"), is("[192.168.1.1]"));
	}

	//## Equality and comparison

	/**
	 * Tests for {@link EmailAddress#equals(Object)}.
	 * <p>Equality of email addresses.</p>
	 */
	@Test
	public void testEquals() throws ArgumentSyntaxException {
		final EmailAddress address1 = EmailAddress.fromString("test@example.com");
		final EmailAddress address2 = EmailAddress.fromString("test@example.com");
		final EmailAddress address3 = EmailAddress.fromString("other@example.com");

		assertThat("same content equals", address1.equals(address2), is(true));
		assertThat("same instance equals", address1.equals(address1), is(true));
		assertThat("different content not equals", address1.equals(address3), is(false));
		assertThat("null not equals", address1.equals(null), is(false));
	}

	/**
	 * Tests for {@link EmailAddress#compareTo(EmailAddress)}.
	 * <p>Comparison of email addresses.</p>
	 */
	@Test
	public void testCompareTo() throws ArgumentSyntaxException {
		final EmailAddress address1 = EmailAddress.fromString("alice@example.com");
		final EmailAddress address2 = EmailAddress.fromString("bob@example.com");
		final EmailAddress address3 = EmailAddress.fromString("alice@sample.org");

		assertThat("same address compares equal", address1.compareTo(address1), is(0));
		assertThat("same domain, different local parts", address1.compareTo(address2) < 0, is(true));
		assertThat("different domains", address1.compareTo(address3) < 0, is(true));
	}

	//## Invalid email addresses

	/**
	 * Tests for {@link EmailAddress#fromString(String)}.
	 * <p>Email address missing at sign should throw exception.</p>
	 */
	@Test
	public void testFromStringMissingAtSign() {
		assertThrows(ArgumentSyntaxException.class, () -> EmailAddress.fromString("abc.example.com"), "missing @ sign");
	}

	/**
	 * Tests for {@link EmailAddress#fromString(String)}.
	 * <p>Email address with multiple at signs should throw exception.</p>
	 */
	@Test
	public void testFromStringMultipleAtSigns() {
		assertThrows(ArgumentSyntaxException.class, () -> EmailAddress.fromString("a@b@c@example.com"), "multiple @ signs");
	}

	/**
	 * Tests for {@link EmailAddress#fromString(String)}.
	 * <p>Email address with special characters outside quotes should throw exception.</p>
	 */
	@Test
	public void testFromStringSpecialCharsOutsideQuotes() {
		assertThrows(ArgumentSyntaxException.class, () -> EmailAddress.fromString("a\"b(c)d,e:f;g<h>i[j\\k]l@example.com"), "special characters outside quotes");
	}

	/**
	 * Tests for {@link EmailAddress#fromString(String)}.
	 * <p>Email address with improperly placed quotes should throw exception.</p>
	 */
	@Test
	public void testFromStringImproperQuotes() {
		assertThrows(ArgumentSyntaxException.class, () -> EmailAddress.fromString("just\"not\"right@example.com"), "improperly placed quotes");
	}

	/**
	 * Tests for {@link EmailAddress#fromString(String)}.
	 * <p>Email address with unescaped spaces should throw exception.</p>
	 */
	@Test
	public void testFromStringUnescapedSpaces() {
		assertThrows(ArgumentSyntaxException.class, () -> EmailAddress.fromString("this is\"not\\allowed@example.com"), "unescaped spaces");
	}

	/**
	 * Tests for {@link EmailAddress#fromString(String)}.
	 * <p>Email address with improperly escaped characters should throw exception.</p>
	 */
	@Test
	public void testFromStringImproperEscaping() {
		assertThrows(ArgumentSyntaxException.class, () -> EmailAddress.fromString("this\\ still\\\"not\\\\allowed@example.com"), "improperly escaped characters");
	}

	/**
	 * Tests for {@link EmailAddress#of(String, String)}.
	 * <p>Email address with invalid local part should throw exception.</p>
	 */
	@Test
	public void testOfInvalidLocalPart() {
		assertThrows(ArgumentSyntaxException.class, () -> EmailAddress.of("invalid local", "example.com"), "invalid local part with space");
	}

	/**
	 * Tests for {@link EmailAddress#of(String, String)}.
	 * <p>Email address with invalid domain should throw exception.</p>
	 */
	@Test
	public void testOfInvalidDomain() {
		assertThrows(ArgumentSyntaxException.class, () -> EmailAddress.of("user", "invalid domain"), "invalid domain with space");
	}

	//## URI generation

	/**
	 * Tests for {@link EmailAddress#getURI()}.
	 * <p>Generating mailto URI from email address.</p>
	 */
	@Test
	public void testGetURI() throws ArgumentSyntaxException {
		final EmailAddress address = EmailAddress.fromString("test@example.com");
		assertThat("mailto URI", address.getURI().toString(), is("mailto:test@example.com"));
	}

}
