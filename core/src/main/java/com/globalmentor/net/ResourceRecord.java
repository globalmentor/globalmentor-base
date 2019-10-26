/*
 * Copyright © 2019 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static com.globalmentor.java.CharSequences.*;
import static com.globalmentor.java.Conditions.*;
import static java.util.Objects.*;

import java.util.*;

import javax.annotation.*;

/**
 * Encapsulation of information that appears in the resource record of a DNS zone file.
 * @apiNote This is an entity type, not a value type. In particular equality is not defined based upon the contents of the record.
 * @implSpec This implementation does not support raw <code>TXT</code> records that begin and end with double-quote characters as if they were quoted. It is
 *           likely such values do not exist in real-world scenarios,
 * @implSpec This implementation does not handle escaped characters other than {@value #CHARACTER_STRING_QUOTE_CHAR} and {@value #CHARACTER_STRING_ESCAPE_CHAR}.
 *           as quoted strings as per RFC 1035 indicate escaping.
 * @see <a href="https://tools.ietf.org/html/rfc1035">RFC 1035: DOMAIN NAMES - IMPLEMENTATION AND SPECIFICATION</a>
 * @author Garret Wilson
 */
public class ResourceRecord {

	/** The character for quoting a string designated as a <dfn><code>character-string</code></dfn> in RFC 1035 containing spaces. */
	public static final char CHARACTER_STRING_QUOTE_CHAR = '"';

	/** The character for escaping a quote character in a string designated as a <dfn><code>character-string</code></dfn> in RFC 1035. */
	public static final char CHARACTER_STRING_ESCAPE_CHAR = '\\';

	/** A string representing an unquoted quote in a string designated as a <dfn><code>character-string</code></dfn> in RFC 1035. */
	private static final String CHARACTER_STRING_UNESCAPED_QUOTE = String.valueOf(CHARACTER_STRING_QUOTE_CHAR);

	/** A string representing a quoted quote in a string designated as a <dfn><code>character-string</code></dfn> in RFC 1035. */
	private static final String CHARACTER_STRING_ESCAPED_QUOTE = CHARACTER_STRING_ESCAPE_CHAR + CHARACTER_STRING_UNESCAPED_QUOTE;

	/** A string representing an unescaped escape in a string designated as a <dfn><code>character-string</code></dfn> in RFC 1035. */
	private static final String CHARACTER_STRING_UNESCAPED_ESCAPE = String.valueOf(CHARACTER_STRING_ESCAPE_CHAR);

	/** A string representing an escaped escape in a string designated as a <dfn><code>character-string</code></dfn> in RFC 1035. */
	private static final String CHARACTER_STRING_ESCAPED_ESCAPE = CHARACTER_STRING_ESCAPE_CHAR + CHARACTER_STRING_UNESCAPED_ESCAPE;

	/**
	 * Common, known resource record types.
	 * @apiNote This list is not exhaustive, but provides merely a convenience, type-safe approach for indicating common types.
	 * @see <a href="https://en.wikipedia.org/wiki/List_of_DNS_record_types">List of DNS record types</a>
	 * @see <a href="https://tools.ietf.org/html/rfc1035">RFC 1035: DOMAIN NAMES - IMPLEMENTATION AND SPECIFICATION</a>
	 * @see <a href="https://tools.ietf.org/html/rfc2308">RFC 2308: Negative Caching of DNS Queries (DNS NCACHE)</a>
	 */
	public static enum Type {

		/**
		 * A host address.
		 * @see <a href="https://tools.ietf.org/html/rfc1035">RFC 1035</a>
		 */
		A(1),
		/**
		 * The canonical name for an alias
		 * @see <a href="https://tools.ietf.org/html/rfc1035">RFC 1035</a>
		 */
		CNAME(5),
		/**
		 * Mailbox or mail list information.
		 * @see <a href="https://tools.ietf.org/html/rfc1035">RFC 1035</a>
		 */
		HINFO(14),
		/**
		 * A mailbox domain name.
		 * @apiNote Experimental.
		 * @see <a href="https://tools.ietf.org/html/rfc1035">RFC 1035</a>
		 */
		MB(7),
		/**
		 * A mail destination.
		 * @deprecated Obsolete; use {@link #MX}.
		 * @see <a href="https://tools.ietf.org/html/rfc1035">RFC 1035</a>
		 */
		@Deprecated
		MD(3),
		/**
		 * A mail forwarder.
		 * @deprecated Obsolete; use {@link #MX}.
		 * @see <a href="https://tools.ietf.org/html/rfc1035">RFC 1035</a>
		 */
		@Deprecated
		MF(3),
		/**
		 * A mail group member.
		 * @apiNote Experimental.
		 * @see <a href="https://tools.ietf.org/html/rfc1035">RFC 1035</a>
		 */
		MG(8),
		/**
		 * A mail rename domain name.
		 * @apiNote Experimental.
		 * @see <a href="https://tools.ietf.org/html/rfc1035">RFC 1035</a>
		 */
		MR(9),
		/**
		 * Mail exchange.
		 * @see <a href="https://tools.ietf.org/html/rfc1035">RFC 1035</a>
		 * @see <a href="https://tools.ietf.org/html/rfc7505">RFC 7505</a>
		 */
		MX(15),
		/**
		 * An authoritative name server.
		 * @see <a href="https://tools.ietf.org/html/rfc1035">RFC 1035</a>
		 */
		NS(2),
		/**
		 * A null resource record.
		 * @apiNote Experimental.
		 * @see <a href="https://tools.ietf.org/html/rfc1035">RFC 1035</a>
		 */
		NULL(10),
		/**
		 * A domain name pointer.
		 * @see <a href="https://tools.ietf.org/html/rfc1035">RFC 1035</a>
		 */
		PTR(12),
		/**
		 * Marks the start of a zone of authority.
		 * @see <a href="https://tools.ietf.org/html/rfc1035">RFC 1035</a>
		 * @see <a href="https://tools.ietf.org/html/rfc2308">RFC 2308</a>
		 */
		SOA(6),
		/**
		 * Text strings.
		 * @apiNote A <code>TXT</code> string value are <dfn><code>character-string</code></dfn> in RFC 1035
		 * @see <a href="https://tools.ietf.org/html/rfc1035">RFC 1035</a>
		 */
		TXT(16),
		/**
		 * A well known service description.
		 * @see <a href="https://tools.ietf.org/html/rfc1035">RFC 1035</a>
		 */
		WKS(11);

		private final int id;

		/** @return The type ID. */
		public int getId() {
			return id;
		}

		/**
		 * Constructor.
		 * @param id The type ID.
		 */
		private Type(final int id) {
			this.id = id;
		}

	}

	private final String type;

	/** @return The type of resource record. */
	public String getType() {
		return type;
	}

	private final DomainName name;

	/**
	 * @return The domain name of the resource record, which may be absolute or relative if the name should be resolved against the origin; or empty to indicate
	 *         the origin should be used.
	 */
	public Optional<DomainName> getName() {
		return Optional.ofNullable(name);
	}

	private final String value;

	/** @return The resource record value encoded as a appropriate for the resource record type. */
	public String getValue() {
		return value;
	}

	private final long ttl;

	/** @return The resource record cache time to live, in seconds; or empty if not specified, indicating the default TTL should be used. */
	public OptionalLong getTtl() {
		return ttl >= 0 ? OptionalLong.of(ttl) : OptionalLong.empty();
	}

	/**
	 * Constructor using a known type.
	 * @apiNote The given value must be in its unencoded form. If storing use-entered information, it may be desirable to call
	 *          {@link #decodeCharactString(String)} on the value first.
	 * @param type The type of resource record.
	 * @param name The domain name of the resource record, which may be absolute or relative if the name should be resolved against the origin; or
	 *          <code>null</code> to indicate the origin should be used.
	 * @param value The value to store in the resource record, encoded as a appropriate for the resource record type.
	 * @param ttl The resource record cache time to live, in seconds; or <code>-1</code> if not specified, indicating the default TTL should be used.
	 * @throws IllegalArgumentException if the given TTL is a negative value other than <code>-1</code>.
	 * @see #detectCharacterStringEncoded(String)
	 */
	public ResourceRecord(@Nonnull final Type type, @Nullable final DomainName name, @Nonnull final String value, @Nonnegative final long ttl) {
		this(type.name(), name, value, ttl);
	}

	/**
	 * Constructor.
	 * @apiNote The given value must be in its unencoded form. If storing use-entered information, it may be desirable to call
	 *          {@link #decodeCharactString(String)} on the value first.
	 * @param type The type of resource record.
	 * @param name The domain name of the resource record, which may be absolute or relative if the name should be resolved against the origin; or
	 *          <code>null</code> to indicate the origin should be used.
	 * @param value The value to store in the resource record, encoded as a appropriate for the resource record type.
	 * @param ttl The resource record cache time to live, in seconds; or <code>-1</code> if not specified, indicating the default TTL should be used.
	 * @throws IllegalArgumentException if the given TTL is a negative value other than <code>-1</code>.
	 * @see #detectCharacterStringEncoded(String)
	 */
	public ResourceRecord(@Nonnull final String type, @Nullable final DomainName name, @Nonnull final String value, @Nonnegative final long ttl) {
		this.type = requireNonNull(type);
		this.name = name;
		this.value = requireNonNull(value);
		if(ttl != -1) { //-1 is the only negative value allowed
			checkArgumentNotNegative(ttl);
		}
		this.ttl = ttl;
	}

	@Override
	public String toString() {
		final StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append('[').append(getType()).append("] `");
		getName().ifPresent(stringBuilder::append);
		stringBuilder.append("` = `").append(getValue()).append('`');
		getTtl().ifPresent(ttl -> stringBuilder.append(" (").append(ttl).append(")"));
		return stringBuilder.toString();
	}

	/**
	 * Attempts to determine whether a string designated as a <dfn><code>character-string</code></dfn> in RFC 1035 is already encoded (quoted) as per RFC 1035.
	 * @apiNote Not all string values are "character string" value; this only applies to the values of some types of records in RFC 1035, notably
	 *          {@link Type#TXT}.
	 * @implNote This method has no way of knowing with certainty whether a value is encoded, because a string with surrounding double-quote characters could
	 *           simply be a string itself needing encoding. However such a situation is very rare and unlikely to be found in practice.
	 * @param string A "character string" value that may or may not already be encoded.
	 * @return <code>true</code> if the given "character string" is in quoted, encoded form.
	 * @throws IllegalArgumentException if the given value is incorrectly encoded, for example starting but not ending with a double quote.
	 * @see #CHARACTER_STRING_QUOTE_CHAR
	 */
	public static boolean detectCharacterStringEncoded(@Nonnull final String string) {
		if(!string.isEmpty()) {
			if(startsWith(string, CHARACTER_STRING_QUOTE_CHAR)) {
				checkArgument(endsWith(string, CHARACTER_STRING_QUOTE_CHAR), "Value %s not correctly encoded.", string);
				//TODO ensure internal quotes and escapes are correctly escaped using `\` as per RFC 1035
				return true;
			}
		}
		return false;
	}

	/**
	 * Encodes (quotes) as needed a resource record string designated as a <dfn><code>character-string</code></dfn> in RFC 1035. If the value is detected as
	 * already encoded, the string is returned unchanged. Otherwise the string is quoted only as necessary (i.e. if it contains one or more spaces) unless always
	 * quoting is requested.
	 * @apiNote This method may be useful for normalizing an RFC 1035 character string entered by a user.
	 * @implSpec This method delegates to {@link #normalizeCharacterString(String, boolean)}.
	 * @implNote This method has no way of knowing with certainty whether a value is encoded, because a string with surrounding double-quote characters could
	 *           simply be a string itself needing encoding. However such a situation is very rare and unlikely to be found in practice.
	 * @param string A "character string" value that may or may not already be encoded.
	 * @return The value encoded as necessary for a "character string".
	 * @see #detectCharacterStringEncoded(String)
	 * @see #encodeCharacterString(String)
	 */
	public static String normalizeCharacterString(@Nonnull String string) {
		return normalizeCharacterString(string, false);
	}

	/**
	 * Encodes (quotes) as needed a resource record string designated as a <dfn><code>character-string</code></dfn> in RFC 1035. If the value is detected as
	 * already encoded, the string is returned unchanged. Otherwise the string is quoted only as necessary (i.e. if it contains one or more spaces).
	 * @apiNote This method may be useful for normalizing an RFC 1035 character string entered by a user.
	 * @implNote This method has no way of knowing with certainty whether a value is encoded, because a string with surrounding double-quote characters could
	 *           simply be a string itself needing encoding. However such a situation is very rare and unlikely to be found in practice.
	 * @param string A "character string" value that may or may not already be encoded.
	 * @param alwaysQuote <code>true</code> if the string should always be quoted, even if it contains no characters that require quoting by RFC 1035.
	 * @return The value encoded as necessary for a "character string".
	 * @see #detectCharacterStringEncoded(String)
	 * @see #encodeCharacterString(String, boolean)
	 */
	public static String normalizeCharacterString(@Nonnull String string, final boolean alwaysQuote) {
		return detectCharacterStringEncoded(string) ? string : encodeCharacterString(string, alwaysQuote);
	}

	/**
	 * Encodes (quotes) a resource record string designated as a <dfn><code>character-string</code></dfn> in RFC 1035. The string is quoted only as necessary
	 * (i.e. if it contains one or more spaces).
	 * @apiNote Calling this method multiple times on a string that needs encoding will result in a string that is multiply-encoded.
	 * @implSpec This method delegates to {@link #encodeCharacterString(String, boolean)}.
	 * @param string A raw, unencoded resource record string value.
	 * @return The value encoded as necessary for a "character string".
	 */
	public static String encodeCharacterString(@Nonnull String string) {
		return encodeCharacterString(string, false);
	}

	/**
	 * Encodes (quotes) a resource record string designated as a <dfn><code>character-string</code></dfn> in RFC 1035. The string is quoted only as necessary
	 * (i.e. if it contains one or more spaces) unless always quoting is requested.
	 * @apiNote Calling this method multiple times on a string that needs encoding will result in a string that is multiply-encoded.
	 * @param string A raw, unencoded resource record string value.
	 * @param alwaysQuote <code>true</code> if the string should always be quoted, even if it contains no characters that require quoting by RFC 1035.
	 * @return The value encoded as necessary for a "character string".
	 */
	public static String encodeCharacterString(@Nonnull String string, final boolean alwaysQuote) {
		if(alwaysQuote || contains(string, ' ') || startsWith(string, CHARACTER_STRING_QUOTE_CHAR) || endsWith(string, CHARACTER_STRING_QUOTE_CHAR)) {
			string = CHARACTER_STRING_QUOTE_CHAR + string.replace(CHARACTER_STRING_UNESCAPED_ESCAPE, CHARACTER_STRING_ESCAPED_ESCAPE)
					.replace(CHARACTER_STRING_UNESCAPED_QUOTE, CHARACTER_STRING_ESCAPED_QUOTE) + CHARACTER_STRING_QUOTE_CHAR;
		}
		return string;
	}

	/**
	 * Decodes a resource record string designated as a <dfn><code>character-string</code></dfn> in RFC 1035 if it is quoted. If the value is not quoted, the
	 * string is returned unchanged.
	 * @apiNote In theory calling this method multiple times on a string that was doubly quoted would result in different strings; that is, if a quoted string
	 *          itself had been encoded.
	 * @param characterString A resource record "character string" value in encoded form, potentially quoted.
	 * @throws IllegalArgumentException if the given value is incorrectly encoded, for example starting but not ending with a double quote.
	 * @return The character string decoded as appropriate.
	 */
	public static String decodeCharactString(@Nonnull String characterString) {
		if(!characterString.isEmpty()) {
			if(startsWith(characterString, CHARACTER_STRING_QUOTE_CHAR)) {
				checkArgument(endsWith(characterString, CHARACTER_STRING_QUOTE_CHAR), "Value %s not correctly encoded.", characterString);
				//TODO support decoding of other escaped values
				characterString = characterString.substring(1, characterString.length() - 1).replace(CHARACTER_STRING_ESCAPED_QUOTE, CHARACTER_STRING_UNESCAPED_QUOTE)
						.replace(CHARACTER_STRING_ESCAPED_ESCAPE, CHARACTER_STRING_UNESCAPED_ESCAPE);
			}
		}
		return characterString;
	}

}
