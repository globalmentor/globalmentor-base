/*
 * Copyright © 2019 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import static com.globalmentor.java.CharSequences.*;
import static com.globalmentor.java.Conditions.*;

import java.util.*;

import javax.annotation.*;

import com.globalmentor.java.CharSequences;

/**
 * Utilities and encapsulation of a <dfn>domain name</dfn> as defined in <a href="https://tools.ietf.org/html/rfc1035">RFC 1035</a>, <cite>DOMAIN NAMES -
 * IMPLEMENTATION AND SPECIFICATION</cite>.
 * @implNote The current implementation makes no validations of the domain name syntax other than a few simple rules related to delimiter location and number.
 * @see <a href="https://tools.ietf.org/html/rfc1035">RFC 1035: DOMAIN NAMES - IMPLEMENTATION AND SPECIFICATION</a>
 * @author Garret Wilson
 */
public final class DomainName {

	/** The dot <code>.</code> delimiter for domain name segments. */
	public static final char DELIMITER = '.';

	/** Two sequential delimiters, which is not allowed. */
	private static final String SUBSEQUENT_DELIMITERS = String.valueOf(DELIMITER) + DELIMITER;

	/** An empty relative domain name. */
	public static final DomainName EMPTY = new DomainName("");

	/** The root domain name as per RFC 1035 */
	public static final DomainName ROOT = new DomainName(String.valueOf(DELIMITER));

	/** The domain name string. */
	private final String name;

	/**
	 * Name constructor.
	 * @param name The string form of the domain name; empty names are allowed.
	 * @throws IllegalArgumentException if a domain name other than the root begins with a dot <code>.</code> character, or has two subsequent dot characters.
	 */
	private DomainName(@Nonnull final String name) {
		checkArgument(name.length() == 1 || !startsWith(name, DELIMITER), "Non-root domain name %s must not start with the `%s` delimiter.", name, DELIMITER);
		checkArgument(!name.contains(SUBSEQUENT_DELIMITERS), "Domain name %s must not contain subsequent `%s` delimiters.", name, DELIMITER);
		this.name = name;
	}

	/**
	 * Returns a domain name from a string form of the name.
	 * @param name The string form of the domain name; empty names are allowed.
	 * @return A domain name from the string.
	 * @throws IllegalArgumentException if a domain name other than the root begins with a dot <code>.</code> character, or has two subsequent dot characters.
	 * @see #DELIMITER
	 */
	public static DomainName of(@Nonnull final String name) {
		if(name.isEmpty()) {
			return EMPTY;
		}
		if(CharSequences.equals(name, DELIMITER)) {
			return ROOT;
		}
		return new DomainName(name);
	}

	/**
	 * Determines if the domain name is absolute; that is, it ends with a delimiter {@value #DELIMITER} character.
	 * @apiNote An absolute domain name is also referred to as a Fully Qualified Domain Name (FQDN).
	 * @return <code>true</code> if the domain name is absolute.
	 * @see #DELIMITER
	 */
	public boolean isAbsolute() {
		return endsWith(name, DELIMITER);
	}

	/**
	 * Checks to see if the domain name is absolute. If the domain name is not absolute, an exception is thrown.
	 * @return This domain name.
	 * @throws IllegalArgumentException if the domain name is not absolute.
	 * @see #isAbsolute()
	 */
	public DomainName checkArgumentAbsolute() throws IllegalArgumentException {
		checkArgument(isAbsolute(), "The domain name `%s` is not absolute.", this);
		return this;
	}

	/**
	 * Determines if this is an empty domain name; that is, one consisting of only an empty string <code>""</code>.
	 * @apiNote An empty domain name is a relative domain name.
	 * @return <code>true</code> if the domain name is empty.
	 * @see #EMPTY
	 */
	public boolean isEmpty() {
		return name.isEmpty();
	}

	/**
	 * Determines if this the root domain name; that is, one consisting of only the {@value #DELIMITER} delimiter.
	 * @apiNote The root domain is an absolute domain.
	 * @return <code>true</code> if the domain name is the root domain.
	 * @see #ROOT
	 */
	public boolean isRoot() {
		return CharSequences.equals(name, DELIMITER);
	}

	/**
	 * Determines if this domain name is relative; that is, it does not end with a delimiter {@value #DELIMITER} character.
	 * @implSpec This implementation delegates to {@link #isAbsolute()}.
	 * @return <code>true</code> if the domain name is relative.
	 * @see #DELIMITER
	 */
	public boolean isRelative() {
		return !isAbsolute();
	}

	/**
	 * Checks to see if this domain name is relative. If the domain name is not relative, an exception is thrown.
	 * @return This domain name.
	 * @throws IllegalArgumentException if the domain name is not relative.
	 * @see #isRelative()
	 */
	public DomainName checkArgumentRelative() throws IllegalArgumentException {
		checkArgument(isRelative(), "The domain name `%s` is not relative.", this);
		return this;
	}

	/**
	 * Relativizes the other domain against this name. If the other domain end with the identical segments of this domain name, the given domain name is returned.
	 * A domain name relativized against itself returns an empty domain name.
	 * @param domainName The domain name to relativize against this one.
	 * @return A domain name that, when resolved against this domain, will result in the given domain.
	 * @see #resolve(DomainName)
	 */
	public DomainName relativize(@Nonnull final DomainName domainName) {
		if(!isEmpty()) {
			final String string = toString();
			final String domainNameString = domainName.toString();
			if(domainNameString.endsWith(string)) {
				final int length = string.length();
				final int domainNameLength = domainNameString.length();
				if(domainNameLength == length) {
					return EMPTY;
				}
				if(domainNameLength > length) { //if the domain name ends with this one, make sure it is truly a segment division
					final int delimiterIndex;
					if(isRoot()) { //resolving against the root means simply dropping the ending delimiter
						delimiterIndex = domainNameLength - 1;
					} else {
						delimiterIndex = domainNameLength - length - 1; //otherwise take what comes _before_ the delimiter
						if(domainNameString.charAt(delimiterIndex) != DELIMITER) { //but make sure there is a delimiter
							return domainName;
						}
					}
					return DomainName.of(domainNameString.substring(0, delimiterIndex));
				}
			}
		}
		return domainName; //the domain name can't be resolved against this one
	}

	/**
	 * Resolves another domain name against this domain name. If the other domain name is absolute, the other domain name is returned. If the other domain name is
	 * empty, this domain name is returned. If the other domain is not empty but this domain is empty, the other domain name is returned. Otherwise, the other
	 * domain is prepended to this domain name using the {@link #DELIMITER} delimiter, without regard to whether this
	 * @param domainName The domain name to resolve against this one
	 * @return The other domain name resolved against this one.
	 * @see #relativize(DomainName)
	 */
	public DomainName resolve(@Nonnull final DomainName domainName) {
		if(domainName.isEmpty()) {
			return this;
		}
		if(isEmpty() || domainName.isAbsolute()) {
			return domainName;
		}
		final StringBuilder domainNameBuilder = new StringBuilder();
		domainNameBuilder.append(domainName.toString());
		if(!isRoot()) { //prevent subsequent delimiters if resolving against the root
			domainNameBuilder.append(DELIMITER);
		}
		domainNameBuilder.append(toString());
		return DomainName.of(domainNameBuilder.toString());
	}

	@Override
	public int hashCode() {
		return name.hashCode();
	}

	@Override
	public boolean equals(final Object object) {
		if(object == this) {
			return true;
		}
		if(!(object instanceof DomainName)) {
			return false;
		}
		return ((DomainName)object).toString().equals(toString());
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation returns the string form of the domain name.
	 */
	@Override
	public String toString() {
		return name;
	}

	/**
	 * Determines the domain name with the longest sequence of common base domains. For example if any combination of the domain names
	 * <code>www.example.com.</code>, <code>test.example.com.</code>, and <code>example.com.</code> are passed, the common base domain name
	 * <code>example.com.</code> is returned.
	 * <p>
	 * This method requires that all domain names be relative or all be absolute.
	 * </p>
	 * @apiNote This method does not make any checks to determine whether a segment is empty, e.g. <code>"foo..bar"</code>.
	 * @param domainNames The domain names to check.
	 * @return The greatest base domain name, which may not be present if there is no longest base.
	 * @throws NullPointerException if the iterable is <code>null</code> or contains a <code>null</code> value.
	 * @throws IllegalArgumentException if both relative and absolute domains names are passed.
	 */
	public static Optional<DomainName> findGreatestCommonBase(@Nonnull final Iterable<DomainName> domainNames) {
		final Iterator<DomainName> domainNameIterator = domainNames.iterator();
		if(!domainNameIterator.hasNext()) { //no domains given
			return Optional.empty();
		}
		Boolean areAbsolute = null;
		final List<String> domainNameStrings = new ArrayList<>();
		do {
			final DomainName domainName = domainNameIterator.next();
			final boolean isAbsolute = domainName.isAbsolute();
			if(areAbsolute != null) {
				checkArgument(areAbsolute == isAbsolute, "Cannot determine greatest common base domain name from a mix of relative and absolute domain names.");
			} else {
				areAbsolute = isAbsolute;
			}
			domainNameStrings.add(domainName.toString());
		} while(domainNameIterator.hasNext());
		assert areAbsolute != null : "At least one domain name was present; it should be known whether it was absolute.";
		final boolean shouldResolveToRoot = areAbsolute; //see if we need to resolve the result back to root
		return longestCommonSegmentSuffix(domainNameStrings, DELIMITER).map(DomainName::of)
				.map(baseDomainName -> shouldResolveToRoot ? ROOT.resolve(baseDomainName) : baseDomainName);
	}

}
