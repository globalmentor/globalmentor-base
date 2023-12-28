/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.security;

import java.util.Date;
import java.util.Random;
import java.util.StringTokenizer;

import static com.globalmentor.text.TextFormatter.*;

import com.globalmentor.java.CharSequences;
import com.globalmentor.text.ArgumentSyntaxException;

/**
 * A nonce that uses the current time, the date, a secret key, and a random number. This implementation stores information in the form:
 * <var>time</var>:<var>privateKey</var>:<var>value</var>
 * @author Garret Wilson
 */
public class DefaultNonce implements Nonce {

	/** The character that delimits fields in the nonce. */
	public static final char DELIMITER = ':';

	/** The string version of the character that delimits fields in the nonce. */
	protected static final String DELIMITER_STRING = String.valueOf(DELIMITER);

	/** The shared random number factory. */
	protected static final Random RANDOM = new Random();

	/** The private key stored in the nonce. */
	private final String privateKey;

	/** @return The private key stored in the nonce. */
	public String getPrivateKey() {
		return privateKey;
	}

	/** The time stored in the nonce. */
	private final Date time;

	/** @return The time stored in the nonce. */
	public Date getTime() {
		return time;
	}

	/** A random value held by the nonce. */
	private final long value;

	/**
	 * Returns a random value held by the nonce.
	 * @return A random value held by the nonce.
	 */
	public long getValue() {
		return value;
	}

	/**
	 * Constructs a new nonce with generated values.
	 * @param privateKey The private key stored in the nonce.
	 */
	public DefaultNonce(final String privateKey) {
		this(privateKey, new Date(), new Random().nextLong()); //construct the nonce with the current time and a new random number
	}

	/**
	 * Constructs a nonce from existing information.
	 * @param privateKey The private key stored in the nonce. This key must not contain any occurrences of the delimiter.
	 * @param time The time stored in the nonce.
	 * @param value A random value held by the nonce.
	 * @throws IllegalArgumentException if the private key contains an occurrence of the delimiter.
	 */
	protected DefaultNonce(final String privateKey, final Date time, final long value) throws IllegalArgumentException {
		if(CharSequences.contains(privateKey, DELIMITER)) { //if the private key contains the delimiter
			throw new IllegalArgumentException("Private key \"" + privateKey + "\" contains delimiter \'" + DELIMITER + "\'.");
		}
		this.privateKey = privateKey;
		this.time = time;
		this.value = value;
	}

	/**
	 * Creates a nonce from a formatted string.
	 * @param string The string containing the nonce.
	 * @return A nonce containing values that would generate the same string as the one given.
	 * @throws ArgumentSyntaxException if the given string does not have the correct format for this type of nonce.
	 */
	public DefaultNonce createNonce(final String string) throws ArgumentSyntaxException {
		try {
			final StringTokenizer tokenizer = new StringTokenizer(string, DELIMITER_STRING); //tokenize the string on the delimiter
			if(tokenizer.hasMoreTokens()) { //if there is a private key 
				final String privateKey = tokenizer.nextToken(); //get the private key
				if(tokenizer.hasMoreTokens()) { //if there is a time 
					final Date time = new Date(Long.parseLong(tokenizer.nextToken(), 16)); //parse the hex value of the time and create a date object from it
					if(tokenizer.hasMoreTokens()) { //if there is a value 
						final long value = Long.parseLong(tokenizer.nextToken(), 16); //parse the hex value
						return new DefaultNonce(privateKey, time, value); //create and return a new nonce from the values
					} else { //if there is no value
						throw new ArgumentSyntaxException("Missing value.", string);
					}
				} else { //if there is no time
					throw new ArgumentSyntaxException("Missing time.", string);
				}
			} else { //if there is no key
				throw new ArgumentSyntaxException("Missing key.", string);
			}
		} catch(final NumberFormatException numberFormatException) { //if one of the numbers couldn't be parsed
			throw new ArgumentSyntaxException(numberFormatException, string); //indicate that the string couldn't be parsed
		} catch(final IllegalArgumentException illegalArgumentException) { //if one of the parameters were incorrect
			throw new ArgumentSyntaxException(illegalArgumentException, string); //indicate that the string couldn't be parsed
		}
	}

	/** @return A string representation of the nonce, suitable for serialization. */
	public String toString() {
		return formatList(DELIMITER, privateKey, Long.toHexString(getTime().getTime()), Long.toHexString(getValue())); //format the nonce string  		
	}

}
