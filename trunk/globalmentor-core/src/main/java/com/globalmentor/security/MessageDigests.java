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

package com.globalmentor.security;

import java.nio.charset.Charset;
import java.security.*;

import static com.globalmentor.io.Charsets.*;

import static com.globalmentor.java.Characters.*;

/**Utility methods for working with message digests.
@author Garret Wilson
*/
public class MessageDigests
{

	/**The MD5 digest algorithm.*/
	public final static String MD5_ALGORITHM="MD5";
	/**The SHA digest algorithm.*/
	public final static String SHA_ALGORITHM="SHA";	

	/**Computes a digest for the given strings using the UTF-8 charset.
	@param messageDigest The implementation of a message digest algorithm.
	@param strings The strings to digest.
	@return The array of bytes for the resulting hash value.
	*/
	public static byte[] digest(final MessageDigest messageDigest, final String... strings)
	{
		return update(messageDigest, strings).digest();	//update the digest with the given strings and return the digest
	}

	/**Computes a digest for the given string, using the given charset.
	@param messageDigest The implementation of a message digest algorithm.
	@param charset The charset to use when converting characters to bytes.
	@param strings The strings to digest.
	@return The array of bytes for the resulting hash value.
	*/
	public static byte[] digest(final MessageDigest messageDigest, final Charset charset, final String... strings)
	{
		return update(messageDigest, charset, strings).digest();	//update the digest from the string's characters using the given charset and return the digest
	}

	/**Computes a digest for the given characters using the UTF-8 charset.
	@param messageDigest The implementation of a message digest algorithm.
	@param characters The characters to digest.
	@return The array of bytes for the resulting hash value.
	*/
	public static byte[] digest(final MessageDigest messageDigest, final char[] characters)
	{
		return digest(messageDigest, UTF_8_CHARSET, characters);	//digest the characters using UTF-8
	}

	/**Computes a digest for the given characters, using the given charset.
	@param messageDigest The implementation of a message digest algorithm.
	@param charset The charset to use when converting characters to bytes.
	@param characters The arrays of characters to digest.
	@return The array of bytes for the resulting hash value.
	*/
	public static byte[] digest(final MessageDigest messageDigest, final Charset charset, final char[] characters)
	{
		final byte[] bytes=toByteArray(characters, charset);	//convert the characters to bytes
		return messageDigest.digest(bytes);	//calculate and return the digest
	}

	/**Updates a digest from the given strings using the UTF-8 charset.
	@param messageDigest The implementation of a message digest algorithm.
	@param strings The string to digest.
	@return The message digest.
	*/
	public static MessageDigest update(final MessageDigest messageDigest, final String... strings)
	{
		return update(messageDigest, UTF_8_CHARSET, strings);	//update the digest using UTF-8
	}

	/**Updates a digest from given strings, using the given charset.
	@param messageDigest The implementation of a message digest algorithm.
	@param charset The charset to use when converting characters to bytes.
	@param strings The strings to digest.
	@return The message digest.
	*/
	public static MessageDigest update(final MessageDigest messageDigest, final Charset charset, final String... strings)
	{
		for(final String string:strings)	//for each string
		{
			update(messageDigest, charset, string);	//update the digest from the string using the given charset
		}
		return messageDigest;	//return the message digest
	}
	
	/**Updates a digest with the given characters using the UTF-8 charset.
	@param messageDigest The implementation of a message digest algorithm.
	@param characters The characters to digest.
	@return The message digest.
	*/
	public static MessageDigest update(final MessageDigest messageDigest, final char[] characters)
	{
		return update(messageDigest, UTF_8_CHARSET, characters);	//update the digest using UTF-8
	}

	/**Updates a digest with the given string, using the given charset.
	@param messageDigest The implementation of a message digest algorithm.
	@param charset The charset to use when converting characters to bytes.
	@param string The string to digest.
	@return The message digest.
	*/
	public static MessageDigest update(final MessageDigest messageDigest, final Charset charset, final String string)
	{
		final byte[] bytes=string.getBytes(charset);	//convert the characters to bytes
		messageDigest.update(bytes);	//update the digest
		return messageDigest;	//return the message digest
	}	

	/**Updates a digest with the given characters, using the given charset.
	@param messageDigest The implementation of a message digest algorithm.
	@param charset The charset to use when converting characters to bytes.
	@param characters The arrays of characters to digest.
	@return The message digest.
	*/
	public static MessageDigest update(final MessageDigest messageDigest, final Charset charset, final char[] characters)
	{
		final byte[] bytes=toByteArray(characters, charset);	//convert the characters to bytes
		messageDigest.update(bytes);	//update the digest
		return messageDigest;	//return the message digest
	}

}
