package com.garretwilson.security;

import java.io.UnsupportedEncodingException;
import java.security.*;

import static com.garretwilson.lang.CharacterUtilities.*;
import com.garretwilson.text.CharacterEncodingConstants;

/**Utility methods for working with message digests.
@author Garret Wilson
*/
public class MessageDigestUtilities
{

	/**Computes a digest for the given string using the UTF-8 character encoding.
	@param messageDigest The implementation of a message digest algorithm.
	@param string The string to digest.
	@return The array of bytes for the resulting hash value.
	*/
	public static byte[] digest(final MessageDigest messageDigest, final String string)
	{
		return digest(messageDigest, string.toCharArray());	//create the digest from the string's characters
	}

	/**Computes a digest for the given string, using the given character encoding.
	@param messageDigest The implementation of a message digest algorithm.
	@param string The string to digest.
	@param encoding The encoding to use when converting characters to bytes.
	@return The array of bytes for the resulting hash value.
	@exception UnsupportedEncodingException if the given encoding is not supported.
	*/
	public static byte[] digest(final MessageDigest messageDigest, final String string, final String encoding) throws UnsupportedEncodingException
	{
		return digest(messageDigest, string.toCharArray(), encoding);	//create the digest from the string's characters using the given encoding
	}


	/**Computes a digest for the given characters using the UTF-8 character encoding.
	@param messageDigest The implementation of a message digest algorithm.
	@param characters The characters to digest.
	@return The array of bytes for the resulting hash value.
	*/
	public static byte[] digest(final MessageDigest messageDigest, final char[] characters)
	{
		try
		{
			return digest(messageDigest, characters, CharacterEncodingConstants.UTF_8);	//digest the characters using UTF-8
		}
		catch(UnsupportedEncodingException unsupportedEncodingException)	//all JVMs should support UTF-8
		{
			throw new AssertionError(unsupportedEncodingException);
		}
	}

	/**Computes a digest for the given characters, using the given character encoding.
	@param messageDigest The implementation of a message digest algorithm.
	@param characters The characters to digest.
	@param encoding The encoding to use when converting characters to bytes.
	@return The array of bytes for the resulting hash value.
	@exception UnsupportedEncodingException if the given encoding is not supported.
	*/
	public static byte[] digest(final MessageDigest messageDigest, final char[] characters, final String encoding) throws UnsupportedEncodingException
	{
		final byte[] bytes=toByteArray(characters, encoding);	//convert the characters to bytes
		return messageDigest.digest(bytes);	//calculate and return the digest
	}

}
