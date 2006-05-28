package com.garretwilson.security;

import java.io.UnsupportedEncodingException;
import java.security.*;

import static com.garretwilson.lang.CharacterUtilities.*;
import com.garretwilson.text.CharacterEncoding;
import static com.garretwilson.text.CharacterEncodingConstants.*;

/**Utility methods for working with message digests.
@author Garret Wilson
*/
public class MessageDigestUtilities
{

	/**Computes a digest for the given strings using the UTF-8 character encoding.
	@param messageDigest The implementation of a message digest algorithm.
	@param strings The strings to digest.
	@return The array of bytes for the resulting hash value.
	*/
	public static byte[] digest(final MessageDigest messageDigest, final String... strings)
	{
		update(messageDigest, strings);	//update the digest with the given strings
		return messageDigest.digest();	//complete and return the digest
	}

	/**Computes a digest for the given string, using the given character encoding.
	@param messageDigest The implementation of a message digest algorithm.
	@param encoding The encoding to use when converting characters to bytes.
	@param strings The strings to digest.
	@return The array of bytes for the resulting hash value.
	@exception UnsupportedEncodingException if the given encoding is not supported.
	*/
	public static byte[] digest(final MessageDigest messageDigest, final CharacterEncoding encoding, final String... strings) throws UnsupportedEncodingException
	{
		update(messageDigest, encoding, strings);	//update the digest from the string's characters using the given encoding
		return messageDigest.digest();	//complete and return the digest
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
			return digest(messageDigest, UTF_8_ENCODING, characters);	//digest the characters using UTF-8
		}
		catch(UnsupportedEncodingException unsupportedEncodingException)	//all JVMs should support UTF-8
		{
			throw new AssertionError(unsupportedEncodingException);
		}
	}

	/**Computes a digest for the given characters, using the given character encoding.
	@param messageDigest The implementation of a message digest algorithm.
	@param encoding The encoding to use when converting characters to bytes.
	@param characters The arrays of characters to digest.
	@return The array of bytes for the resulting hash value.
	@exception UnsupportedEncodingException if the given encoding is not supported.
	*/
	public static byte[] digest(final MessageDigest messageDigest, final CharacterEncoding encoding, final char[] characters) throws UnsupportedEncodingException
	{
		final byte[] bytes=toByteArray(characters, encoding.getEncoding());	//convert the characters to bytes
		return messageDigest.digest(bytes);	//calculate and return the digest
	}

	/**Updates a digest from the given strings using the UTF-8 character encoding.
	@param messageDigest The implementation of a message digest algorithm.
	@param strings The string to digest.
	@return The message digest.
	*/
	public static MessageDigest update(final MessageDigest messageDigest, final String... strings)
	{
		for(final String string:strings)	//for each string
		{
			update(messageDigest, string.toCharArray());	//update the digest from the string's characters
		}
		return messageDigest;	//return the message digest
	}

	/**Updates a digest from given strings, using the given character encoding.
	@param messageDigest The implementation of a message digest algorithm.
	@param encoding The encoding to use when converting characters to bytes.
	@param strings The strings to digest.
	@return The message digest.
	@exception UnsupportedEncodingException if the given encoding is not supported.
	*/
	public static MessageDigest update(final MessageDigest messageDigest, final CharacterEncoding encoding, final String... strings) throws UnsupportedEncodingException
	{
		for(final String string:strings)	//for each string
		{
			update(messageDigest, encoding, string.toCharArray());	//update the digest from the string's characters using the given encoding
		}
		return messageDigest;	//return the message digest
	}
	
	/**Updates a digest with the given characters using the UTF-8 character encoding.
	@param messageDigest The implementation of a message digest algorithm.
	@param characters The characters to digest.
	@return The message digest.
	*/
	public static MessageDigest update(final MessageDigest messageDigest, final char[] characters)
	{
		try
		{
			update(messageDigest, UTF_8_ENCODING, characters);	//update the digest using UTF-8
		}
		catch(UnsupportedEncodingException unsupportedEncodingException)	//all JVMs should support UTF-8
		{
			throw new AssertionError(unsupportedEncodingException);
		}
		return messageDigest;	//return the message digest
	}

	/**Updates a digest with the given characters, using the given character encoding.
	@param messageDigest The implementation of a message digest algorithm.
	@param encoding The encoding to use when converting characters to bytes.
	@param characters The arrays of characters to digest.
	@return The message digest.
	@exception UnsupportedEncodingException if the given encoding is not supported.
	*/
	public static MessageDigest update(final MessageDigest messageDigest, final CharacterEncoding encoding, final char[] characters) throws UnsupportedEncodingException
	{
		final byte[] bytes=toByteArray(characters, encoding.getEncoding());	//convert the characters to bytes
		messageDigest.update(bytes);	//update the digest
		return messageDigest;	//return the message digest
	}	
	
}
