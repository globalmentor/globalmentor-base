package com.garretwilson.net.http;

import java.io.*;
import java.net.URI;
import java.security.NoSuchAlgorithmException;
import java.util.*;

import com.garretwilson.io.ParseIOException;
import com.garretwilson.io.ParseReader;
import static com.garretwilson.lang.BooleanUtilities.*;
import static com.garretwilson.lang.CharSequenceUtilities.*;
import static com.garretwilson.lang.StringBuilderUtilities.*;
import static com.garretwilson.net.http.DigestAuthenticationConstants.*;
import static com.garretwilson.net.http.HTTPConstants.*;
import static com.garretwilson.security.SecurityConstants.*;
import static com.garretwilson.text.CharacterConstants.*;
import static com.garretwilson.text.CharacterEncodingConstants.*;
import com.garretwilson.util.*;
import static com.garretwilson.util.MapUtilities.*;

/**Parses HTTP content. 
@author Garret Wilson
*/
public class HTTPParser
{

	/**Parses the HTTP status line.
	@param inputStream The source of the HTTP message.
	@return An array of parsed headers.
	*/
	public static HTTPStatus parseStatusLine(final InputStream inputStream) throws ParseIOException, EOFException, IOException
	{
		HTTPVersion version=null;
		int statusCode=-1;
		boolean haveStatusCode=false;	//show that we're still building the status code
		final StringBuilder versionBuilder=new StringBuilder();	//build the version
		final StringBuilder statusCodeBuilder=new StringBuilder();	//build the status code
		final ByteArrayOutputStream reasonByteArrayOutputStream=new ByteArrayOutputStream();	//create a dynamic byte array to build the reason phrase
		
		int value;	//we'll keep track of each value we read
		while((value=inputStream.read())>=0)	//read another value; while we haven't reached the end of the data stream
		{
//G***del Debug.trace("read value", value, "character", (char)value);
			if(version==null)	//if we're parsing the version
			{
				if(value==CR)	//ignore beginning CRLF sequences to compensate for buggy HTTP 1.0 implementations, as per the HTTP 1.1 specifications
				{
					parseLF(inputStream);	//make sure there is a following LF, but ignore it
				}
				else if(value==LF)	//if we get a bare LF
				{
					throw new ParseIOException("Unexpected LF.");					
				}
				else if(value==SP)	//if we've reached the delimiter
				{
					try
					{
						version=parseVersion(versionBuilder);	//parse the version
					}
					catch(final SyntaxException syntaxException)	//if the version wasn't syntactically correct
					{
						throw new ParseIOException(syntaxException.getMessage());	//make ParseIOException construction better
					}
				}
				else	//if we're still collecting version characters
				{
					versionBuilder.append((char)value);	//there is a byte-to-char equivalence for the version
				}
			}
			else if(!haveStatusCode)	//if we're building the status code
			{
				if(value==SP)	//if we've reached the delimiter
				{
					statusCode=Integer.parseInt(statusCodeBuilder.toString());	//parse the status code
					haveStatusCode=true;	//we're finished building the status code
				}
				else	//if we're still collecting status code characters
				{
					statusCodeBuilder.append((char)value);	//there is a byte-to-char equivalence for the status code
				}
			}
			else	//if we're building the reason phrase
			{
				if(value==CR)	//if we've reached the end of the reason phrase
				{
					parseLF(inputStream);	//make sure there is a following LF, but ignore it
					final String reasonPhrase=new String(reasonByteArrayOutputStream.toByteArray(), UTF_8);	//conver the reason phrase to a string
					return new HTTPStatus(version, statusCode, reasonPhrase);	//return the status we parsed
				}
				else	//if we're still collecting reason phrase characters
				{
					reasonByteArrayOutputStream.write((byte)value);	//save the reason phrase byte
				}
			}
		}
		throw new EOFException("Unexpectedly reached end of stream while reading status line.");		
	}
	
	/**Reads a byte expected to be an LF.
	@param inputStream The source of the second half of a CRLF sequence.
	@exception EOFException If there is no more data in the input stream.
	@exception ParseIOException if the next character read is not an LF.
	@exception IOException if there is an error reading the content.
	*/
	protected static void parseLF(final InputStream inputStream) throws EOFException, ParseIOException, IOException	//TODO make sure our byte-level processing doesn't interfere with any UTF-8 encoding
	{
		final int lfValue=inputStream.read();	//read the LF value
		if(lfValue<0)	//if we reached the end of the file
		{
			throw new EOFException("Unexpectedly reached end of stream while reading line looking for second half of CRLF.");						
		}
		else if(lfValue!=LF)	//if we found an unknown value
		{
			throw new ParseIOException("Unexpected character '"+(char)lfValue+"'.");
		}		
	}

	/**Parses an HTTP version from the given character sequence.
	@param versionCharSequence The version characters to parse.
	@return An HTTP version object.
	@throws SyntaxException if the HTTP version is not formatted correctly.
	 */
	public static HTTPVersion parseVersion(final CharSequence versionCharSequence) throws SyntaxException
	{
		final int versionBeginIndex=VERSION_IDENTIFIER.length()+1;	//the version begins after the version identifier and separator
			//if the sequence begins with "HTTP/"
		if(startsWith(versionCharSequence, VERSION_IDENTIFIER) && versionCharSequence.length()>versionBeginIndex && versionCharSequence.charAt(versionBeginIndex-1)==VERSION_SEPARATOR)
		{
			final int delimiterIndex=indexOf(versionCharSequence, VERSION_DELIMITER, versionBeginIndex);	//look for the version delimiter
			if(delimiterIndex>versionBeginIndex)	//if we found the delimiter
			{
				try
				{
					final int major=Integer.valueOf(versionCharSequence.subSequence(versionBeginIndex, delimiterIndex).toString());	//parse the major version number
					final int minor=Integer.valueOf(versionCharSequence.subSequence(delimiterIndex+1, versionCharSequence.length()).toString());	//parse the minor version number
					return new HTTPVersion(major, minor);	//return the version number we parsed
				}
				catch(final NumberFormatException numberFormatException)	//if one of the version numbers weren't correctly formatted
				{
					throw new SyntaxException(versionCharSequence.subSequence(versionBeginIndex, versionCharSequence.length()).toString(), numberFormatException);
				}
			}
			else	//if we didn't find the delimiter
			{
				throw new SyntaxException(versionCharSequence.toString(), "HTTP version missing delimiter '"+VERSION_DELIMITER+"'.");
			}
		}
		else	//if the sequence doesn't begin with "HTTP/"
		{
			throw new SyntaxException(versionCharSequence.toString(), "HTTP version does not begin with "+VERSION_IDENTIFIER+VERSION_SEPARATOR);
		}
	}
	
	/**Parses HTTP message headers, correctly folding LWS into a single space.
	@param inputStream The source of the HTTP message.
	@return The parsed headers.
	*/
	protected static Iterable<NameValuePair<String, String>> parseHeaders(final InputStream inputStream) throws ParseIOException, EOFException, IOException
	{
		final List<NameValuePair<String, String>> headerList=new ArrayList<NameValuePair<String, String>>();	//create a new list to hold the headers
		StringBuilder lineBuilder=new StringBuilder(parseHeaderLine(inputStream));	//parse the first header line
		while(lineBuilder.length()>0)	//while we haven't hit the empty line (originally containing only CRLF)
		{
			final StringBuilder nextLineBuilder=new StringBuilder(parseHeaderLine(inputStream));	//get the next line
			if(startsWithChar(nextLineBuilder, LWS_CHARS))	//if the new line begins with linear whitespace
			{
				trimBeginning(nextLineBuilder, LWS_CHARS);	//remove all beginning linear whitespace from the new line
				if(!endsWith(lineBuilder, SP))	//if the last line didn't end with a space
				{
					lineBuilder.append(SP);	//add a space to our line builder, as we're collapsing LWS into a single SP
				}
				lineBuilder.append(nextLineBuilder);	//append the next line to the previous one
			}
			else	//if the next line doesn't start with whitespace, it's a true new header (or the empty line); parse the last line 
			{
				final int delimiterIndex=charIndexOf(lineBuilder, DELIMITER_CHARS);	//get the index of the first delimiter
				if(delimiterIndex>=0 && lineBuilder.charAt(delimiterIndex)==HEADER_SEPARATOR)	//if we found the separator
				{
					final String name=lineBuilder.substring(0, delimiterIndex);	//find the name
					lineBuilder.delete(0, delimiterIndex+1);	//remove everything up to and including the delimiter
					trim(lineBuilder, LWS_CHARS);	//trim beginning and ending whitespace
					final String value=lineBuilder.toString();	//the value is whatever is remaining between the whitespace, if any
					headerList.add(new NameValuePair<String, String>(name, value));	//create a new name-value pair and add it to the list
					lineBuilder=nextLineBuilder;	//we'll start from the begining processing the next line next time
				}
				else	//if we didn't find the header separator
				{
					throw new ParseIOException("Header does not contain delimiter ':'.");					
				}
			}
		}
		return headerList;	//return the list of headers
	}

	/**Parses a line of text from a message header, assuming each line ends
	 	in CRLF and the content is encoded in UTF-8.
	All spaces and horizontal tabs are folded into a single space.
	@param inputStream The source of the HTTP message.
	@return A line of text without the ending CRLF.
	@exception ParseIOException if the line is not properly formatted.
	@exception EOFException If the end of the data string was unexpected reached
		while searching for the end of the line.
	@exception IOException if there is an error reading the content.
	*/
	protected static String parseHeaderLine(final InputStream inputStream) throws ParseIOException, EOFException, IOException	//TODO make sure our byte-level processing doesn't interfere with any UTF-8 encoding
	{
		final ByteArrayOutputStream byteArrayOutputStream=new ByteArrayOutputStream();	//create a dynamic byte array
//G***del		byte b;	//we'll keep track of each byte we read
		int value;	//we'll keep track of each value we read
		boolean foldingLWS=false;	//whether we are currently folding linear whitespace
		while((value=inputStream.read())>=0)	//read another value; while we haven't reached the end of the data stream
		{
			final byte b=(byte)value;	//cast the value to a byte
			if(b==CR)	//if this is the first half of a CRLF sequence
			{
				parseLF(inputStream);	//make sure there is a following LF, but ignore it
				final byte[] bytes=byteArrayOutputStream.toByteArray();	//get the bytes we collected
				return new String(bytes, UTF_8);	//return a string from the UTF-8-encoded bytes
			}
			else if(b==LF)	//if we get a bare LF
			{
				throw new ParseIOException("Unexpected LF.");					
			}
			if(contains(LWS_CHARS, (char)b))	//if this byte is linear white space
			{
				if(!foldingLWS)	//if we haven't started folding linear whitespace, yet
				{
					foldingLWS=true;	//we'll start folding linear whitespace now
					byteArrayOutputStream.write(SP);	//folder all linearwhitespace into a single space; future runs of whitespace will be ignored
				}
			}
			else	//if this is not linear whitespace
			{
				byteArrayOutputStream.write(b);	//write the byte normally
				foldingLWS=false;	//we're not folding linear whitespace, whether we were before or not
			}			
		}
		throw new EOFException("Unexpectedly reached end of stream while reading line looking for CRLF.");
	}

	/**Parses a list of attribute name/value pairs.*/
/*G***fix
	public final List<NameValuePair<String, String>> parseList(final String string)
	{
		
	}
	@param groupBegins The valid group beginning characters.
	@param groupEnds The valid group ending characters, matching to beginning characters.

	public ReaderTokenizer(final Reader reader, final String delimiters, final String groupBegins, final String groupEnds)
	{
*/

	/**Parses a list of */
/*G***fix
	public final List<NameValuePair<String, String>> parseList(final String string)
	{
		
	}
*/

	/**Parses a list of strings, reading until the end of the reader is reached.
	@param reader The source of the data.
	@exception IOException if there is an error reading the data.
	*/
	public static String[] parseList(final ParseReader reader) throws IOException
	{
		final List<String> elementList=new ArrayList<String>();	//create a new list to hold our list element
/*TODO maybe fix for EOL detection later 
		reader.skipCharsEOF(WHITESPACE_CHARS);	//skip whitespace until we reach a character or the end of the file
		reader.skipCh
*/
		reader.skipCharsEOF(WHITESPACE_CHARS+LIST_DELIMITER);	//skip whitespace and the list delimiter, if there is one (or two or however many)
		while(!reader.isEOF())	//while we haven't reached the end of the file
		{
			final String element=parseListElement(reader);	//parse the next element
			elementList.add(element);	//add this element
			reader.skipCharsEOF(WHITESPACE_CHARS+LIST_DELIMITER);	//skip whitespace and the list delimiter, if there is one (or two or however many)
		}
		return elementList.toArray(new String[elementList.size()]);	//return the list of elements we parsed
	}

	/**Parses a list element from the given reader.
	@param reader The source of the data.
	@exception IOException if there is an error reading the data.
	@return A string list element.
	*/
	public static String parseListElement(final ParseReader reader) throws IOException	//TODO fix to handle embedded quotes
	{
		reader.skipChars(WHITESPACE_CHARS);	//skip whitespace
		return reader.readStringUntilCharEOF(DELIMITER_CHARS);	//read until we hit a delimiter or the end of the file
	}

	/**Parses a list of attribute name/value pair from the given reader, reading until the end of the reader is reached.
	Quotes are removed from quoted values.
	The parameters are mapped by name and returned.
	@param reader The source of the data.
	@exception IOException if there is an error reading the data.
	@exception IllegalArgumentException if more than one parameter with the same
		name was exists.
	*/
	public static Map<String, String> parseParameterMap(final ParseReader reader) throws IOException, IllegalArgumentException
	{
		final List<NameValuePair<String, String>> parameterList=parseParameters(reader);	//parse the parameters
		final Map<String, String> parameterMap=new HashMap<String, String>();	//create a map for the parameters
		if(addAllValues(parameterMap, parameterList))	//add the values to the map; if we have duplicate values
		{
			throw new IllegalArgumentException("Encountered duplicate parameter names.");
		}
		return parameterMap;	//return the parameter map
	}

	/**Parses a list of attribute name/value pair from the given reader, reading until the end of the reader is reached.
	Quotes are removed from quoted values. 
	@param reader The source of the data.
	@exception IOException if there is an error reading the data.
	*/
	public static List<NameValuePair<String, String>> parseParameters(final ParseReader reader) throws IOException
	{
		final List<NameValuePair<String, String>> parameterList=new ArrayList<NameValuePair<String, String>>();	//create a new list to hold our parameters
/*TODO maybe fix for EOL detection later 
		reader.skipCharsEOF(WHITESPACE_CHARS);	//skip whitespace until we reach a character or the end of the file
		reader.skipCh
*/
		reader.skipCharsEOF(WHITESPACE_CHARS+LIST_DELIMITER);	//skip whitespace and the list delimiter, if there is one (or two or however many)
		while(!reader.isEOF())	//while we haven't reached the end of the file
		{
			final NameValuePair<String, String> parameter=parseParameter(reader);	//parse the next parameter
			parameterList.add(parameter);	//add this parameter
			reader.skipCharsEOF(WHITESPACE_CHARS+LIST_DELIMITER);	//skip whitespace and the list delimiter, if there is one (or two or however many)
		}
		return parameterList;	//return the list of parameters we parsed
	}
	
	/**Parses a attribute name/value pair from the given reader.
	Quotes are removed from quoted values. 
	@param reader The source of the data.
	@exception IOException if there is an error reading the data.
	@return A name/value pair representing a parameter.
	*/
	public static NameValuePair<String, String> parseParameter(final ParseReader reader) throws IOException
	{
		reader.skipChars(WHITESPACE_CHARS);	//skip whitespace
		final String name=reader.readStringUntilChar(WHITESPACE_CHARS+EQUALS_SIGN_CHAR);	//name
		reader.readExpectedChar(EQUALS_SIGN_CHAR);	//=
		reader.skipChars(WHITESPACE_CHARS);	//skip whitespace
		final String value;
		if(reader.peekChar()==QUOTE)	//if this value is quoted
		{
			value=parseQuotedString(reader);	//parse the quoted string value
		}
		else	//if the value isn't quoted
		{
			value=reader.readStringUntilCharEOF(DELIMITER_CHARS);	//read until we hit a delimiter or the end of the file
		}
		return new NameValuePair<String, String>(name, value);	//return the name and value we parsed
	}

	/**Parses a quoted string from the reader and returns the value within the quotes.
	Escaped quotes are correctly parsed.
	@param reader The source of the data.
	@exception IOException if there is an error reading the data.
	*/
	public static String parseQuotedString(final ParseReader reader) throws IOException
	{
		final StringBuilder stringBuilder=new StringBuilder();
		reader.readExpectedChar(QUOTE);	//"
		char nextChar;	//this will be a quote when we finish 
		do
		{
			stringBuilder.append(reader.readStringUntilChar(""+QUOTE+ESCAPE_CHAR));	//find the end of the quoted value or an escape character
			nextChar=reader.peekChar();	//see what the next character is
			if(nextChar==ESCAPE_CHAR)	//if we've run ino an escape character
			{
				if(reader.peek()==QUOTE)	//if this is a quoted pair (\")
				{
					reader.skip(1);	//skip the escape character and continue on
				}
			}
		}
		while(nextChar!=QUOTE);	//keep reading until we reach the ending quote
		reader.readExpectedChar(QUOTE);	//"		
		return stringBuilder.toString();	//return the value we parsed
	}
	
	/**Parses a list of attribute name/value pair from the given reader.
	Quoted values are 
	@param reader The source of the data.
	@exception IOException if there is an error reading the data.
	*/
/*G***fix
	public List<NameValuePair<String, String>> parseParameters(final ParseReader reader) throws IOException
	{
		reader.skipChars(WHITESPACE_CHARS);	//skip whitespace
		final String name=reader.readStringUntilChar(WHITESPACE_CHARS+EQUALS_SIGN_CHAR);	//name
		reader.readExpectedChar(EQUALS_SIGN_CHAR);	//=
		reader.skipChars(WHITESPACE_CHARS);	//skip whitespace
		if(reader.peekChar()==QUOTE_CHAR)	//if this value is quoted
		{
			
		}
		final String value=reader.readStringUntilChar(WHITESPACE_CHARS+EQUALS_SIGN_CHAR);	//name
		
		
		
		
	}
*/

	/**Parses an HTTP header and returns the authenticate credentials.
	This method does not allow the wildcard '*' request-URI for the digest URI parameter.
	@param header The header value.
	@return The credentials from the authorization header.
	@exception SyntaxException if the given header was not syntactically correct.
	@exception IllegalArgumentException if the authorization information is not supported. 
	@see HTTPConstants#AUTHORIZATION_HEADER
	*/
	public static AuthenticateCredentials parseAuthorizationHeader(final CharSequence header) throws SyntaxException, IllegalArgumentException
	{
Debug.trace("parsing authorization header", header);
		try
		{
			final int schemeDelimiterIndex=indexOf(header, SP);	//find the space between the scheme and the rest of the credentials
			if(schemeDelimiterIndex>=0)	//if we found the scheme delimiter
			{
				final String scheme=header.subSequence(0, schemeDelimiterIndex).toString();	//get the scheme
				final String parameters=header.subSequence(schemeDelimiterIndex+1, header.length()).toString();	//get the rest of the credentials
				switch(AuthenticationScheme.valueOf(scheme.toUpperCase()))	//see which type of authentication scheme this is
				{
					case DIGEST:
						{
							final Map<String, String> parameterMap=parseParameterMap(new ParseReader(parameters));	//parse the parameters into a map
		Debug.trace("parameter map", parameterMap);
							final String username=parameterMap.get(USERNAME_PARAMETER);	//get the username
							if(username==null)	//if no username is present
							{
								throw new SyntaxException(header.toString(), AUTHORIZATION_HEADER+" missing parameter "+USERNAME_PARAMETER);
							}
							final String realm=parameterMap.get(REALM_PARAMETER);	//get the realm
							if(realm==null)	//if no realm is present
							{
								throw new SyntaxException(header.toString(), AUTHORIZATION_HEADER+" missing parameter "+REALM_PARAMETER);
							}
							final String nonce=parameterMap.get(NONCE_PARAMETER);	//get the nonce
							if(nonce==null)	//if no nonce is present
							{
								throw new SyntaxException(header.toString(), AUTHORIZATION_HEADER+" missing parameter "+NONCE_PARAMETER);
							}
							final String digestURIString=parameterMap.get(DIGEST_URI_PARAMETER);	//get the digest URI as a string
							if(digestURIString==null)	//if no digest URI is present
							{
								throw new SyntaxException(header.toString(), AUTHORIZATION_HEADER+" missing parameter "+DIGEST_URI_PARAMETER);
							}
//TODO del when works							final URI digestURI=URI.create(digestURIString);	//create a URI from the digest URI string; this will reject the wildcard request URI ('*')
							final String response=parameterMap.get(RESPONSE_PARAMETER);	//get the response
							if(response==null)	//if no response is present
							{
								throw new SyntaxException(header.toString(), AUTHORIZATION_HEADER+" missing parameter "+RESPONSE_PARAMETER);
							}
							final String algorithm=parameterMap.get(ALGORITHM_PARAMETER);	//get the algorithm
							final String cnonce=parameterMap.get(CNONCE_PARAMETER);	//get the cnonce
							final String opaque=parameterMap.get(OPAQUE_PARAMETER);	//get the opaque parameter
							final String messageQOPString=parameterMap.get(QOP_PARAMETER);	//get the quality of protection
							final QOP messageQOP=messageQOPString!=null ? QOP.valueOfString(messageQOPString) : null;	//convert the quality of protection from a string to an enum
							final String nonceCountString=parameterMap.get(NONCE_COUNT_PARAMETER);	//get the quality of protection
							if(nonceCountString.length()!=NONCE_COUNT_LENGTH)	//if the nonce count is not of the correct length
							{
								throw new SyntaxException(header.toString(), AUTHORIZATION_HEADER+' '+NONCE_COUNT_PARAMETER+" does not have length "+NONCE_COUNT_LENGTH+".");
							}
							final long nonceCount=Long.parseLong(nonceCountString, 16);	//parse the hex nonce count string to a long
							return new DigestAuthenticateCredentials(username, realm, nonce, digestURIString, response, cnonce, opaque, messageQOP, nonceCount, algorithm!=null ? algorithm : MD5_ALGORITHM);
						}
					default:	//if we don't support this authentication scheme
						return null;	//show that we don't support this authentication scheme TODO fix for BASIC and other schemes
				}
			}
			else	//if no scheme delimiter was found
			{
				throw new SyntaxException(header.toString(), AUTHORIZATION_HEADER+" missing scheme delimiter.");
			}
		}
		catch(final NoSuchAlgorithmException noSuchAlgorithmException)	//if the algorithm was not supported
		{
			throw new IllegalArgumentException(noSuchAlgorithmException);
		}
		catch(IOException ioException)
		{
			throw new SyntaxException(header.toString(), ioException);
		}
	}

	/**Parses an HTTP header and returns the WWW-Authenticate challenge.
	@param header The header value.
	@return The challenge from the authenticate header.
	@exception SyntaxException if the given header did not contain valid information.
	@see HTTPConstants#WWW_AUTHENTICATE_HEADER
	*/
	public static AuthenticateChallenge parseWWWAuthenticateHeader(final CharSequence header) throws SyntaxException, IllegalArgumentException
	{
		try
		{
			final int schemeDelimiterIndex=indexOf(header, SP);	//find the space between the scheme and the rest of the credentials
			if(schemeDelimiterIndex>=0)	//if we found the scheme delimiter
			{
				final String scheme=header.subSequence(0, schemeDelimiterIndex).toString();	//get the scheme
				final String parameters=header.subSequence(schemeDelimiterIndex+1, header.length()).toString();	//get the rest of the credentials
				switch(AuthenticationScheme.valueOf(scheme.toUpperCase()))	//see which type of authentication scheme this is
				{
					case DIGEST:
						{
							final Map<String, String> parameterMap=parseParameterMap(new ParseReader(parameters));	//parse the parameters into a map
Debug.trace("parameter map", parameterMap);
							final String realm=parameterMap.get(REALM_PARAMETER);	//get the realm
							if(realm==null)	//if no realm is present
							{
								throw new SyntaxException(header.toString(), WWW_AUTHENTICATE_HEADER+" missing parameter "+REALM_PARAMETER);
							}
//TODO implement domain
							final String nonce=parameterMap.get(NONCE_PARAMETER);	//get the nonce
							if(nonce==null)	//if no nonce is present
							{
								throw new SyntaxException(header.toString(), WWW_AUTHENTICATE_HEADER+" missing parameter "+NONCE_PARAMETER);
							}
							final String opaque=parameterMap.get(OPAQUE_PARAMETER);	//get the opaque parameter
							final String staleString=parameterMap.get(STALE_PARAMETER);	//get the stale parameter
							final Boolean stale=parseBoolean(staleString);	//get the staleness parameter
							final String algorithm=parameterMap.get(ALGORITHM_PARAMETER);	//get the algorithm
							final String qopOptionsString=parameterMap.get(QOP_PARAMETER);	//get the quality of protection
//TODO implement auth-param
							final DigestAuthenticateChallenge digestChallenge=new DigestAuthenticateChallenge(realm, nonce, opaque, algorithm);	//create the challenge
							if(stale!=null)	//if a stale parameter was given
							{
								digestChallenge.setStale(stale);	//set the staleness
							}
							if(qopOptionsString!=null)	//if quality of protection was indicated
							{
								final String[] qopOptionsStrings=parseList(new ParseReader(qopOptionsString));	//parse the quality of protection identifiers
								final QOP[] qopOptions=new QOP[qopOptionsStrings.length];	//create an array of quality of protection enums
								for(int i=qopOptions.length-1; i>=0; --i)	//look at each quality of protection string
								{
									qopOptions[i]=QOP.valueOfString(qopOptionsStrings[i]);	//convert this quality of protection string to an enum
								}
								digestChallenge.setQOPOptions(qopOptions);	//set the quality of protection options								
							}
							return digestChallenge;
						}
					default:	//if we don't support this authentication scheme
						return null;	//show that we don't support this authentication scheme TODO fix for BASIC and other schemes
				}
			}
			else	//if no scheme delimiter was found
			{
				throw new SyntaxException(header.toString(), AUTHORIZATION_HEADER+" missing scheme delimiter.");
			}
		}
		catch(final NoSuchAlgorithmException noSuchAlgorithmException)	//if the algorithm was not supported
		{
			throw new IllegalArgumentException(noSuchAlgorithmException);
		}
		catch(IOException ioException)
		{
			throw new SyntaxException(header.toString(), ioException);
		}
	}

}