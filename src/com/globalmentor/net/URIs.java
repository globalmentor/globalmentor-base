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

import java.io.*;
import java.net.*;
import java.util.*;

import javax.mail.internet.ContentType;

import com.globalmentor.io.*;
import static com.globalmentor.io.FileConstants.EXTENSION_SEPARATOR;
import com.globalmentor.java.Integers;

import static com.globalmentor.java.CharSequences.*;
import static com.globalmentor.java.Objects.*;
import com.globalmentor.text.*;
import static com.globalmentor.text.CharacterEncoding.*;
import com.globalmentor.text.unicode.UnicodeCharacter;
import com.globalmentor.util.*;

/**Various URI manipulating functions for working with URIs as defined in
	<a href="http://www.ietf.org/rfc/rfc2396.txt">RFC 2396</a>,
	"Uniform Resource Identifiers (URI): Generic Syntax".
@see URI
*/
public class URIs
{

	/**The shared static empty array of URIs.*/
	public final static URI[] NO_URIS=new URI[0];

	/**The file scheme identifier.*/
	public final static String FILE_SCHEME="file";

	/**The FTP scheme identifier.*/
	public final static String FTP_SCHEME="ftp";

	/**The email address scheme identifier.*/
	public final static String MAILTO_SCHEME="mailto";

	/**The info scheme identifier.*/
	public final static String INFO_SCHEME="info";

	/**The delimiter separating the info scheme namespace from the rest of the info scheme-specific part.*/
	public final static char INFO_SCHEME_NAMESPACE_DELIMITER='/';

	/**The path scheme identifier for representing relative and absolute URI paths.*/
	public final static String PATH_SCHEME="path";

	/**The resource scheme identifier "resource".*/
	public final static String RESOURCE_SCHEME="resource";

	/**The URN scheme identifier "urn".*/
	public final static String URN_SCHEME="urn";

	/**The colon character (':') that separates a URI schema from the rest of the URI.*/
	public final static char SCHEME_SEPARATOR=':';

	/**The prefix string that introduces an authority.*/
	public final static String AUTHORITY_PREFIX="//";

	/**The at sign ('@') that separates user information from a host in a URI.*/
	public final static char USER_INFO_SEPARATOR='@';

	/**The colon character (':') that separates a host from a port.*/
	public final static char PORT_SEPARATOR=':';

	/**The slash character ('/') that separates components in a URI path.*/
	public final static char PATH_SEPARATOR='/';

	/**The URI path segment that represents the current hierarchical level of a hierarchical URI.
	*/
	public final static String CURRENT_LEVEL_PATH_SEGMENT=".";

	/**The URI path segment that represents the parent hierarchical level of
		a hierarchical URI.
	*/
	public final static String PARENT_LEVEL_PATH_SEGMENT="..";

	/**The character used to separate an extension from the rest of a name.*/
	public final static char NAME_EXTENSION_SEPARATOR='.';

	/**The character that separates the query from the rest of a URI.*/
	public final static char QUERY_SEPARATOR='?';

	/**The character that separates each name-value pair in a query.*/
	public final static char QUERY_NAME_VALUE_PAIR_DELIMITER='&';

	/**The character that represents assigning a value to a name in a query.*/
	public final static char QUERY_NAME_VALUE_ASSIGNMENT='=';

	/**The pound character ('#') that separates a fragment from the rest of a URI.*/
	public final static char FRAGMENT_SEPARATOR='#';

	/**The domain "localhost".*/
	public final static String LOCALHOST_DOMAIN="localhost"; //TODO eventually make a separate Domain class

	/**The path to root, consisting of a single path separator ("/").*/
	public final static String ROOT_PATH=String.valueOf(PATH_SEPARATOR);

	/**The character separating a <code>mailto</code> URI username from the domain.*/
	public final static char MAILTO_USERNAME_DOMAIN_SEPARATOR='@'; //TODO reuse EmailAddress definition

	/**Alphabetic characters as defined by RFC 2396.*/
	public final static String ALPHA_CHARS="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"; //length 52

	/**Digit characters as defined by RFC 2396.*/
	public final static String DIGIT_CHARS="0123456789"; //length 10

	/**Safe characters as defined by RFC 2396.*/
	public final static String SAFE_CHARS="$-_@.&"; //length 6

	/**Extra characters as defined by RFC 2396.*/
	public final static String EXTRA_CHARS="!*\"'(),"; //length 7

	/**The character to use for escaping URI data as defined by RFC 2396.*/
	public final static char ESCAPE_CHAR='%'; //length 1

	/**Reserved characters as defined by RFC 2396.*/
	public final static String RESERVED_CHARS="=;/#?: "; //length 7

	/**Characters that can appear in a URI as defined by RFC 2396.*/
	public final static String URI_CHARS=ALPHA_CHARS+DIGIT_CHARS+SAFE_CHARS
			+EXTRA_CHARS+ESCAPE_CHAR+RESERVED_CHARS; //length 83

	/**Characters that can appear in a URI path with no escape sequences.*/
	public final static String NORMAL_CHARS=ALPHA_CHARS+DIGIT_CHARS+SAFE_CHARS
			+EXTRA_CHARS; //length 76

	/**Unreserved characters defined by RFC 3986.*/
	public final static String UNRESERVED_CHARS=ALPHA_CHARS+DIGIT_CHARS+"-._~";

	/**General delimiter characters defined by RFC 3986.*/
	public final static String GEN_DELIM_CHARACTERS=":/?#[]@";

	/**Subdelimiter characters defined by RFC 3986.*/
	public final static String SUB_DELIM_CHARACTERS="!$&'()*+,;=";

	/**Path segment characters defined by RFC 3986.*/
	public final static String PATH_SEGMENT_CHARACTERS=UNRESERVED_CHARS
			+SUB_DELIM_CHARACTERS+":@";

	/**Path characters defined by RFC 3986.*/
	public final static String PATH_CHARACTERS=PATH_SEGMENT_CHARACTERS+'/';

	/**The maximum URL length allowed by Microsoft Internet Explorer for HTTP GET.
	@see <a href="http://support.microsoft.com/default.aspx?scid=kb;EN-US;q208427">Maximum URL length is 2,083 characters in Internet Explorer</a>
	*/
	public final static int MICROSOFT_INTERNET_EXPLORER_MAXIMUM_URI_LENGTH=2083;

	/**Creates a string of type <code>text/uri-list</code> as defined in <a href="http://www.ietf.org/rfc/rfc2483.txt">RFC 2483</a>, "URI Resolution Services Necessary for URN Resolution".
	@param uris The URIs to include in the list.
	@return A URI list string.
	@see <a href="http://www.ietf.org/rfc/rfc2483.txt">RFC 2483: URI Resolution Services Necessary for URN Resolution</a> 
	*/
	public static String createURIList(final URI... uris)
	{
		return TextFormatter
				.formatList(new StringBuilder(), "\r\n", (Object[])uris).toString(); //create the URI list
	}

	/**Verifies that the given URI has the indicated scheme.
	@param uri The URI to check.
	@param scheme The scheme to match for the URI.
	@return The given URI.
	@exception NullPointerException if the given URI and/or scheme is <code>null</code>.
	@exception IllegalArgumentException if the scheme of the given URI does not match the given scheme.
	*/
	public final static URI checkScheme(final URI uri, final String scheme)
	{
		if(!scheme.equals(uri.getScheme())) //if the URI's scheme doesn't match the given scheme
		{
			throw new IllegalArgumentException("Scheme of URI "+uri+" must be "
					+scheme);
		}
		return uri; //return the URI
	}

	/**Verifies that the given URI is an {@value URIs#INFO_SCHEME} scheme URI with the given namespace.
	@param uri The URI to check.
	@param infoNamespace The info namespace to match for the URI.
	@return The given URI.
	@exception NullPointerException if the given URI and/or info namespace is <code>null</code>.
	@exception IllegalArgumentException if the scheme of the given URI is not {@value URIs#INFO_SCHEME} and/or the info namespace does not match the given info namespace.
	*/
	public final static URI checkInfoNamespace(final URI uri,
			final String infoNamespace)
	{
		if(!checkScheme(uri, URIs.INFO_SCHEME).getRawSchemeSpecificPart()
				.startsWith(infoNamespace+URIs.INFO_SCHEME_NAMESPACE_DELIMITER)) //check for the info scheme; if the scheme-specific part is not what was expected
		{
			throw new IllegalArgumentException("Info namespace of URI "+uri
					+" must be "+infoNamespace);
		}
		return uri; //return the URI
	}

	/**Determines the info namespace of the given {@value URIs#INFO_SCHEME} scheme URI.
	@param uri The URI from which the info namespace should be retrieved.
	@return The info namespace of the given info URI.
	@exception NullPointerException if the given URI is <code>null</code>.
	@exception IllegalArgumentException if the given URI is not a valid {@value URIs#INFO_SCHEME} scheme URI.
	*/
	public final static String getInfoNamespace(final URI uri)
	{
		final String ssp=checkScheme(uri, URIs.INFO_SCHEME)
				.getRawSchemeSpecificPart(); //get the raw scheme-specific part after checking to make sure this is an info URI
		final int namespaceDelimiterIndex=ssp
				.indexOf(URIs.INFO_SCHEME_NAMESPACE_DELIMITER); //get the index of the info URI namespace delimiter
		if(namespaceDelimiterIndex<1) //if there is no namespace delimiter, or there are no namespace characters
		{
			throw new IllegalArgumentException("info URI "+uri
					+" missing delimited namespace.");
		}
		return ssp.substring(0, namespaceDelimiterIndex); //return the namespace
	}

	/**Determines the info indentifier of the given {@value URIs#INFO_SCHEME} scheme URI.
	@param uri The URI from which the info identifier should be retrieved.
	@return The decoded info identifier of the given info URI.
	@exception NullPointerException if the given URI is <code>null</code>.
	@exception IllegalArgumentException if the given URI is not a valid {@value URIs#INFO_SCHEME} scheme URI.
	*/
	public final static String getInfoIdentifier(final URI uri)
	{
		return uriDecode(getInfoRawIdentifier(uri)); //decode the raw identifier of the info URI
	}

	/**Determines the raw, encoded info indentifier of the given {@value URIs#INFO_SCHEME} scheme URI.
	@param uri The URI from which the info identifier should be retrieved.
	@return The raw, encoded info identifier of the given info URI.
	@exception NullPointerException if the given URI is <code>null</code>.
	@exception IllegalArgumentException if the given URI is not a valid {@value URIs#INFO_SCHEME} scheme URI.
	*/
	public final static String getInfoRawIdentifier(final URI uri)
	{
		final String ssp=checkScheme(uri, URIs.INFO_SCHEME)
				.getRawSchemeSpecificPart(); //get the raw scheme-specific part after checking to make sure this is an info URI
		final int namespaceDelimiterIndex=ssp
				.indexOf(URIs.INFO_SCHEME_NAMESPACE_DELIMITER); //get the index of the info URI namespace delimiter
		if(namespaceDelimiterIndex<1) //if there is no namespace delimiter, or there are no namespace characters
		{
			throw new IllegalArgumentException("info URI "+uri
					+" missing delimited namespace.");
		}
		return ssp.substring(namespaceDelimiterIndex+1); //return the identifier (the part after the namespace and delimiter)
	}

	/**Determines whether the given URI is an {@value URIs#INFO_SCHEME} scheme URI with the given namespace.
	@param uri The URI to check.
	@param infoNamespace The info namespace to match for the URI.
	@return The <code>true</code> if the given URI has a scheme of {@value URIs#INFO_SCHEME} and has the indicated info namespace.
	@exception NullPointerException if the given URI and/or info namespace is <code>null</code>.
	*/
	public final static boolean isInfoNamespace(final URI uri,
			final String infoNamespace)
	{
		return URIs.INFO_SCHEME.equals(uri.getScheme())
				&&uri.getRawSchemeSpecificPart().startsWith(
						infoNamespace+URIs.INFO_SCHEME_NAMESPACE_DELIMITER); //check for the info scheme and the info namespace
	}

	/**Determines the raw, encoded path of the given {@value #PATH_SCHEME} scheme URI.
	The path will never be <code>null</code>; the empty relative path <code>path:</code> will return the empty string.
	Any query or fragment is ignored.
	This method is needed because the {@link URI#getRawPath()} method does not recognize relative paths for the {@value #PATH_SCHEME} scheme.
	@param uri The path URI from which the path should be retrieved.
	@return The raw, encoded path of the given path URI.
	@exception NullPointerException if the given URI is <code>null</code>.
	@exception IllegalArgumentException if the given URI is not a valid {@value #PATH_SCHEME} scheme URI.
	*/
	public final static String getPathRawPath(final URI uri)
	{
		String rawPath=checkScheme(uri, PATH_SCHEME).getRawPath(); //get the raw path of the URI, ensuring that it is a "path:" URI
		if(rawPath==null) //if Java sees no path, it must be a relative path; extract it manually
		{
			rawPath=uri.getRawSchemeSpecificPart(); //the raw path is the scheme-specific part
			final int queryStart=rawPath.indexOf(URIs.QUERY_SEPARATOR); //see if this URI has a query (the scheme-specific part will not include the fragment, if any
			if(queryStart>=0) //if a query is present
			{
				rawPath=rawPath.substring(0, queryStart); //remove the query
			}
		}
		return rawPath; //return the raw path
	}

	/**Returns the path of the given {@value #PATH_SCHEME} scheme URI as a {@link URIPath}.
	@param uri The path URI from which the path should be retrieved.
	@return A URI path object representing the path of the given path URI.
	@exception NullPointerException if the given URI is <code>null</code>.
	@exception IllegalArgumentException if the given URI is not a valid {@value #PATH_SCHEME} scheme URI.
	*/
	public final static URIPath getPathURIPath(final URI uri)
	{
		return new URIPath(getPathRawPath(uri)); //get the raw path and create a URIPath from that
	}

	/**Creates a new URI identical to the supplied URI with a different raw path.
	@param uri The URI to change.
	@param newRawPath The raw, escaped path, or <code>null</code> if there should be no path.
	@return A new URI with the new raw path information.
	@exception NullPointerException if the given URI is <code>null</code>.
	@exception IllegalArgumentException if the given path results in an invalid URI.
	*/
	public static URI changeRawPath(final URI uri, final String newRawPath)
	{
		return createURI(uri.getScheme(), uri.getRawUserInfo(), uri.getHost(), uri
				.getPort(), newRawPath, uri.getRawQuery(), uri.getRawFragment()); //construct an identical URI except with a different raw path
	}

	/**Creates a new URI identical to the supplied URI with a different host.
	@param uri The URI to change.
	@param newHost The new host information.
	@return A new URI with the new host information.
	@throws NullPointerException if the given URI and/or new host is <code>null</code>.
	@throws IllegalArgumentException if the given URI has no host or if the given host results in an invalid URI.
	*/
	public static URI changeHost(final URI uri, final String newHost)
	{
		return createURI(uri.getScheme(), uri.getRawUserInfo(), checkInstance(
				newHost, "Host cannot be null."), uri.getPort(), uri.getRawPath(), uri
				.getRawQuery(), uri.getRawFragment()); //construct an identical URI except with a different host
	}

	/**Creates a new URI identical to the supplied URI with a different raw scheme-specific part.
	@param uri The URI to change.
	@param newRawSSP The raw, escaped scheme-specific part, or <code>null</code> if there should be no scheme-specific part.
	@return A new URI with the new raw scheme-specific part information.
	@exception NullPointerException if the given URI and/or scheme-specific part is <code>null</code>.
	@exception IllegalArgumentException if the given scheme-specific part results in an invalid URI.
	*/
	public static URI changeRawSchemeSpecificPart(final URI uri,
			final String newRawSSP)
	{
		final String oldRawSSP=uri.getRawSchemeSpecificPart(); //get the old raw scheme-specific part of the URI
		if(oldRawSSP.equals(newRawSSP)) //if the scheme-specific part is the same
		{
			return uri; //the URI remains unchanged
		}
		final StringBuilder stringBuilder=new StringBuilder(); //create a new string builder
		stringBuilder.append(uri.getScheme()).append(URIs.SCHEME_SEPARATOR).append(
				newRawSSP); //append the scheme and the scheme-specific part
		final String rawFragment=uri.getRawFragment(); //get the raw fragment, if any
		if(rawFragment!=null) //if there is a raw fragment
		{
			stringBuilder.append(URIs.FRAGMENT_SEPARATOR).append(rawFragment); //include the raw fragment
		}
		return URI.create(stringBuilder.toString()); //create a URI from the constructed string
	}

	/**Returns a path object to represent the path of the URI.
	@param uri The URI for which a path object should be returned.
	@return An object representing the path, or <code>null</code> if the URI has no path.
	*/
	public static URIPath getPath(final URI uri)
	{
		final String rawPath=uri.getRawPath(); //get the raw path of the URI
		return rawPath!=null ? new URIPath(rawPath) : null; //return a path object if there is a path
	}

	/**Returns the name of the resource at the given path, which will be the name of the last path component.
	If the path is a collection (i.e. it ends with slash), the component before the last slash will be returned.
	As examples, "/path/name.ext" and "name.ext" will return "name.ext". "/path/", "path/", and "path" will all return "path".
	@param path The path, which should be encoded if {@value URIs#PATH_SEPARATOR} characters are present within a path component.
	@return The name of the last last path component, the empty string if the path is the empty string, or "/" if the path is the root path.
	@exception NullPointerException if the given path is <code>null</code>.
	*/
	public static String getName(final String path)
	{
		final int length=path.length(); //get the length of the path
		if(length>0) //if there are path characters
		{
			int endIndex=length; //start at the end of the path (endIndex will always be one position after the ending character)
			if(path.charAt(endIndex-1)==URIs.PATH_SEPARATOR) //if the path ends with a path separator
			{
				--endIndex; //skip the ending path separator
			}
			final int beginIndex=path.lastIndexOf(URIs.PATH_SEPARATOR, endIndex-1)+1; //get the index after the previous separator; if there are no previous separators, this will correctly yield index 0
			if(endIndex-beginIndex>0) //if there are characters to collect (if not, this is the root path, "/")
			{
				return path.substring(beginIndex, endIndex); //return the name we found
			}
			assert URIs.ROOT_PATH.equals(path) : "Path unexpectedly not the root path.";
		}
		return path; //the path (either "" or "/") is already its name
	}

	/**Returns the raw name of the resource at the given URI's path, which will be the raw name of the last path component.
	If the path is a collection (i.e. it ends with slash), the component before the last slash will be returned.
	As examples, "/path/name.ext" and "name.ext" will return "name.ext". "/path/", "path/", and "path" will all return "path".
	This method correctly handles {@value URIs#INFO_SCHEME} URIs.
	@param URI The URI the path of which will be examined.
	@return The name of the last last path component, the empty string if the path is the empty string, "/" if the path is the root path, or <code>null</code> if the URI has no path.
	@exception NullPointerException if the given URI is <code>null</code>.
	*/
	public static String getRawName(final URI uri) //TODO important: update all references to check for null
	{
		final String rawPath=uri.isOpaque()
				&&URIs.INFO_SCHEME.equals(uri.getScheme()) ? uri
				.getRawSchemeSpecificPart() : uri.getRawPath(); //get the raw path, using the scheme-specific part of any info URI
		return rawPath!=null ? getName(rawPath) : null; //if we have a raw path, return the name
	}

	/**Returns the decoded name of the resource at the given URI's path, which will be the decoded name of the last path component.
	If the path is a collection (i.e. it ends with slash), the component before the last slash will be returned.
	As examples, "/path/name.ext" and "name.ext" will return "name.ext". "/path/", "path/", and "path" will all return "path".
	An empty name is never returned; <code>null</code> will be returned instead.
	The path name is first extracted from the URI's raw path and then decoded so that encoded {@value URIs#PATH_SEPARATOR} characters will not prevent correct parsing. 
	This method correctly handles {@value URIs#INFO_SCHEME} URIs.
	@param URI The URI the path of which will be examined.
	@return The name of the last path component, the empty string if the path is the empty string, "/" if the path is the root path, or <code>null</code> if the URI has no path.
	@exception NullPointerException if the given URI is <code>null</code>.
	*/
	public static String getName(final URI uri) //TODO important: update all references to check for null
	{
		final String rawName=getRawName(uri); //get the raw name of the URI
		return rawName!=null ? uriDecode(rawName) : null; //if there is a raw name, decode and return it
	}

	/**Changes the name of the path of the given URI to the given name.
	If the path is a collection (i.e. it ends with slash), the name is the last component before the last slash.
	As examples, "/path/name.ext" and "name.ext" will change "name.ext". "/path/", "path/", and "path" will all change "path".
	"" with return "name" and "/" will return "/name/"
	@param path The path, which should be encoded if {@value URIs#PATH_SEPARATOR} characters are present.
	@param name The new name of the path.
	@return A new path with the name changed to the given name.
	@exception NullPointerException if the given path and/or name is <code>null</code>.
	@see #getName(String)
	*/
	public static String changeName(final String path, final String name)
	{
		checkInstance(name, "Name cannot be null."); //TODO check to see if the name has illegal characters
		final int length=checkInstance(path, "Path cannot be null.").length(); //get the length of the path
		if(length==0) //if there are no characters
		{
			return name; //the empty path becomes the name itself
		}
		int endIndex=length; //start at the end of the path (endIndex will always be one position after the ending character)
		if(path.charAt(endIndex-1)==URIs.PATH_SEPARATOR) //if the path ends with a path separator
		{
			--endIndex; //skip the ending path separator
		}
		final int beginIndex=path.lastIndexOf(URIs.PATH_SEPARATOR, endIndex-1)+1; //get the index after the previous separator; if there are no previous separators, this will correctly yield index 0
		final StringBuilder pathStringBuilder=new StringBuilder(path); //create a new string builder from the given path
		if(endIndex-beginIndex>1) //if there are characters to collect (there must be more than one position difference in the start and end positions, because the end position is the index after the last character)
		{
			pathStringBuilder.replace(beginIndex, endIndex, name); //replace the found name with the new name
		}
		else
		//if there are no characters to collect, this must be the root path ("/")
		{
			assert URIs.ROOT_PATH.equals(path) : "Path unexpectedly not the root path.";
			pathStringBuilder.append(name).append(URIs.PATH_SEPARATOR); //append "name/" to the root path to yield "/name/"
		}
		return pathStringBuilder.toString(); //return the new path we determined
	}

	/**Changes the raw name of the path of the given URI to the given raw name.
	If the path is a collection (i.e. it ends with slash), the name is the last component before the last slash.
	As examples, "/path/name.ext" and "name.ext" will change "name.ext". "/path/", "path/", and "path" will all change "path".
	"" with return "name" and "/" will return "/name/"
	This method correctly handles {@value URIs#INFO_SCHEME} URIs.
	@param uri The URI the raw name of which to change.
	@param rawName The new raw name of the URI.
	@return A new URI with the raw name changed to the given raw name.
	@exception NullPointerException if the given URI and/or name is <code>null</code>.
	@exception IllegalArgumentException if the given URI has no path.
	@see #getRawName(URI)
	*/
	public static URI changeRawName(final URI uri, final String rawName)
	{
		if(uri.isOpaque()&&URIs.INFO_SCHEME.equals(uri.getScheme())) //if this is an info URI
		{
			final String rawSSP=uri.getRawSchemeSpecificPart(); //get the raw scheme-specific part
			final String newRawSSP=changeName(rawSSP, rawName); //change the name to the given name
			return changeRawSchemeSpecificPart(uri, newRawSSP); //change the URI's scheme-specific part to the new scheme-specific part			
		}
		else
		//if this is not an info URI
		{
			final String rawPath=checkInstance(uri, "URI cannot be null")
					.getRawPath(); //get the raw path
			if(rawPath==null) //if the URI has no path
			{
				throw new IllegalArgumentException("URI "+uri+" has no path.");
			}
			final String newRawPath=changeName(rawPath, rawName); //change the name to the given name
			return changeRawPath(uri, newRawPath); //change the URI's raw path to the new raw path
		}
	}

	/**Changes the name of the path of the given URI to the given name.
	If the path is a collection (i.e. it ends with slash), the name is the last component before the last slash.
	As examples, "/path/name.ext" and "name.ext" will change "name.ext". "/path/", "path/", and "path" will all change "path".
	"" with return "name" and "/" will return "/name/"
	This method correctly handles {@value URIs#INFO_SCHEME} URIs.
	@param uri The URI the name of which to change.
	@param name The new unencoded name of the URI, which will be encoded.
	@return A new URI with the name changed to the given name.
	@exception NullPointerException if the given URI and/or name is <code>null</code>.
	@exception IllegalArgumentException if the given URI has no path.
	@see URIPath#encodeSegment(String)
	@see #getName(URI)
	*/
	public static URI changeName(final URI uri, final String name)
	{
		return changeRawName(uri, URIPath.encodeSegment(name)); //encode the name and change the name of the URI's path
	}

	/**Adds the given extension to a name and returns the new name with the new extension.
	The name is not checked to see if it currently has an extension.
	@param name The name to which to add an extension.
	@param extension The extension to add.
	@return The name with the new extension.
	@exception NullPointerException if the given extension is <code>null</code>.
	*/
	public static String addNameExtension(final String name,
			final String extension)
	{
		return new StringBuilder(name).append(EXTENSION_SEPARATOR).append(
				checkInstance(extension, "Extension cannot be null")).toString(); //add the requested extension and return the new filename
	}

	/**Extracts the extension from a name.
	@param name The URI name to examine.
	@return The extension of the name (not including '.'), or <code>null</code> if no extension is present.
	*/
	public static String getNameExtension(final String name)
	{
		final int separatorIndex=name.lastIndexOf(EXTENSION_SEPARATOR); //see if we can find the extension separator, which will be the last such character in the string
		return separatorIndex>=0 ? name.substring(separatorIndex+1) : null; //if we found a separator, return everything after it 
	}

	/**Changes the extension of a name and returns a new name with the new extension.
	If the name does not currently have an extension, one will be added.
	@param name The name to examine.
	@param extension The extension to set, or <code>null</code> if the extension should be removed.
	@return The name with the new extension.
	*/
	public static String changeNameExtension(String name, final String extension)
	{
		final int separatorIndex=name.lastIndexOf(EXTENSION_SEPARATOR); //see if we can find the extension separator
		if(separatorIndex>=0) //if we found a separator
		{
			name=name.substring(0, separatorIndex); //remove the extension
		}
		if(extension!=null) //if an extension was given
		{
			name=addNameExtension(name, extension); //add the requested extension
		}
		return name; //return the new filename
	}

	/**Adds the extension, if any, of a name and returns the new name.
	This is a convenience method that delegates to {@link #addNameExtension(String, String)} if a non-<code>null</code> extension is given.
	@param name The name to examine.
	@param extension The extension to add, or <code>null</code> if no extension should be added.
	@return The name with the new extension, if any.
	*/
	public static String setNameExtension(final String name,
			final String extension)
	{
		return extension!=null ? addNameExtension(name, extension) : name; //if an extension was given, add it; otherwise, return the name unmodified
	}

	/**Removes the extension, if any, of a name and returns a new name with no extension.
	This is a convenience method that delegates to {@link #changeNameExtension(String, String)}.
	@param name The name to examine.
	@return The name with no extension.
	*/
	public static String removeNameExtension(final String name)
	{
		return changeNameExtension(name, null); //replace the extension with nothing
	}

	/**Creates a new URI identical to the supplied URI with no query or fragment.
	@param uri The URI from which to remove the query and fragment, if any.
	@return A new URI with no query or fragment.
	*/
	public static URI getPlainURI(final URI uri)
	{
		return createURI(uri.getScheme(), uri.getRawUserInfo(), uri.getHost(), uri
				.getPort(), uri.getRawPath(), null, null); //construct an identical URI except with no query or fragment
	}

	/**Constructs an absolute path from the given elements in the form:
	<code>/<var>element1</var>/<var>element2</var></code>.
	Each element in the path is URI-encoded using UTF-8.
	@param absolute <code>true</code> if the path should be absolute
		and therefore should begin with '/'.
	@param pathElements <code>true</code> if the path represents a collection
		and therefore should end with '/'. 
	@return A path constructed according to the given rules.
	@exception IllegalArgumentException if there are no path elements and an
		absolute non-collection or non-absolute collection is requested.
	*/
	public static String constructPath(final boolean absolute,
			final boolean collection, final String... pathElements)
	{
		if(pathElements.length==0&&absolute!=collection) //if there are no path elements, an absolute URI must also be a collection
		{
			throw new IllegalArgumentException(
					"A path with no elements must be an absolute collection or a relative non-collection.");
		}
		final StringBuilder stringBuilder=new StringBuilder(); //create a string builder
		if(absolute) //if this should be an absolute path
		{
			stringBuilder.append(URIs.PATH_SEPARATOR); //prepend '/'
		}
		boolean hasPath=false; //don't assume we have any path elements
		for(final String pathElement : pathElements) //look at each path element
		{
			//G***fix			try
			{
				//TODO fix encoding using a real encoder, not the www-encoding URLEncoder				stringBuilder.append(encode(pathElement, UTF_8));	//encode and append this path element
				stringBuilder.append(pathElement); //encode and append this path element
				stringBuilder.append(URIs.PATH_SEPARATOR); //separate the path elements
			}
			/*G***fix
						catch(final UnsupportedEncodingException unsupportedEncodingException)	//we should always support UTF-8
						{
							throw new AssertionError(unsupportedEncodingException);
						}
			*/
		}
		if(!collection&&pathElements.length>0) //if there were path elements but this wasn't a collection, we have one too many path separators 
		{
			stringBuilder.deleteCharAt(stringBuilder.length()-1); //remove the last character, a '/'
		}
		return stringBuilder.toString(); //return the string version of the constructed path
	}

	/**Constructs a query string for a URI by URI-encoding each name-value pair,
	 	separating them with '&', and prepending the entire string (if there is
	 	at least one parameter) with '?'.
	@param params The name-value pairs representing the query parameters.
	@return A string representing the constructed query, or the empty string if
		there were no parameters.
	*/
	public static String constructQuery(
			final NameValuePair<String, String>... params)
	{
		return constructQuery(constructQueryParameters(params)); //construct a query, prepended with the query character
	}

	/**Constructs a query string for a URI by prepending the given query string, if it is not the empty string, with '?'.
	@param params The string representing the query parameters.
	@return A string representing the constructed query, or the empty string if there were no parameters.
	*/
	public static String constructQuery(final String params)
	{
		final StringBuilder query=new StringBuilder();
		if(params.length()>0) //if there is at least one parameter character
		{
			query.append(URIs.QUERY_SEPARATOR); //append the query prefix
			query.append(params); //append the params
		}
		return query.toString(); //return the query string we constructed
	}

	/**Appends a query string to a URI.
	@param uri The existing URI.
	@param rawQuery The encoded query information, without a beginning query separator.
	@return A URI representing the URI with the appended query parameters.
	@exception NullPointerException if the given URI and/or query is <code>null</code>.
	*/
	public static URI appendRawQuery(final URI uri, final String rawQuery)
	{
		final StringBuilder stringBuilder=new StringBuilder(uri.toString()); //create a string builder from the URI
		stringBuilder
				.append(uri.getRawQuery()!=null ? URIs.QUERY_NAME_VALUE_PAIR_DELIMITER
						: URIs.QUERY_SEPARATOR); //if there already is a query, separate the new parameters from the existing ones; otherwise, add the query introduction character
		stringBuilder.append(checkInstance(rawQuery, "Query cannot be null.")); //add the new query information
		return URI.create(stringBuilder.toString()); //return the new URI
	}

	/**Constructs a query string for a URI and appends it to the query of the given URI, if any.
	@param uri The existing URI.
	@param params The name-value pairs representing the query parameters.
	@return A URI representing the URI with the appended query parameters.
	@exception NullPointerException if the given URI and/or params is <code>null</code>.
	*/
	public static URI appendQueryParameters(final URI uri,
			final NameValuePair<String, String>... params)
	{
		if(params.length>0) //if there are parameters
		{
			return appendRawQuery(uri, constructQueryParameters(params)); //add the new query parameters and return the resulting URI
		}
		else
		//if there are no parameters
		{
			return uri; //reuturn the URI as-is
		}
	}

	/**Constructs a query string for a URI and appends it to the given query, if any.
	@param query The existing query paramters, or <code>null</code> or the empty string if there is no query.
	@param params The name-value pairs representing the query parameters.
	@return A string representing the query with the appended parameters, or the empty string if there was no query and there were no parameters.
	*/
	public static String appendQueryParameters(final String query,
			final NameValuePair<String, String>... params)
	{
		final String queryParameters=constructQueryParameters(params); //get query parameters
		return query!=null&&query.length()>0 ? query
				+URIs.QUERY_NAME_VALUE_PAIR_DELIMITER+queryParameters : queryParameters; //if there was a query, append the new parameters; otherwise, just return the parameters
	}

	/**Constructs a query string for a URI by URI-encoding each name-value pair, separating them with '&'.
	@param params The name-value pairs representing the query parameters.
	@return A string representing the constructed query, or the empty string if there were no parameters.
	*/
	public static String constructQueryParameters(
			final NameValuePair<String, String>... params)
	{
		final StringBuilder paramStringBuilder=new StringBuilder();
		if(params.length>0) //if there is at least one parameter
		{
			for(NameValuePair<String, String> param : params) //look at each parameter
			{
				paramStringBuilder.append(encode(param.getName())); //append the parameter name
				paramStringBuilder.append(URIs.QUERY_NAME_VALUE_ASSIGNMENT); //append the value-assignment character
				paramStringBuilder.append(encode(param.getValue())); //append the parameter value
				paramStringBuilder.append(URIs.QUERY_NAME_VALUE_PAIR_DELIMITER); //append the name-value pair delimiter
			}
			paramStringBuilder.delete(paramStringBuilder.length()-1,
					paramStringBuilder.length()); //remove the last name-value pair delimiter
		}
		return paramStringBuilder.toString(); //return the query parameter string we constructed
	}

	/**Creates a path-based query from a standard URI query.
	A query in the form <code>?var1=value1&amp;var2=value2</code> will be converted to the form
		<code>/var1%3Dvalue1/var2&3Dvalue2</code>.
	@param query The standard URI query string, optionally beginning with '?'.
	@return A query string converted to a path. A query string beginning with '?' will be converted into
		an absolute path.
	*/
	/*TODO fix; do *not* use URLEncoder, which is for www-encoded forms	
		public static String createPathQuery(final String query)
		{
			if(query.length()>0)	//if the query has at least one character
			{
				final StringBuilder stringBuilder=new StringBuilder();	//create a string builder for creating a path query
				final int startIndex;	//find out if we should skip the first character			
				if(query.charAt(0)==QUERY_SEPARATOR)	//if the first character is '?'
				{
					stringBuilder.append(PATH_SEPARATOR);	//convert it to a '/'
					startIndex=1;	//skip the introductory character
				}
				else	//if the string doesn't begin with '?'
				{
					startIndex=0;	//we'll just start at the first
				}
					//tokenize the string on the attribute delimiter, '&'
				final StringTokenizer stringTokenizer=new StringTokenizer(query.substring(startIndex), String.valueOf(QUERY_NAME_VALUE_PAIR_DELIMITER));
				while(stringTokenizer.hasMoreTokens())	//while there are more tokens
				{
					final String token=stringTokenizer.nextToken();	//get the next token
					try
					{
						stringBuilder.append(URLEncoder.encode(token, UTF_8));	//encode and append the next token
					}
					catch(UnsupportedEncodingException unsupportedEncodingException)	//we should always support UTF-8
					{
						throw new AssertionError(unsupportedEncodingException);
					}					
					if(stringTokenizer.hasMoreTokens())	//if there are more tokens
					{
						stringBuilder.append(PATH_SEPARATOR);	//add a path separator, '/'					
					}
				}
				return stringBuilder.toString();	//return the string we constructed
			}
			else	//if the query is empty
			{
				return query;	//return the query as it is
			}		
		}
	*/

	/**Creates a standard URI based query from a path-based query.
	A query in the form <code>/var1%3Dvalue1/var2&3Dvalue2</code> will be converted to the form
		<code>?var1=value1&amp;var2=value2</code>.
	@param pathQuery The standard URI query string, optionally beginning with '?'.
	@return A query string converted to a path. A query string beginning with '?' will be converted into
		an absolute path.
	*/
	/*TODO fix; do *not* use URLEncoder, which is for www-encoded forms	
		public static String createQuery(final String pathQuery)
		{
			if(pathQuery.length()>0)	//if the query has at least one character
			{
				final StringBuilder stringBuilder=new StringBuilder();	//create a string builder for creating a query
				final int startIndex;	//find out if we should skip the first character			
				if(pathQuery.charAt(0)==PATH_SEPARATOR)	//if the first character is '/'
				{
					stringBuilder.append(QUERY_SEPARATOR);	//convert it to a '?'
					startIndex=1;	//skip the introductory character
				}
				else	//if the string doesn't begin with '/'
				{
					startIndex=0;	//we'll just start at the first
				}
					//tokenize the string on path separators, '/'
				final StringTokenizer stringTokenizer=new StringTokenizer(pathQuery.substring(startIndex), String.valueOf(PATH_SEPARATOR));
				while(stringTokenizer.hasMoreTokens())	//while there are more tokens
				{
					final String token=stringTokenizer.nextToken();	//get the next token
					try
					{
						stringBuilder.append(URLDecoder.decode(token, UTF_8));	//encode and append the next token
					}
					catch(UnsupportedEncodingException unsupportedEncodingException)	//we should always support UTF-8
					{
						throw new AssertionError(unsupportedEncodingException);
					}					
					if(stringTokenizer.hasMoreTokens())	//if there are more tokens
					{
						stringBuilder.append(QUERY_NAME_VALUE_PAIR_DELIMITER);	//add a query name/value pair separator, '&'					
					}
				}
				return stringBuilder.toString();	//return the string we constructed
			}
			else	//if the path query is empty
			{
				return pathQuery;	//return the path query as it is
			}		
		}
	*/

	/**Retrieves name-value parameters from a standard URI query string.
	@param query The URI query string, optionally beginning with a '?' character.
	@return An array of name-value pairs representing query parameters.
	*/
	/*TODO fix; do *not* use URLEncoder, which is for www-encoded forms	
		public static NameValuePair<String, String>[] getQueryParameters(final String query)
		{
			final List<NameValuePair<String, String>> parameterList=new ArrayList<NameValuePair<String, String>>();	//create a list to hold our parameters
			if(query.length()>0)	//if the query has at least one character
			{
				final int startIndex;	//find out if we should skip the first character			
				if(query.charAt(0)==QUERY_SEPARATOR)	//if the first character is '?'
				{
					startIndex=1;	//skip the introductory character
				}
				else	//if the string doesn't begin with '?'
				{
					startIndex=0;	//we'll just start at the first
				}
					//tokenize the string on the attribute delimiter, '&'
				final StringTokenizer stringTokenizer=new StringTokenizer(query.substring(startIndex), String.valueOf(QUERY_NAME_VALUE_PAIR_DELIMITER));
				while(stringTokenizer.hasMoreTokens())	//while there are more tokens
				{
					final String token=stringTokenizer.nextToken();	//get the next token
					final int equalsIndex=token.indexOf(QUERY_NAME_VALUE_ASSIGNMENT);	//get the index of the '=' character
					final String name;	//we'll determine the name and the value
					final String value;
					if(equalsIndex>=0)	//if there is an equals character
					{
						name=token.substring(0, equalsIndex);	//the name is everything up to but not including the '='
						value=token.substring(equalsIndex+1);	//the value is everything after the '='
					}
					else	//if there is no equals character
					{
						name=token;	//take the token as it is for the name
						value=null;	//there is no value G***is this the correct thing to do? should it be ""?
					}
					parameterList.add(new NameValuePair<String, String>(name, value));	//add this parameter to the list 
				}
			}
			return parameterList.toArray(new NameValuePair[parameterList.size()]);	//return the list as an array
		}
	*/

	/**Retrieves the parameters from the query of a URI, if present.
	@param uri The URI from which to extract parameters.
	@return An array of parameters.
	*/
	public static CollectionMap<String, String, List<String>> getParameterMap(
			final URI uri)
	{
		final NameValuePair<String, String>[] parameters=getParameters(uri); //get the parameters from the URI
		final CollectionMap<String, String, List<String>> parameterListMap=new ArrayListHashMap<String, String>(); //create a new list map in which to store the parameters
		if(parameters!=null) //if this URI specified a query
		{
			for(final NameValuePair<String, String> parameter : parameters) //for each parameter
			{
				parameterListMap.addItem(parameter.getName(), parameter.getValue()); //add this name and value, each of which may have been encoded
			}
		}
		return parameterListMap; //return the parameters, if any
	}

	/**Retrieves the query parameters from a URI.
	@param uri The URI which may contain a query.
	@return An array of parameters represented by the URI query, or <code>null</code> if the given URI does not contain a query.
	*/
	public static NameValuePair<String, String>[] getParameters(final URI uri)
	{
		return getParameters(uri.getRawQuery()); //return the paramters for this URI query, if there is a query
	}

	/**Retrieves the parameters from a URI query.
	An empty string query will return an empty array of name/value pairs.
	@param query The string containing URI query parameters (without the '?' prefix), or <code>null</code>.
	@return An array of parameters represented by the query, or <code>null</code> if the given query is <code>null</code>.
	*/
	@SuppressWarnings("unchecked")
	//we can't check the creation of a generic array
	public static NameValuePair<String, String>[] getParameters(final String query)
	{
		if(query!=null) //if a query was given
		{
			if(query.length()==0) //if there is no query in the string
			{
				return new NameValuePair[0]; //return an empty array
			}
			final String[] parameterStrings=query.split(String
					.valueOf(URIs.QUERY_NAME_VALUE_PAIR_DELIMITER)); //split the query into parameters
			final NameValuePair<String, String>[] parameters=new NameValuePair[parameterStrings.length]; //create an array to hold parameters
			int i=0;
			for(final String parameterString : parameterStrings) //for each parameters
			{
				final String[] nameValue=parameterString.split(String
						.valueOf(URIs.QUERY_NAME_VALUE_ASSIGNMENT)); //split the parameter into its name and value
				final String name; //we'll get the parameter name
				final String value; //we'll get the parameter value
				if(nameValue.length>0) //if there was at least one token
				{
					name=uriDecode(nameValue[0]); //the first token is the name
					value=nameValue.length>1 ? uriDecode(nameValue[1]) : ""; //use the empty string for the value if no value was provided
				}
				else
				//if there wasn't at least one token
				{
					name=value=""; //there is no name or value
				}
				parameters[i++]=new NameValuePair<String, String>(name, value); //create a new parameter and advance to the next index
			}
			return parameters; //return the parameters
		}
		else
		//if no query is given
		{
			return null; //there are no parameters
		}
	}

	/**Creates a URI from the given path, verifying that the string contains only a path.
	@param path The string version of a path to convert to a URI form of that same path.
	@exception NullPointerException if the given path is <code>null</code>.
	@exception IllegalArgumentException if the provided path specifies a URI scheme (i.e. the URI is absolute) and/or authority.
	@see #isPathURI(URI)
	*/
	public static URI createPathURI(final String path)
	{
		final URI pathURI=URI.create(checkInstance(path, "Path cannot be null")); //create a URI from the given path
		if(!isPathURI(pathURI)) //if there is a scheme or an authority
		{
			throw new IllegalArgumentException(
					"Path cannot have a URI scheme or authority, and must include a path: "
							+path);
		}
		return pathURI; //return the URI we created
	}

	/**Checks to see if a given URI is absolute.
	If the given URI is not absolute, an exception is thrown.
	@param uri The URI to check to see if it is absolute.
	@return The given absolute URI.
	@exception NullPointerException if the given URI is <code>null</code>.
	@exception IllegalArgumentException if the given URI is not absolute.
	@see URI#isAbsolute()
	*/
	public static URI checkAbsolute(final URI uri)
			throws IllegalArgumentException
	{
		if(!uri.isAbsolute()) //if the given URI is not absolute
		{
			throw new IllegalArgumentException("The given URI "+uri
					+" is not absolute.");
		}
		return uri; //return the absolute URI
	}

	/**Checks to see if a given path is only a path and not a URI with a scheme and/or authority.
	If the given string is not a path, an exception is thrown.
	@param path The string version of a path to determine if it is indeed only a path.
	@return The given path.
	@exception NullPointerException if the given path is <code>null</code>.
	@exception IllegalArgumentException if the given string is not a path.
	@see #isPath(String)
	*/
	public static String checkPath(final String path)
			throws IllegalArgumentException
	{
		if(!isPath(path)) //if the string is not a path
		{
			throw new IllegalArgumentException("The given string "+path
					+" is not a valid sole path.");
		}
		return path; //return the path
	}

	/**Checks to see if a given path is only a relative path and not a URI with a scheme and/or authority.
	If the given string is not a relative path, an exception is thrown.
	@param path The string version of a path to determine if it is indeed only a relative path.
	@return The given relative path.
	@exception NullPointerException if the given path is <code>null</code>.
	@exception IllegalArgumentException if the given string is not a path or the path is not relative.
	@see #isPath(String)
	*/
	public static String checkRelativePath(final String path)
			throws IllegalArgumentException
	{
		if(isAbsolutePath(checkPath(path))) //check the path; if it is a path but it is absolute
		{
			throw new IllegalArgumentException("The given path "+path
					+" is not relative.");
		}
		return path; //return the relative path
	}

	/**Determines if a given path is only a path and not a URI with a scheme and/or authority.
	@param path The string version of a path to determine if it.
	@return <code>true</code> if the path is a path and does not specifiy a scheme (i.e. the URI is not absolute) or authority.
	@exception NullPointerException if the given path is <code>null</code>.
	@see #isPathURI(URI)
	*/
	public static boolean isPath(final String path)
	{
		final URI pathURI=URI.create(checkInstance(path, "Path cannot be null")); //create a URI from the given path
		return isPathURI(pathURI); //indicate whether the constructed URI represents a path
	}

	/**Checks to see if a given URI is only a path and not a URI with a scheme, authority, query, and/or fragment.
	If the given URI is not a path, an exception is thrown.
	@param uri The URI to check to for path status.
	@return The given path URI.
	@exception NullPointerException if the given path URI is <code>null</code>.
	@exception IllegalArgumentException if the provided URI specifies a URI scheme (i.e. the URI is absolute), authority, query, and/or fragment.
	@exception IllegalArgumentException if the given URI is not a path.
	@see #isPath(String)
	*/
	public static URI checkPathURI(final URI pathURI)
	{
		if(!isPathURI(pathURI)) //if the string is not a path
		{
			throw new IllegalArgumentException("The given URI "+pathURI
					+" is not a valid sole path URI.");
		}
		return pathURI; //return the path URI
	}

	/**Determines if a given URI contains only a path and does not have a scheme, authority, query, and/or fragment.
	@param uri The URI to check to for path status.
	@exception NullPointerException if the given URI is <code>null</code>.
	@return <code>true</code> if the URI has a path and does not specifiy a scheme (i.e. the URI is not absolute), authority, query, or fragment.
	*/
	public static boolean isPathURI(final URI uri)
	{
		checkInstance(uri, "URI cannot be null");
		return uri.getScheme()==null&&uri.getRawAuthority()==null
				&&uri.getPath()!=null&&uri.getRawQuery()==null
				&&uri.getRawFragment()==null; //see if there is no scheme, no authority, a path, no query, and no fragment
	}

	/**Determines the current level of a hierarchical URI.
	This is equivalent to resolving the path {@value URIs#CURRENT_LEVEL_PATH_SEGMENT} to the URI.
	@param uri The URI to examine.
	@return A URI representing the current hierarchical level of a hierarchical URI.	
	*/
	public static URI getCurrentLevel(final URI uri)
	{
		return uri.resolve(URIs.CURRENT_LEVEL_PATH_SEGMENT); //resolve the URI to "."
	}

	/**Determines the parent level of a hierarchical URI.
	This is equivalent to resolving the path {@value URIs#PARENT_LEVEL_PATH_SEGMENT} to the URI.
	@param uri The URI to examine.
	@return A URI representing the parent hierarchical level of a hierarchical URI.	
	*/
	public static URI getParentLevel(final URI uri)
	{
		return uri.resolve(URIs.PARENT_LEVEL_PATH_SEGMENT); //resolve the URI to ".."
	}

	/**Determines the parent collection of a hierarchical URI.
	@param uri The URI to examine.
	@return A URI representing the parent collection of a hierarchical
		URI; if the URI ends in '/', equivalent to resolving the path ".." to the URI;
		if the URI does not end in '/', equivalent to resolving the path "." to the URI.	
	@exception IllegalArgumentException if the URI does not have a path component.
	*/
	public static URI getParentURI(final URI uri)
	{
		return isCollectionURI(uri) ? getParentLevel(uri) : getCurrentLevel(uri); //if the path ends with a slash, get the parent level; otherwise, get the current level
	}

	/**Determines the canonical root URI of a URI.
	@param uri The URI to examine.
	@return A URI representing the URI with no path and no query or fragment.
	*/
	public static URI getRootURI(final URI uri)
	{
		return createURI(uri.getScheme(), uri.getRawUserInfo(), uri.getHost(), uri
				.getPort(), null, null, null);
	}

	/**Returns the content type for the specified URI based on its name extension.
	@param uri The URI for which to return a content type.
	@return The default content type for the URI's name extension, or <code>null</code> if no known content type is associated with this URI's extension.
	@see Files#getExtensionContentType(String)
	@see #getRawName(URI)
	@see #getNameExtension(String)
	*/
	public static ContentType getContentType(final URI uri)
	{
		final String rawPath=uri.getRawPath(); //get the raw path
		return rawPath!=null ? Files
				.getExtensionContentType(getNameExtension(getRawName(uri))) : null; //return the content type based on the extension of the URI name, if there is one
	}

	/**Normalizes the given path by resolving the '.' and '..' path segments.
	@param path The path to normalize.
	@return The normalized form of the given path.
	@exception NullPointerException if the given path is <code>null</code>.
	@exception IllegalArgumentException if the provided path specifies a URI scheme (i.e. the URI is absolute) and/or authority.
	@see URI#normalize()
	*/
	public static String normalizePath(final String path)
	{
		return createPathURI(path).normalize().getPath(); //get a URI from the path, normalize that URI, and then return the path of the resulting URI
	}

	/**Relativizes the given full path against the given base path.
	If the full path is not composed of the given base path, the full path is returned.
	@param basePath The path against which the full path should be relativized.
	@param fullPath The full path to be relativized.
	@return A form of the full path relative to the base path.
	@exception NullPointerException if one of the given paths is <code>null</code>.
	@exception IllegalArgumentException if one of the provided path specifies a URI scheme (i.e. the URI is absolute) and/or authority.
	*/
	public static String relativizePath(final String basePath,
			final String fullPath)
	{
		final URI baseURI=createPathURI(basePath); //create a URI for the base path, ensuring it's a path
		final URI fullURI=createPathURI(fullPath); //create a URI for the full path, ensuring it's a path
		return baseURI.relativize(fullURI).getPath(); //relativize the URIs and return the path
	}

	/**Creates a URI from a URL.
	JDK 1.5 provides an equivalent {@link URL#toURI()}.
	This method is provided for backwards-compatibility using for example Retroweaver. 
	@param url The URL to convert to a URI. The URL should already be properly encoded.
	@return The URI form of the URL.
	@exception URISyntaxException Thrown if the URL could not be converted to a URI.
	*/
	/*TODO del
		public static URI createURI(final URL url) throws URISyntaxException
		{
			return new URI(url.toString());	//assuming the URL is already escaped, create a new URI from the string representation of the URL
		}
	*/

	/**Creates a URN in the form <code>urn:<var>nid</var>:nss</code>.
	@param nid The namespace identifier.
	@param nss The namespace-specific string.
	@return A URN based upon the given parameters.
	@see <a href="http://www.ietf.org/rfc/rfc2141.txt">RFC 2141</a>
	@throws IllegalArgumentException if the resulting string violates RFC 2396.
	*/
	public static URI createURN(final String nid, final String nss)
	{
		return URI.create(URIs.URN_SCHEME+URIs.SCHEME_SEPARATOR+nid
				+URIs.SCHEME_SEPARATOR+nss); //construct and return the URN
	}

	/**Creates an {@value URIs#INFO_SCHEME} URI with the given info namespace and identifier with no fragment.
	@param namespace The info namespace.
	@param rawIdentifier The raw, encoded info identifier.
	@return An info URI based upon the given parameters.
	@see <a href="http://www.ietf.org/rfc/rfc4452.txt">RFC 4452</a>
	@exception NullPointerException if the given namespace and/or identifier is <code>null</code>.
	@exception IllegalArgumentException if the given namespace, and/or identifier result in an invalid URI.
	*/
	public static URI createInfoURI(final String namespace,
			final String rawIdentifier)
	{
		return createInfoURI(namespace, rawIdentifier, null); //create an info URI with no fragment
	}

	/**Creates an {@value URIs#INFO_SCHEME} URI with the given info namespace, identifier, and optional fragment.
	@param namespace The info namespace.
	@param rawIdentifier The raw, encoded info identifier.
	@param rawFragment The raw, encoded fragment, or <code>null</code> if there should be no fragment
	@return An info URI based upon the given parameters.
	@see <a href="http://www.ietf.org/rfc/rfc4452.txt">RFC 4452</a>
	@exception NullPointerException if the given namespace and/or identifier is <code>null</code>.
	@exception IllegalArgumentException if the given namespace, identifier, and/or fragment result in an invalid URI.
	*/
	public static URI createInfoURI(final String namespace,
			final String rawIdentifier, final String rawFragment)
	{
		final StringBuilder stringBuilder=new StringBuilder(); //create a string builder
		stringBuilder.append(URIs.INFO_SCHEME).append(URIs.SCHEME_SEPARATOR)
				.append(checkInstance(namespace, "Namespace cannot be null.")).append(
						URIs.INFO_SCHEME_NAMESPACE_DELIMITER); //info:namespace/
		stringBuilder.append(rawIdentifier); //identifier
		if(rawFragment!=null) //if there is a fragment
		{
			stringBuilder.append(URIs.FRAGMENT_SEPARATOR).append(rawFragment); //#fragment
		}
		return URI.create(stringBuilder.toString()); //construct and return an info URI from the string builder
	}

	/**Creates a {@value URIs#MAILTO_SCHEME} URI in the form <code>mailto:<var>username</var>@<var>domain</var></code>.
	The username and domain will be URI-encoded.
	@param username The mail username.
	@param domain The mail domain.
	@return A <code>mailto</code> URI based upon the given parameters.
	@see <a href="http://www.ietf.org/rfc/rfc2368.txt">RFC 2368</a>
	@exception NullPointerException if the given username and/or domain is <code>null</code>.
	*/
	public static URI createMailtoURI(final String username, final String domain)
	{
		return URI.create(URIs.MAILTO_SCHEME+URIs.SCHEME_SEPARATOR+encode(username)
				+URIs.MAILTO_USERNAME_DOMAIN_SEPARATOR+encode(domain)); //construct and return the mailto URI
	}

	/**Creates a URI from the given URI string relative to the given context object.
	@param contextObject The source context, such as a URL, a URI, a File, or <code>null</code> if
		the filename should not be referenced from any object.
	@param string The string version of a URI, either relative or absolute, or a URI
		fragment beginning with "#".
	@return A URI constructed from the URI string and context object.
	@exception URISyntaxException Thrown if the context object and string cannot be used to create a valid URI.
	@see File
	@see URI
	@see URL
	*/
	public static URI createURI(final Object contextObject, final String string)
			throws URISyntaxException //TODO maybe delete this eventually
	{
		if(contextObject instanceof URI) //if the context is a URI
		{
			//TODO if the string contains illegal URI characters, such as spaces, this won't work
			//TODO also check to see if the string is null.
			return ((URI)contextObject).resolve(new URI(string)); //resolve the URI form of the string, creating a URISyntaxException if there is a problem
		}
		else if(contextObject instanceof URL) //if the context is a URL
		{
			return ((URL)contextObject).toURI().resolve(string); //convert the URL to a URI and use it as a context
		}
		else if(contextObject instanceof File) //if the context object is a file
		{
			return createURI(((File)contextObject).toURI(), string); //convert the File to a URI and use it as a context
		}
		else
		//if we don't recognize the context object
		{
			return new URI(string); //create a new URI from the string, ignoring the context object
		}
	}

	/**Creates an absolute URI from the given string, guessing what the string represents.
	<p>If the string is not a valid URL (e.g. it contains a space), this method assumes that a file was intended and a file URI is constructed.</p>
	<p>This method is convenient for creating URIs based upon user input.</p>
	@param string The string to convert to a URI. 
	@return A URI representing the contents of the string, interpreted in a
		lenient fashion.
	*/
	public static URI guessAbsoluteURI(final String string)
	{
		//TODO del Debug.trace("guessing URI: ", string);
		try
		{
			final URI uri=new URI(string); //see if the string is already a valid URI
			if(uri.isAbsolute()) //if the URI is absolute
			{
				return uri; //return the URI
			}
			else
			//if the URI is not absolute
			{
				return new File(string).toURI(); //a local file must have been requested				
			}
		}
		/*G***del if not needed
				catch(IllegalArgumentException illegalArgumentException)	//if the string is not an absolute URI
				{
					return new File(string).toURI();	//construct a file object and convert that to a URI
				}
		*/
		catch(URISyntaxException uriSyntaxException) //if the string is not a valid URI
		{
			return new File(string).toURI(); //construct a file object and convert that to an absolute URI
		}
	}

	/**Returns a URL representing the directory of the given file URL. (It is
		assumed that the given URL represents a file.)
	@param url The URL of a file.
	@return A URL of the file's directory, ending with '/'.
	@throws MalformedURLException Thrown if a directory URL cannot be created.
	*/
	public static URL getDirectoryURL(final URL url) throws MalformedURLException
	{
		return new URL(url, "."); //create a new URL from the directory of the URL G***use a constant here
	}

	/**Returns the unencoded host and optional port of the given URI.
	@param uri The URI from which to extract the host and optional port.
	@return The host name and optional port of the given URI,
		or <code>null</code> if there is no host specified in the given URI.
	*/
	public static Host getHost(final URI uri)
	{
		final String host=uri.getHost(); //get the host
		final int port=uri.getPort(); //get the port
		return host!=null ? new Host(host, port) : null; //if there is a hostname, return the host information
		/*G***del		
				if(host!=null)	//if a host is given
				{
					final int port=uri.getPort();	//get the port
					if(port>=0)	//if a port is given
					{
						return new StringBuilder(host).append(PORT_SEPARATOR).append(port).toString();	//append the port
					}
					else	//if no port is given
					{
						return host;	//just return the host
					}
				}
				else	//if no host was given
				{
					return null;	//show that no host can be returned
				}
		*/

	}

	/**Returns the unencoded path, optional unencoded query, and optional unencoded fragment of the given URI.
	@param uri The URI from which to extract the path, optional query, and optional fragment.
	@return An unencoded path in the form <code>[<var>path</var>][?<var>query</var>][#<var>fragment</var>]</code>,
		or <code>null</code> if none of those components are present in the given URI.
	*/
	public static String getRawPathQueryFragment(final URI uri)
	{
		final StringBuilder stringBuilder=new StringBuilder();
		final String rawPath=uri.getRawPath(); //get the path
		if(rawPath!=null) //if there is a path
		{
			stringBuilder.append(rawPath); //path
		}
		final String rawQuery=uri.getRawQuery(); //get the query
		if(rawQuery!=null) //if there is a query
		{
			stringBuilder.append(URIs.QUERY_SEPARATOR).append(rawQuery); //?query
		}
		final String rawFragment=uri.getRawFragment(); //get the fragment
		if(rawFragment!=null) //if there is a fragment
		{
			stringBuilder.append(URIs.FRAGMENT_SEPARATOR).append(rawFragment); //#query			
		}
		//if any of the components were present (which is distinct from them having string content), return the constructed string; otherwise, return null
		return rawPath!=null||rawFragment!=null||rawFragment!=null ? stringBuilder
				.toString() : null;
	}

	/**Returns a relative path to the URL from the given context URL.
		This version requires the file to be on the same branch of the context
		path (e.g. "http://abc.de/a/c/d.html" is not on the same branch of
		"http://abc.de/a/b").
	G***del The context URL must be a URL of a directory, ending with the directory divider character '/'
	@param contextURL The reference URL to use in making the relative
		path.
	@param url The URL for which a relative path should be returned, in relation
		to the context URL.
	@return A relative path to the URL in relation to the context URL.
	@throws MalformedURLException Thrown if a relative URL cannot be
		determined from the context URL.
	*/
	/*G***fix
		public static String getRelativePath(final URL contextURL, final URL url) throws MalformedURLException
		{

			  //G***check this new implementation; this simply chops off everything that matches

			if(urlPath.startsWith(directoryURLPath)) //if the directory URL path is at the beginning of the URL path
			{
				final String relativePath=urlPath.substring(directoryURLPath.length());  //get everything after the directory URL
				return relativePath;  //return the relative path
			}
			throw new MalformedURLException("Cannot create relative path for "+url+" from context "+contextURL);  //show that we couldn't determine a relative path
		}
	*/

	/**Opens a connection to the given URL, recognizing redirects.
		This method was inspired by the source code to
		<code>javax.swing.JTextPane.getStream()</code>.
	@param url The URL a connection to which should be opened.
	@return A connection to the given URL or the URL to which it redirects.
	*/
	/*G***fix; we need to leave the old version in XMLTextPane because it changes the URL appropriately instead of just automatically redirecting
		public static URLConnection openConnection(final URL url)//G***fix throws IOException
		{
			final URLConnection urlConnection=url.openConnection(); //open a connection to the URL
			if(urlConnection instanceof HttpURLConnection)  //if this is a HTTP connection
			{
			  final HttpURLConnection httpURLConnectoin=(HttpURLConnection)urlConnection; //cast the
		    hconn.setInstanceFollowRedirects(false);
		    int response = hconn.getResponseCode();
		    boolean redirect = (response >= 300 && response <= 399);

	//G***del In the case of a redirect, we want to actually change the URL
	//G***del that was input to the new, redirected URL

		    if (redirect) {
			String loc = conn.getHeaderField("L4ocation");
			if (loc.startsWith("http", 0)) {
			    page = new URL(loc);
			} else {
			    page = new URL(page, loc);
			}
			return getStream(page);
		    }
		}
		String contentType = conn.getContentType();
		if(contentType!=null) //if we receive at least a guess of the content type
		{
	*/

	/**Loads the contents of a URL into an array of bytes.
	@param url The URL from which to read.
	@return An array of bytes from the URL.
	@exception IOException Thrown if there is an error loading the bytes.
	@see InputStreamUtilities#getBytes
	*/
	/*G***fix
		public static byte[] readBytes(final URL url) throws IOException
		{
			final InputStream urlInputStream=url.openConnection().getInputStream();  //create an input stream to the URL
			try
			{
				return InputStreamUtilities.getBytes(urlInputStream);  //convert the URL to an array of bytes
			}
			finally
			{
				urlInputStream.close();  //always close the URL input stream
			}
		}
	*/

	/**Loads the contents of a URL into a string.
	@param url The URL from which to read.
	@param encoding The encoding (such as UTF-8) used to store the string.
	@return A string containing the contents of the URL.
	@exception IOException Thrown if there is an error loading the bytes.
	*/
	/*G***fix
		public static String readString(final URL url, final String encoding) throws IOException
		{
			final byte[] bytes=readBytes(url); //load the contents of the URL
			return new String(bytes, encoding); //convert the bytes into a string, using the given encoding
		}
	*/

	/**Determines the relative path of the given absolute path by removing the root path '/' character from the beginning of the path.
	@param absolutePath The absolute path to convert to a relative path.
	@return A relative path from the root of the absolute path.
	@exception IllegalArgumentException if the given path is not absolute.
	*/
	public static String getRelativePath(final String absolutePath)
	{
		if(!isAbsolutePath(absolutePath)) //if the path is not really absolute
		{
			throw new IllegalArgumentException("Path is not absolute: "+absolutePath);
		}
		return absolutePath.substring(URIs.ROOT_PATH.length()); //remove the beginning root path indicator
	}

	/**Determines whether the URI represents a canonical collection.
	This method returns <code>false</code> for URIs with no path component.
	@param uri The URI the raw path of which to examine.
	@return <code>true</code> if the given URI has a path that ends with a slash ('/').
	@see #isCollectionPath(String)
	*/
	public static boolean isCollectionURI(final URI uri)
	{
		final String rawPath=uri.getRawPath(); //get the raw path (use the raw path in case the last character is an encoded slash)
		return rawPath!=null ? isCollectionPath(rawPath) : false; //see if the path ends with '/'		
	}

	/**Determines whether the given path is a canonical collection path.
	@param rawPath The raw path to examine.
	@return <code>true</code> if the path ends with a slash ('/').
	@exception NullPointerException if the given path is <code>null</code>.
	*/
	public static boolean isCollectionPath(final String rawPath)
	{
		return endsWith(rawPath, URIs.PATH_SEPARATOR); //see if the path ends with '/'		
	}

	/**Determines whether the path of the URI (which may or may not be absolute) is absolute.
	@param uri The URI the path of which to examine.
	@return <code>true</code> if the path of the given URI begins with a slash ('/').
	@see #isAbsolutePath(String)
	*/
	public static boolean isAbsolutePath(final URI uri)
	{
		return isAbsolutePath(uri.getRawPath()); //see if the path begins with '/' (use the raw path in case the first character is an encoded slash)		
	}

	/**Determines whether the given path is absolute.
	@param path The path to examine.
	@return <code>true</code> if the path begins with a slash ('/').
	@exception NullPointerException if the path is <code>null</code>.
	*/
	public static boolean isAbsolutePath(final String path)
	{
		return checkInstance(path, "Path cannot be null")
				.startsWith(URIs.ROOT_PATH); //see if the path begins with '/'		
	}

	/**Determines whether the URI contains only a host and optional port.
	@param uri The URI the path of which to examine.
	@return <code>true</code> if the URI contains only a host and optionally a port.
	*/
	public static boolean isHost(final URI uri)
	{
		return uri.getHost()!=null //a host URI contains only a host and nothing else except maybe a port
				&&uri.getScheme()==null&&uri.getUserInfo()==null
				&&uri.getPath()==null
				&&uri.getQuery()==null&&uri.getFragment()==null;
	}

	/**Creates a URL from a URI. If a valid URL cannot be formed, <code>null</code> is returned.
	@param uri The URI to convert to a URL, or <code>null</code> if no URI is available (in which case <code>null</code> will be returned).
	@return The URL form of the URI, or <code>null</code> if the URI cannot be converted to a valid URL. 
	*/
	public static URL toValidURL(final URI uri)
	{
		try
		{
			//TODO we probably want to check for the condition java.lang.IllegalArgumentException: URI is not absolute
			return uri!=null ? uri.toURL() : null; //convert the URI to a URL, if we have a URI	
		}
		catch(MalformedURLException e) //if there was an error converting to a URL
		{
			return null; //show that we couldn't create a valid URL from the given URI
		}
	}

	/**Resolved a relative URI against a base URI with added functionality.
	The emptry string is appended to the given base URI with no fragment.
	This method correctly resolves fragment URIs against opaque base URIs.
	@param baseURI The URI against which the child URI should be resolved.
	@param childURI The URI to resolve against the base URI.
	@return The child URI resolved against the base URI.
	@exception NullPointerException if the base URI and/or the child URI is <code>null</code>.
	@see <a href="http://www.w3.org/TR/rdf-syntax-grammar/#section-baseURIs">RDF/XML Syntax Specification (Revised) 5.3 Resolving URIs</a>
	*/
	public static URI resolve(final URI baseURI, final URI childURI)
	{
		if(baseURI.isOpaque()) //if the base URI is opaque, do special processing
		{
			final String childURIString=childURI.toString(); //get the child URI as a string
			if(startsWith(childURIString, URIs.FRAGMENT_SEPARATOR)) //if the child URI is a fragment
			{
				return URI.create(removeFragment(baseURI).toString()+childURIString); //remove the fragment, if any, from the base URI, and append the fragment
			}
		}
		if(isPathURI(childURI)) //if the given URI is only a path (with no fragment)
		{
			final String rawPath=childURI.getRawPath(); //get the raw path of the URI
			if(rawPath.length()==0) //if this URI is ""
			{
				return removeFragment(baseURI); //return the base URI with no fragment
			}
		}
		return baseURI.resolve(childURI); //resolve the child URI against the base normally
	}

	/**Resolves a URI path against a base URI.
	@param baseURI The base URI against which the path should be resolved.
	@param path The path to resolve against the base URI.
	@return A URI that represents the path resolved against the base URI.
	@exception NullPointerException if the given base URI and/or path is <code>null</code>.
	*/
	public static URI resolve(final URI baseURI, final URIPath path)
	{
		return baseURI.resolve(path.toURI()); //resolve the path as a URI against the base URI
	}

	/**Returns a URI constructed from a given URI and a fragment identifier.
	<p>If the URI is not syntactically correct, an <code>IllegalArgumentException</code>will be thrown.
	<p>This method should normally only be used when the format of the string is known to be a syntactically correct URI.</p>
	<p>If no URI is provided, a URI is created from the fragment itself.</p>
	@param uri The URI to which to add a fragement identifier, or <code>null</code> if a URI chould be created from just the fragment.
	@param fragment The raw, encoded fragment to add to the end of the URI.
	@exception IllegalArgumentException if the a URI cannot be constructed from the given information.
	@see URI#create(String)
	*/
	public static URI resolveFragment(final URI uri, final String fragment)
			throws IllegalArgumentException
	{
		final String fragmentSuffix=new StringBuilder().append(
				URIs.FRAGMENT_SEPARATOR).append(fragment).toString(); //create a suffix that includes the fragment separator and the fragment
		final URI fragmentURI=URI.create(fragmentSuffix); //create a URI from the fragment
		return uri!=null ? resolve(uri, fragmentURI) : fragmentURI; //if a URI was given, resolve the fragment against the URI; otherwise, just return the fragment suffix itself 
	}

	/**Returns a URI with its fragment, if any, removed.
	@param uri The URI from which a fragment should be removed.
	@return The URI with the fragment, if any, removed.
	@exception NullPointerException if the given URI is <code>null</code>.
	@see #replaceRawFragment(URI, String)
	*/
	public static URI removeFragment(final URI uri)
	{
		return replaceRawFragment(uri, null); //replace the raw fragment, if any, with nothing
	}

	/**Returns a URI with its fragment, if any, replaced.
	@param uri The URI from which a fragment should be removed.
	@param newRawFragment The new encoded fragment, or <code>null</code> if the URI should have no fragment.
	@return The URI with the fragment, if any, removed and replaced with the given raw fragment, if any.
	@exception NullPointerException if the given URI is <code>null</code>.
	*/
	public static URI replaceRawFragment(final URI uri,
			final String newRawFragment)
	{
		final String oldRawFragment=uri.getRawFragment(); //get the raw fragment, if any
		if(oldRawFragment!=null) //if there is currently a fragment
		{
			final int oldRawFragmentLength=oldRawFragment.length(); //get theh length of the current raw fragment
			final StringBuilder uriStringBuilder=new StringBuilder(uri.toString()); //get the string representation of the URI
			final int uriLength=uriStringBuilder.length(); //get the length of the URI
			assert uriStringBuilder.toString().endsWith(
					new StringBuilder().append(URIs.FRAGMENT_SEPARATOR).append(
							oldRawFragment).toString());
			if(newRawFragment!=null) //if a new raw fragment was given
			{
				uriStringBuilder.replace(uriLength-oldRawFragmentLength, uriLength,
						newRawFragment); //replace the old fragment with the new one
			}
			else
			//if no new raw fragment was given
			{
				uriStringBuilder.delete(uriLength-oldRawFragmentLength-1, uriLength); //delete the entire fragment
			}
			return URI.create(uriStringBuilder.toString()); //create a URI from the new URI string
		}
		else
		//if there is no fragment
		{
			if(newRawFragment!=null) //if a new raw fragment was given
			{
				return URI
						.create(uri.toString()+URIs.FRAGMENT_SEPARATOR+newRawFragment); //append the new raw fragment
			}
			else
			//if no new raw fragment was given
			{
				return checkInstance(uri, "URI cannot be null."); //return the original URI
			}
		}
	}

	/**Returns a URI constructed from the given parts, any of which can be <code>null</code>.
	<p>This method should normally only be used when the format of the string is known to be a syntactically correct URI.</p>
	@param scheme The name of the URI scheme.
	@param rawSchemeSpecificPart The raw, encoded scheme-specific part, or <code>null</code> if there is no scheme-specific part.
	@exception IllegalArgumentException if the a URI cannot be constructed from the given strings.
	*/
	public static URI createURI(final String scheme,
			final String rawSchemeSpecificPart) throws IllegalArgumentException
	{
		return createURI(scheme, rawSchemeSpecificPart, null); //create a URI with no fragment
	}

	/**Returns a URI constructed from the given parts, any of which can be <code>null</code>.
	<p>This method should normally only be used when the format of the string is known to be a syntactically correct URI.</p>
	@param scheme The name of the URI scheme.
	@param rawSchemeSpecificPart The raw, encoded scheme-specific part, or <code>null</code> if there is no scheme-specific part.
	@param rawFragment The raw, encoded fragment at the end of the URI, or <code>null</code> if there is no fragment.
	@exception IllegalArgumentException if the a URI cannot be constructed from the given strings.
	*/
	public static URI createURI(final String scheme,
			final String rawSchemeSpecificPart, final String rawFragment)
			throws IllegalArgumentException
	{
		final StringBuilder stringBuilder=new StringBuilder(); //we'll use this to construct the URI
		if(scheme!=null) //if there is a scheme
		{
			stringBuilder.append(scheme).append(URIs.SCHEME_SEPARATOR); //append the scheme and its separator
		}
		if(rawSchemeSpecificPart!=null) //if there is a scheme-specific part
		{
			stringBuilder.append(rawSchemeSpecificPart); //append the scheme-specific part
		}
		if(rawFragment!=null) //if there is a fragment
		{
			stringBuilder.append(URIs.FRAGMENT_SEPARATOR).append(rawFragment); //append the fragment
		}
		return URI.create(stringBuilder.toString()); //create and return a new URI
	}

	/**Returns a URI constructed from the given parts, any of which can be <code>null</code>.
	<p>This method should normally only be used when the format of the string is known to be a syntactically correct URI.</p>
	@param scheme The name of the URI scheme.
	@param rawUserInfo The raw, encoded user information, or <code>null</code> if there is no user information.
	@param host The host information, or <code>null</code> if there is no host.
	@param port The port number, or -1 for no defined port.
	@param rawPath The raw, encoded path, or <code>null</code> if there is no path.
	@param rawQuery The raw, encoded URI query, or <code>null</code> if there is no query.
	@param rawFragment The raw, encoded fragment at the end of the URI, or <code>null</code> if there is no fragment.
	@exception IllegalArgumentException if the a URI cannot be constructed from the given strings.
	*/
	public static URI createURI(final String scheme, final String rawUserInfo,
			final String host, final int port, final String rawPath,
			final String rawQuery, final String rawFragment)
			throws IllegalArgumentException
	{
		final StringBuilder stringBuilder=new StringBuilder(); //we'll use this to construct the URI
		if(scheme!=null) //if there is a scheme
		{
			stringBuilder.append(scheme).append(URIs.SCHEME_SEPARATOR); //append the scheme and its separator
		}
		if(host!=null) //if there is authority information
		{
			stringBuilder.append(URIs.AUTHORITY_PREFIX); //append the authority prefix
			if(rawUserInfo!=null) //if there is user information
			{
				stringBuilder.append(rawUserInfo).append(URIs.USER_INFO_SEPARATOR); //append the user information
			}
			stringBuilder.append(host); //append the host
			if(port>=0) //if there is a port
			{
				stringBuilder.append(URIs.PORT_SEPARATOR).append(port); //append the port
			}
		}
		else
		//if there is no host
		{
			if(rawUserInfo!=null) //if user information was given
			{
				throw new IllegalArgumentException(
						"URI cannot have user info without a host.");
			}
			if(port>=0) //if a port was given
			{
				throw new IllegalArgumentException(
						"URI cannot have a port without a host.");
			}
		}
		if(rawPath!=null) //if there is a path
		{
			stringBuilder.append(rawPath); //append the path
		}
		if(rawQuery!=null) //if there is a query
		{
			stringBuilder.append(URIs.QUERY_SEPARATOR).append(rawQuery); //append the query
		}
		if(rawFragment!=null) //if there is a fragment
		{
			stringBuilder.append(URIs.FRAGMENT_SEPARATOR).append(rawFragment); //append the fragment
		}
		return URI.create(stringBuilder.toString()); //create and return a new URI
	}

	/**Encodes the URI reserved characters in the string, using '%' as an escape character, according to the URI encoding rules in <a href="http://www.ietf.org/rfc/rfc2396.txt">RFC 2396</a>, "Uniform Resource Identifiers (URI): Generic Syntax".
	All characters not considered {@link URIs#NORMAL_CHARS} are encoded.
	The escape character {@link URIs#ESCAPE_CHAR} will always be encoded.
	@param string The data to URI-encode.
	@return A string containing the escaped data.
	@see URIs#ESCAPE_CHAR
	@see URIs#NORMAL_CHARS
	@deprecated
	*/
	public static String encode(final String string)
	{
		return encode(string, null, null); //encode the string with no extra valid or invalid characters
	}

	/**Encodes the URI reserved characters in the string, using '%' as an escape character, according to the URI encoding rules in <a href="http://www.ietf.org/rfc/rfc2396.txt">RFC 2396</a>, "Uniform Resource Identifiers (URI): Generic Syntax".
	All characters not considered {@link URIs#NORMAL_CHARS} are encoded.
	The escape character {@link URIs#ESCAPE_CHAR} will always be encoded.
	@param string The data to URI-encode.
	@param extraValidCharacters Characters to be categorically not , or <code>null</code> if no extra valid characters are given.
	@param extraInvalidCharacters Characters to be categorically encoded, or <code>null</code> if no extra invalid characters are given.
	@return A string containing the escaped data.
	@see URIs#ESCAPE_CHAR
	@see URIs#NORMAL_CHARS
	@deprecated
	*/
	public static String encode(final String string,
			final String extraValidCharacters, final String extraInvalidCharacters)
	{
		final String validCharacters=extraValidCharacters!=null ? URIs.NORMAL_CHARS
				+extraValidCharacters : URIs.NORMAL_CHARS; //if extra valid characters were given, add them to our string
		final String invalidCharacters=extraInvalidCharacters!=null ? extraInvalidCharacters
				+URIs.ESCAPE_CHAR
				: String.valueOf(URIs.ESCAPE_CHAR); //if extra invalid characters were given, make note of them, but always consider the escape character invalid
		final StringBuilder stringBuilder=new StringBuilder(string); //put the string in a string builder so that we can work with it; although inserting encoded sequences may seem inefficient, it should be noted that filling a string buffer with the entire string is more efficient than doing it one character at a time, that characters needed encoding are generally uncommon, and that any copying of the string characters during insertion is done via a native method, which should happen very quickly
		for(int characterIndex=stringBuilder.length()-1; characterIndex>=0; --characterIndex) //work backwords; this keeps us from having a separate variable for the length, but it also makes it simpler to calculate the next position when we swap out characters
		{
			final char c=stringBuilder.charAt(characterIndex); //get the current character
			final boolean encode=c==URIs.ESCAPE_CHAR
					||(validCharacters!=null&&validCharacters.indexOf(c)<0) //encode if there is a list of valid characters and this character is not one of them
					||(invalidCharacters!=null&&invalidCharacters.indexOf(c)>=0); //encode if there is a list of invalid characters and this character is one of them
			if(encode) //if we should encode this character
			{
				try
				{
					final byte[] bytes=String.valueOf(c).getBytes(UTF_8); //convert this character to a sequence of UTF-8 bytes
					final int byteCount=bytes.length; //find out how many bytes there are
					final StringBuilder encodeStringBuilder=new StringBuilder(byteCount*3); //create a string builder to hold three characters for each byte we have (the escape character plus a two-digit encoded value)
					for(int byteIndex=0; byteIndex<byteCount; ++byteIndex) //look at each byte
					{
						encodeStringBuilder.append(URIs.ESCAPE_CHAR); //&
						encodeStringBuilder.append(Integers
								.toHexString(bytes[byteIndex], 2).toUpperCase()); //HH
					}
					stringBuilder.replace(characterIndex, characterIndex+1,
							encodeStringBuilder.toString()); //replace the character with its encoding
				}
				catch(final UnsupportedEncodingException unsupportedEncodingException) //the JVM should always know how to convert a string to UTF-8
				{
					throw new AssertionError(unsupportedEncodingException);
				}
			}
		}
		return stringBuilder.toString(); //return the encoded version of the string
	}

	/**Encodes all URI reserved characters in the URI according to the URI encoding rules in <a href="http://www.ietf.org/rfc/rfc3986.txt">RFC 3986</a>, "Uniform Resource Identifiers (URI): Generic Syntax"
	using the URI escape character, {@value URIs#ESCAPE_CHAR}.
	@param uri The URI to URI-encode.
	@return A string containing the escaped data.
	@see URIs#ESCAPE_CHAR
	*/
	public static String encodeURI(final URI uri)
	{
		return encodeURI(uri.toString()); //encode the string version of the URI
	}

	/**Encodes all URI reserved characters in the URI according to the URI encoding rules in <a href="http://www.ietf.org/rfc/rfc3986.txt">RFC 3986</a>, "Uniform Resource Identifiers (URI): Generic Syntax".
	@param uri The URI to URI-encode.
	@parm escapeChar The escape character to use, which will always be escaped.
	@return A string containing the escaped data.
	*/
	public static String encodeURI(final URI uri, final char escapeChar)
	{
		return encodeURI(uri.toString(), escapeChar); //encode all string version of the URI
	}

	/**Encodes all URI reserved characters in the string according to the URI encoding rules in <a href="http://www.ietf.org/rfc/rfc3986.txt">RFC 3986</a>, "Uniform Resource Identifiers (URI): Generic Syntax"
	using the URI escape character, {@value URIs#ESCAPE_CHAR}.
	@param string The data to URI-encode.
	@return A string containing the escaped data.
	@see URIs#ESCAPE_CHAR
	*/
	public static String encodeURI(final String string)
	{
		return encodeURI(string, URIs.ESCAPE_CHAR); //encode the URI using the standard escape character
	}

	/**Encodes all URI reserved characters in the string according to the URI encoding rules in <a href="http://www.ietf.org/rfc/rfc3986.txt">RFC 3986</a>, "Uniform Resource Identifiers (URI): Generic Syntax".
	@param string The data to URI-encode.
	@parm escapeChar The escape character to use, which will always be escaped.
	@return A string containing the escaped data.
	*/
	public static String encodeURI(final String string, final char escapeChar)
	{
		return uriEncode(string, URIs.UNRESERVED_CHARS, escapeChar); //encode all non-unreserved characters
	}

	/**Encodes the URI reserved characters in the string according to the URI encoding rules in <a href="http://www.ietf.org/rfc/rfc3986.txt">RFC 3986</a>, "Uniform Resource Identifiers (URI): Generic Syntax"
	using the URI escape character, {@value URIs#ESCAPE_CHAR}.
	@param string The data to URI-encode.
	@param validCharacters Characters that should not be encoded; all other characters will be encoded.
	@return A string containing the escaped data.
	@see URIs#ESCAPE_CHAR
	*/
	static String uriEncode(final String string, final String validCharacters)
	{
		return uriEncode(string, validCharacters, URIs.ESCAPE_CHAR); //encode the string with the normal escape character
	}

	/**Encodes the URI reserved characters in the string according to the URI encoding rules in <a href="http://www.ietf.org/rfc/rfc3986.txt">RFC 3986</a>, "Uniform Resource Identifiers (URI): Generic Syntax".
	@param string The data to URI-encode.
	@param validCharacters Characters that should not be encoded; all other characters will be encoded.
	@parm escapeChar The escape character to use, which will always be escaped.
	@return A string containing the escaped data.
	*/
	static String uriEncode(final String string, final String validCharacters, final char escapeChar)
	{
		return escapeHex(string, validCharacters, null, escapeChar, 2);	//escape the string using two escape hex digits
	}

	/**Decodes the escaped characters in the character iterator according to the URI encoding rules in
		<a href="http://www.ietf.org/rfc/rfc2396.txt">RFC 2396</a>, "Uniform Resource Identifiers (URI): Generic Syntax",
		using the URI escape character, {@value URIs#ESCAPE_CHAR}.
	@param uri The data to URI-decode.
	@return A string containing the encoded URI data.
	@exception IllegalArgumentException if the given URI string contains a character greater than U+00FF.
	@exception IllegalArgumentException if a given escape character is not followed by a two-digit escape sequence.
	@see URIs#ESCAPE_CHAR
	*/
	public static String uriDecode(final String uri)
	{
		return uriDecode(uri, URIs.ESCAPE_CHAR); //decode the string using the standard URI escape character
	}

	/**Decodes the escaped ('%') characters in the character iterator
		according to the URI encoding rules in
		<a href="http://www.ietf.org/rfc/rfc2396.txt">RFC 2396</a>,
		"Uniform Resource Identifiers (URI): Generic Syntax".
	@param uri The data to URI-decode.
	@parm escapeChar The escape character.
	@return A string containing the encoded URI data.
	@exception IllegalArgumentException if the given URI string contains a character greater than U+00FF.
	@exception IllegalArgumentException if a given escape character is not followed by a two-digit escape sequence.
	*/
	public static String uriDecode(final String uri, final char escapeChar)
	{
		return unescapeHex(uri, escapeChar, 2);	//unescape the string using two escape hex digits
	}

	//variables for fixing a JDK URI.resolve() bug
	private final static String EXPECTED_URI_PREFIX="file:////";

	private final static String RESULT_URI_PREFIX="file:/";

	/**Changes a URI from one base to another.
	For example, <code>http://example.com/base1/test.txt</code> changed to base
		<code>http://example.com/base2/level2/</code> yields
		<code>http://example.com/base2/level2/test.txt</code>.
	<p>If the old and new base URIs are the same, the given URI is returned.</p>
	<p>This method contains a workaround for the JDK 5.0 bug that chops off the
		first few forward slashes for Windows network names.</p>
	@param uri The URI the base of which to change.
	@param oldBaseURI The current base URI.
	@param newBaseURI The base URI of the new URI to construct.
	@return A new URI constructed by relativizing the URI to the old base URI and
		resolving the resulting URI agains the new base URI.
	@see URI#relativize(URI)
	@see URI#resolve(URI)
	@exception IllegalArgumentException Thrown if <var>oldBaseURI</code> is not
		a base URI of <var>uri</var>.
	*/
	public static URI changeBase(final URI uri, final URI oldBaseURI,
			final URI newBaseURI)
	{
		if(oldBaseURI.equals(newBaseURI)) //if the old and new base URIs are the same
		{
			return uri; //the URI will not change
		}
		//G***del Debug.trace("changing base of ", uri, "from", oldBaseURI, "to", newBaseURI);
		final URI relativeURI=oldBaseURI.relativize(uri); //get a URI relative to the old base URI
		if(relativeURI.isAbsolute()) //if we couldn't relativize the the URI to the old base URI and come up with a relative URI
		{
			throw new IllegalArgumentException(oldBaseURI.toString()
					+" is not a base URI of "+uri);
		}
		URI newURI=newBaseURI.resolve(relativeURI); //resolve the relative URI to the new base URI
		//G***del Debug.trace("new URI:", newURI);
		final String newBaseURIString=newBaseURI.toString(); //get the string of the new base URI
		//G***del; maybe not needed		final StringBuilder newURIStringBuilder=new StringBuilder(newURI.toString());	//get the string of the new URI
		final String newURIString=newURI.toString(); //get the string version of the new URI
		/*G***del
					//if the old URI ended with '/' but the new URI doesn't (this can occur when the new URI references a directory
				if(endsWith(uri.toString(), PATH_SEPARATOR) && !endsWith(newURIStringBuilder, PATH_SEPARATOR))
				{
					
				}
		*/

		//check for the JDK 5.0 bug that chops off the first few forward slashes for Windows network names
		if(!newURIString.startsWith(newBaseURIString)) //if the new URI doesn't start with the new base URI we were expecting
		{
			if(newBaseURIString.startsWith(EXPECTED_URI_PREFIX)
					&&newURIString.startsWith(RESULT_URI_PREFIX))
			{
				final String fixedURIString=EXPECTED_URI_PREFIX
						+newURIString.substring(RESULT_URI_PREFIX.length()); //replace the incorrect beginning section
				newURI=URI.create(fixedURIString); //return create a URI that goes back to the new base URI we expected
			}
			else
			//if this is a different bug than we expected
			{
				throw new AssertionError(newURIString
						+" does not begin with expected new base URI "+newBaseURIString);
			}
		}
		return newURI; //return the new URI with the changed base
	}

	/**Determines whether the given URI is a child relative to the given base URI.
	The base URI is considered a child of itself.
	This for the base URI <code>http://www.example.com/base/</code>, the URIs <code>http://www.example.com/base/</code> and <code>http://www.example.com/base/child/</code> are considered
	child URIs, while <code>http://www.example.com/</code> and <code>http://www.example.com/other/</code> are not.
	@param baseURI The assumed base URI.
	@param uri The URI which may be relative to the given base URI.
	@return <code>true</code> if the given URI can be made relative to the given base URI resulting in a non-absolute form.
	@exception NullPointerException if the given base URI and/or URI is <code>null</code>.
	*/
	public static boolean isChild(final URI baseURI, final URI uri)
	{
		final URI relativeURI=baseURI.relativize(uri); //get a URI relative to the base URI
		return !relativeURI.isAbsolute(); //if the relativized URI is not absolute, the URI is relative to the base
	}

	/**Characters that can appear in a URI path with no escape sequences.*/
	//G***del	protected final static String COMPRESS_CHARS=ALPHA_CHARS+DIGIT_CHARS;	//length 49
	/**Compresses a URI into a shorter string representation.
	The resulting string consists only of URI <code>xalpha</code> characters with no escape sequences.
	@param uri The URI to compress.
	@return A compressed string representation of the URI.
	*/
	/*G***fix
		public static String compress(final URI uri)
		{
			final int INPUT_CHAR_WIDTH=6;	//no URI character can be more than six bits wide 
			final int COMPRESS_BASE=NORMAL_CHARS.length();	//this is the numbering base we'll use for compression 
			final String uriString=uri.toString();
			final StringBuilder stringBuilder=new StringBuilder();
			int accumulator=0;	//we'll accumulate bits here
			int bit=0;	//this is how many bits we have to shift an incoming character
			for(int i=uriString.length()-1; i>=0; --i)
			{
				final char character=uriString.charAt(i);	//get the next character to compress
				accumulator|=character<<bit;	//combine our value with the character's value
				bit+=INPUT_CHAR_WIDTH;	//we'll have to shift the next character up above the bits we just added
				while(accumulator>COMPRESS_BASE)	//while the value is more than our conversion base
				{
					final index
					
				}
			}
		}
	*/

	/**URI alphabetic and digit characters.*/
	private final static String COMPRESS_NORMAL_CHARS=URIs.ALPHA_CHARS
			+URIs.DIGIT_CHARS; //"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" length 62

	/**Characters that will be compressed.*/
	private final static String OTHER_CHARS=URIs.SAFE_CHARS+URIs.EXTRA_CHARS
			+URIs.ESCAPE_CHAR+URIs.RESERVED_CHARS; //"$-_@.&!*\"'(),%=;/#?: " length 21

	/**Characters that can appear in a URI path with no escape sequences.*/
	private final static String COMPRESS_ENCODE_CHARS="-_()@"; //length 5

	/**Compresses a URI into a shorter string representation.
	@param uri The URI to compress.
	@return A compressed string representation of the URI.
	*/
	public static String safeEncode(final URI uri)
	{
		final int ENCODE_BASE=COMPRESS_ENCODE_CHARS.length(); //this is the base into which we'll encode certain characters
		final String uriString=uri.toString();
		final StringBuilder stringBuilder=new StringBuilder();
		for(int i=0; i<uriString.length(); ++i) //look at each URI character
		{
			final char character=uriString.charAt(i); //get the next character
			if(COMPRESS_NORMAL_CHARS.indexOf(character)>=0) //if this is a normal character
			{
				stringBuilder.append(character); //add the character normally
			}
			else
			//if this is a character to be encoded
			{
				final int index=OTHER_CHARS.indexOf(character); //get the character's index within our set of special characters
				assert index>=0 : "Invalid URI character: "+character; //if the character came in the URI object, it should always be valid
				final int high=index/ENCODE_BASE; //get the high bits of our encoding
				final int low=index%ENCODE_BASE; //get the high bits of our encoding
				stringBuilder.append(COMPRESS_ENCODE_CHARS.charAt(high)); //add a character to represent our high bits
				stringBuilder.append(COMPRESS_ENCODE_CHARS.charAt(low)); //add a character to represent our low bits
			}
		}
		return stringBuilder.toString(); //return our encoded URI string
	}

	/**Compresses a URI into a shorter string representation.
	@param string The alphanumeric string.
	@return An uncompressed URI from the alphanumeric string.
	@exception SyntaxException Thrown if the given string is not correctly encoded. 
	*/
	public static URI safeDecode(final String string) throws SyntaxException
	{
		final int ENCODE_BASE=COMPRESS_ENCODE_CHARS.length(); //this is the base into which we'll encode certain characters TODO maybe place this outside the method
		final StringBuilder stringBuilder=new StringBuilder();
		for(int i=0; i<string.length(); ++i) //look at each character
		{
			final char character=string.charAt(i); //get the next character
			if(COMPRESS_NORMAL_CHARS.indexOf(character)>=0) //if this is a normal character
			{
				stringBuilder.append(character); //add the character normally
			}
			else
			//if this is a character to be encoded
			{
				final int high=COMPRESS_ENCODE_CHARS.indexOf(character); //get the high bits
				if(high<0) //if the high character wasn't recognized
				{
					throw new SyntaxException("Invalid character.", string); //indicate that an unexpected character was encountered					
				}
				if(i==string.length()-1) //if there are no more characters
				{
					throw new SyntaxException("Incomplete encoding sequence.", string); //indicate that the encoding character was not present.
				}
				final int low=COMPRESS_ENCODE_CHARS.indexOf(string.charAt(++i)); //go to the next character and get its index
				if(low<0) //if the low character wasn't recognized
				{
					throw new SyntaxException("Invalid character.", string); //indicate that an unexpected character was encountered					
				}
				final int index=high*ENCODE_BASE+low; //get the index of the original character
				if(index>=OTHER_CHARS.length()) //if the resulting sequence does not match one of our original characters
				{
					throw new SyntaxException("Invalid encoding sequence.", string); //indicate that the encoding resulted in an invalid sequence					
				}
				stringBuilder.append(OTHER_CHARS.charAt(index)); //add the encoded character to our string builder
			}
		}
		return URI.create(stringBuilder.toString()); //return the original URI
	}

}
