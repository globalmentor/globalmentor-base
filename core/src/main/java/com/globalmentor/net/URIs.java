/*
 * Copyright © 1996-2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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
import java.util.regex.Pattern;

import javax.annotation.*;

import static java.util.Objects.*;

import com.globalmentor.collections.*;
import com.globalmentor.io.*;

import com.globalmentor.java.Characters;
import com.globalmentor.model.NameValuePair;
import static com.globalmentor.java.CharSequences.*;
import static com.globalmentor.java.Conditions.*;
import static com.globalmentor.java.StringBuilders.*;
import static com.globalmentor.text.TextFormatter.*;

import com.globalmentor.text.*;

/**
 * Various URI manipulating functions for working with URIs as defined in <a href="https://tools.ietf.org/html/rfc3986">RFC 3986</a>, "Uniform Resource
 * Identifiers (URI): Generic Syntax".
 * <p>
 * For file URIs Java incorrectly uses the form <code>file:/mnt/sdcard/...</code> instead of <code>file:///mnt/sdcard/...</code>, but these utilities use the
 * former for consistency.
 * </p>
 * @see URI
 * @see <a href="https://tools.ietf.org/html/rfc3986">RFC 3986: Uniform Resource Identifiers (URI): Generic Syntax</a>
 */
public class URIs {

	/** The shared static empty array of URIs. */
	public static final URI[] NO_URIS = new URI[0];

	/**
	 * A shared URI constant equal to creating a URI from an empty path.
	 * @apiNote This constant is useful for detecting a URI relativized against itself, for example.
	 */
	public static final URI EMPTY_PATH_URI = URI.create("");

	/** The file scheme identifier. */
	public static final String FILE_SCHEME = "file";

	/** The FTP scheme identifier. */
	public static final String FTP_SCHEME = "ftp";

	/** The email address scheme identifier. */
	public static final String MAILTO_SCHEME = "mailto";

	/** The info scheme identifier. */
	public static final String INFO_SCHEME = "info";

	/** The delimiter separating the info scheme namespace from the rest of the info scheme-specific part. */
	public static final char INFO_SCHEME_NAMESPACE_DELIMITER = '/';

	/** The path scheme identifier for representing relative and absolute URI paths. */
	public static final String PATH_SCHEME = "path";

	/** The resource scheme identifier "resource". */
	public static final String RESOURCE_SCHEME = "resource";

	/** The telephone scheme identifier. */
	public static final String TEL_SCHEME = "tel";

	/** The URN scheme identifier "urn". */
	public static final String URN_SCHEME = "urn";

	/** The colon character (':') that separates a URI schema from the rest of the URI. */
	public static final char SCHEME_SEPARATOR = ':';

	/**
	 * The pattern to match the scheme-specific part of a URN.
	 * <p>
	 * This pattern is not currently meant to be a vigorous validation of URN format, but rather a means to easily discover the components of a URN.
	 * </p>
	 */
	public static final Pattern URN_SSP_PATTERN = Pattern.compile(String.format("(.+)%s(.+)", SCHEME_SEPARATOR));
	/** The matching group to retrieve the URN namespace identifier. */
	public static final int URN_SSP_PATTERN_NID_MATCHING_GROUP = 1;
	/** The matching group to retrieve the URN-namespace-specific part. */
	public static final int URN_SSP_PATTERN_NSS_MATCHING_GROUP = 2;

	/** The prefix string that introduces an authority. */
	public static final String AUTHORITY_PREFIX = "//";

	/** The at sign (<code>'@'</code>) that separates user information from a host in a URI. */
	public static final char USER_INFO_SEPARATOR = '@';

	/** The colon character (<code>':'</code>) that separates a host from a port. */
	public static final char PORT_SEPARATOR = ':';

	/** The slash character (<code>'/'</code>) that separates components in a URI path. */
	public static final char PATH_SEPARATOR = '/';

	/** The path segment {@value #CURRENT_LEVEL_PATH_SEGMENT} representing the current hierarchical level of a hierarchical URI. */
	public static final String CURRENT_LEVEL_PATH_SEGMENT = ".";

	/** The collection path {@value #CURRENT_LEVEL_PATH} representing the current hierarchical level of a hierarchical URI. */
	public static final String CURRENT_LEVEL_PATH = CURRENT_LEVEL_PATH_SEGMENT + PATH_SEPARATOR;

	/** The URI collection path of {@value #CURRENT_LEVEL_PATH} representing the current hierarchical level of a hierarchical URI. */
	public static final URI CURRENT_LEVEL_PATH_URI = URI.create(CURRENT_LEVEL_PATH);

	/** The path segment {@value #PARENT_LEVEL_PATH_SEGMENT} representing the parent hierarchical level of a hierarchical URI. */
	public static final String PARENT_LEVEL_PATH_SEGMENT = "..";

	/** The collection path {@value #PARENT_LEVEL_PATH} representing the parent hierarchical level of a hierarchical URI. */
	public static final String PARENT_LEVEL_PATH = PARENT_LEVEL_PATH_SEGMENT + PATH_SEPARATOR;

	/** The URI collection path {@value #PARENT_LEVEL_PATH} representing the parent hierarchical level of a hierarchical URI. */
	public static final URI PARENT_LEVEL_PATH_URI = URI.create(PARENT_LEVEL_PATH);

	/** The character that separates the query from the rest of a URI. */
	public static final char QUERY_SEPARATOR = '?';

	/** The character that separates each name-value pair in a query. */
	public static final char QUERY_NAME_VALUE_PAIR_DELIMITER = '&';

	/** The character that represents assigning a value to a name in a query. */
	public static final char QUERY_NAME_VALUE_ASSIGNMENT = '=';

	/** The pound character ('#') that separates a fragment from the rest of a URI. */
	public static final char FRAGMENT_SEPARATOR = '#';

	/** The path to root, consisting of a single path separator ("/"). */
	public static final String ROOT_PATH = String.valueOf(PATH_SEPARATOR);

	/** A URI consisting only of a single path separator ("/"). */
	public static final URI ROOT_PATH_URI = URI.create(ROOT_PATH);

	/** The path Java returns when it tries to resolve <code>..</code> to the root path. */
	public static final String ROOT_PATH_PARENT_LEVEL = ROOT_PATH + PARENT_LEVEL_PATH_SEGMENT;

	/** The character separating a <code>mailto</code> URI username from the domain. */
	public static final char MAILTO_USERNAME_DOMAIN_SEPARATOR = '@'; //TODO reuse EmailAddress definition

	/** Alphabetic characters as defined by RFC 2396. */
	public static final Characters ALPHA_CHARACTERS = Characters.ofRange('a', 'z').addRange('A', 'Z'); //count 52

	/** Digit characters as defined by RFC 2396. */
	public static final Characters DIGIT_CHARACTERS = Characters.ofRange('0', '9'); //count 10

	/** Safe characters as defined by RFC 2396. */
	public static final Characters SAFE_CHARACTERS = Characters.of('$', '-', '_', '@', '.', '&'); //count 6

	/** Extra characters as defined by RFC 2396. */
	public static final Characters EXTRA_CHARACTERS = Characters.of('!', '*', '"', '\'', '(', ')', ','); //count 7

	/** The character to use for escaping URI data as defined by RFC 2396. */
	public static final char ESCAPE_CHAR = '%'; //count 1

	/** Reserved characters as defined by RFC 2396. */
	public static final Characters RESERVED_CHARACTERS = Characters.of('=', ';', '/', '#', '?', ':', ' '); //count 7

	/** Characters that can appear in a URI as defined by RFC 2396. */
	public static final Characters URI_CHARACTERS = ALPHA_CHARACTERS.add(DIGIT_CHARACTERS).add(SAFE_CHARACTERS).add(EXTRA_CHARACTERS).add(ESCAPE_CHAR)
			.add(RESERVED_CHARACTERS); //count 83

	/** Characters that can appear in a URI path with no escape sequences. */
	public static final Characters NORMAL_CHARACTERS = ALPHA_CHARACTERS.add(DIGIT_CHARACTERS).add(SAFE_CHARACTERS).add(EXTRA_CHARACTERS); //length 76

	/** Unreserved characters defined by RFC 3986. */
	public static final Characters UNRESERVED_CHARACTERS = ALPHA_CHARACTERS.add(DIGIT_CHARACTERS).add('-', '.', '_', '~');

	/** General delimiter characters defined by RFC 3986. */
	public static final Characters GEN_DELIM_CHARACTERS = Characters.of(':', '/', '?', '#', '[', ']', '@');

	/** Subdelimiter characters defined by RFC 3986. */
	public static final Characters SUB_DELIM_CHARACTERS = Characters.of('!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=');

	/** Path segment characters defined by RFC 3986. */
	public static final Characters PATH_SEGMENT_CHARACTERS = UNRESERVED_CHARACTERS.add(SUB_DELIM_CHARACTERS).add(':', '@');

	/** Path characters defined by RFC 3986. */
	public static final Characters PATH_CHARACTERS = PATH_SEGMENT_CHARACTERS.add('/');

	/**
	 * The sequence "//" which is supposed to be present in file URIs (e.g. <code>file:///mnt/sdcard/...</code>) but which isn't present in Java file URIs.
	 * @see <a href="http://blogs.msdn.com/b/ie/archive/2006/12/06/file-uris-in-windows.aspx">File URIs in Windows.</a>
	 */
	public static final String FILE_URI_PATH_ROOT_PREFIX = ROOT_PATH + PATH_SEPARATOR;

	/**
	 * The maximum URL length allowed by Microsoft Internet Explorer for HTTP GET.
	 * @see <a href="http://support.microsoft.com/default.aspx?scid=kb;EN-US;q208427">Maximum URL length is 2,083 characters in Internet Explorer</a>
	 */
	public static final int MICROSOFT_INTERNET_EXPLORER_MAXIMUM_URI_LENGTH = 2083;

	/**
	 * A <code>text/uri-list</code> content type as defined in <a href="https://tools.ietf.org/html/rfc2483#section-5">RFC 2483 § 5. The text/uri-list Internet
	 * Media Type</a>.
	 * @see <a href="https://tools.ietf.org/html/rfc2483">RFC 2483: URI Resolution Services Necessary for URN Resolution</a>
	 */
	public static final MediaType URI_LIST_MEDIA_TYPE = MediaType.of(MediaType.TEXT_PRIMARY_TYPE, "uri-list");

	/**
	 * Creates a string of type <code>text/uri-list</code> as defined in <a href="http://www.ietf.org/rfc/rfc2483.txt">RFC 2483</a>, "URI Resolution Services
	 * Necessary for URN Resolution".
	 * @param uris The URIs to include in the list.
	 * @return A URI list string.
	 * @see <a href="http://www.ietf.org/rfc/rfc2483.txt">RFC 2483: URI Resolution Services Necessary for URN Resolution</a>
	 */
	public static String createURIList(final URI... uris) {
		return formatList("\r\n", (Object[])uris); //create the URI list
	}

	/**
	 * Verifies that the given URI has the indicated scheme.
	 * @param uri The URI to check.
	 * @param scheme The scheme to match for the URI.
	 * @return The given URI.
	 * @throws NullPointerException if the given URI and/or scheme is <code>null</code>.
	 * @throws IllegalArgumentException if the scheme of the given URI does not match the given scheme.
	 */
	public static final URI checkScheme(final URI uri, final String scheme) {
		if(!scheme.equals(uri.getScheme())) { //if the URI's scheme doesn't match the given scheme
			throw new IllegalArgumentException("Scheme of URI " + uri + " must be " + scheme);
		}
		return uri; //return the URI
	}

	/**
	 * Creates a new URI identical to the supplied URI with a different scheme.
	 * @param uri The URI to change.
	 * @param newScheme The new scheme information.
	 * @return A new URI with the new scheme information.
	 * @throws NullPointerException if the given URI and/or new scheme is <code>null</code>.
	 * @throws IllegalArgumentException if the given URI has no scheme or if the given scheme results in an invalid URI.
	 */
	public static URI changeScheme(final URI uri, final String newScheme) {
		return createURI(requireNonNull(newScheme, "Scheme cannot be null."), uri.getRawUserInfo(), uri.getHost(), uri.getPort(), uri.getRawPath(),
				uri.getRawQuery(), uri.getRawFragment()); //construct an identical URI except with a different scheme
	}

	/**
	 * Verifies that the given URI is an {@value URIs#INFO_SCHEME} scheme URI with the given namespace.
	 * @param uri The URI to check.
	 * @param infoNamespace The info namespace to match for the URI.
	 * @return The given URI.
	 * @throws NullPointerException if the given URI and/or info namespace is <code>null</code>.
	 * @throws IllegalArgumentException if the scheme of the given URI is not {@value URIs#INFO_SCHEME} and/or the info namespace does not match the given info
	 *           namespace.
	 */
	public static final URI checkInfoNamespace(final URI uri, final String infoNamespace) {
		if(!checkScheme(uri, INFO_SCHEME).getRawSchemeSpecificPart().startsWith(infoNamespace + INFO_SCHEME_NAMESPACE_DELIMITER)) { //check for the info scheme; if the scheme-specific part is not what was expected
			throw new IllegalArgumentException("Info namespace of URI " + uri + " must be " + infoNamespace);
		}
		return uri; //return the URI
	}

	/**
	 * Determines the info namespace of the given {@value URIs#INFO_SCHEME} scheme URI.
	 * @param uri The URI from which the info namespace should be retrieved.
	 * @return The info namespace of the given info URI.
	 * @throws NullPointerException if the given URI is <code>null</code>.
	 * @throws IllegalArgumentException if the given URI is not a valid {@value URIs#INFO_SCHEME} scheme URI.
	 */
	public static final String getInfoNamespace(final URI uri) {
		final String ssp = checkScheme(uri, INFO_SCHEME).getRawSchemeSpecificPart(); //get the raw scheme-specific part after checking to make sure this is an info URI
		final int namespaceDelimiterIndex = ssp.indexOf(INFO_SCHEME_NAMESPACE_DELIMITER); //get the index of the info URI namespace delimiter
		if(namespaceDelimiterIndex < 1) { //if there is no namespace delimiter, or there are no namespace characters
			throw new IllegalArgumentException("info URI " + uri + " missing delimited namespace.");
		}
		return ssp.substring(0, namespaceDelimiterIndex); //return the namespace
	}

	/**
	 * Determines the info identifier of the given {@value URIs#INFO_SCHEME} scheme URI.
	 * @param uri The URI from which the info identifier should be retrieved.
	 * @return The decoded info identifier of the given info URI.
	 * @throws NullPointerException if the given URI is <code>null</code>.
	 * @throws IllegalArgumentException if the given URI is not a valid {@value URIs#INFO_SCHEME} scheme URI.
	 */
	public static final String getInfoIdentifier(final URI uri) {
		return decode(getInfoRawIdentifier(uri)); //decode the raw identifier of the info URI
	}

	/**
	 * Determines the raw, encoded info identifier of the given {@value URIs#INFO_SCHEME} scheme URI.
	 * @param uri The URI from which the info identifier should be retrieved.
	 * @return The raw, encoded info identifier of the given info URI.
	 * @throws NullPointerException if the given URI is <code>null</code>.
	 * @throws IllegalArgumentException if the given URI is not a valid {@value URIs#INFO_SCHEME} scheme URI.
	 */
	public static final String getInfoRawIdentifier(final URI uri) {
		final String ssp = checkScheme(uri, INFO_SCHEME).getRawSchemeSpecificPart(); //get the raw scheme-specific part after checking to make sure this is an info URI
		final int namespaceDelimiterIndex = ssp.indexOf(INFO_SCHEME_NAMESPACE_DELIMITER); //get the index of the info URI namespace delimiter
		if(namespaceDelimiterIndex < 1) { //if there is no namespace delimiter, or there are no namespace characters
			throw new IllegalArgumentException("info URI " + uri + " missing delimited namespace.");
		}
		return ssp.substring(namespaceDelimiterIndex + 1); //return the identifier (the part after the namespace and delimiter)
	}

	/**
	 * Determines whether the given URI is an {@value URIs#INFO_SCHEME} scheme URI with the given namespace.
	 * @param uri The URI to check.
	 * @param infoNamespace The info namespace to match for the URI.
	 * @return The <code>true</code> if the given URI has a scheme of {@value URIs#INFO_SCHEME} and has the indicated info namespace.
	 * @throws NullPointerException if the given URI and/or info namespace is <code>null</code>.
	 */
	public static final boolean isInfoNamespace(final URI uri, final String infoNamespace) {
		return INFO_SCHEME.equals(uri.getScheme()) && uri.getRawSchemeSpecificPart().startsWith(infoNamespace + INFO_SCHEME_NAMESPACE_DELIMITER); //check for the info scheme and the info namespace
	}

	/**
	 * Determines the raw, encoded path of the given {@value #PATH_SCHEME} scheme URI. The path will never be <code>null</code>; the empty relative path
	 * <code>path:</code> will return the empty string. Any query or fragment is ignored.
	 * @apiNote This method is needed because the {@link URI#getRawPath()} method does not recognize relative paths for the {@value #PATH_SCHEME} scheme.
	 * @param uri The path URI from which the path should be retrieved.
	 * @return The raw, encoded path of the given path URI.
	 * @throws NullPointerException if the given URI is <code>null</code>.
	 * @throws IllegalArgumentException if the given URI is not a valid {@value #PATH_SCHEME} scheme URI.
	 */
	public static final String getPathRawPath(final URI uri) {
		String rawPath = checkScheme(uri, PATH_SCHEME).getRawPath(); //get the raw path of the URI, ensuring that it is a "path:" URI
		if(rawPath == null) { //if Java sees no path, it must be a relative path; extract it manually
			rawPath = uri.getRawSchemeSpecificPart(); //the raw path is the scheme-specific part
			final int queryStart = rawPath.indexOf(QUERY_SEPARATOR); //see if this URI has a query (the scheme-specific part will not include the fragment, if any
			if(queryStart >= 0) { //if a query is present
				rawPath = rawPath.substring(0, queryStart); //remove the query
			}
		}
		return rawPath; //return the raw path
	}

	/**
	 * Returns the path of the given {@value #PATH_SCHEME} scheme URI as a {@link URIPath}.
	 * @param uri The path URI from which the path should be retrieved.
	 * @return A URI path object representing the path of the given path URI.
	 * @throws NullPointerException if the given URI is <code>null</code>.
	 * @throws IllegalArgumentException if the given URI is not a valid {@value #PATH_SCHEME} scheme URI.
	 */
	public static final URIPath getPathURIPath(final URI uri) {
		return URIPath.of(getPathRawPath(uri)); //get the raw path and create a URIPath from that
	}

	/**
	 * Creates a new URI identical to the supplied URI with a different path.
	 * @param uri The URI to change.
	 * @param path The path, or <code>null</code> if there should be no path.
	 * @return A new URI with the new path.
	 * @throws NullPointerException if the given URI is <code>null</code>.
	 * @throws IllegalArgumentException if the given path results in an invalid URI.
	 */
	public static URI changePath(final URI uri, final URIPath path) {
		return changeRawPath(uri, path != null ? path.toString() : null);
	}

	/**
	 * Creates a new URI identical to the supplied URI with a different raw path.
	 * @param uri The URI to change.
	 * @param newRawPath The raw, escaped path, or <code>null</code> if there should be no path.
	 * @return A new URI with the new raw path information.
	 * @throws NullPointerException if the given URI is <code>null</code>.
	 * @throws IllegalArgumentException if the given path results in an invalid URI.
	 */
	public static URI changeRawPath(final URI uri, final String newRawPath) {
		return createURI(uri.getScheme(), uri.getRawUserInfo(), uri.getHost(), uri.getPort(), newRawPath, uri.getRawQuery(), uri.getRawFragment()); //construct an identical URI except with a different raw path
	}

	/**
	 * Creates a new URI identical to the supplied URI with a different host.
	 * @param uri The URI to change.
	 * @param newHost The new host information.
	 * @return A new URI with the new host information.
	 * @throws NullPointerException if the given URI and/or new host is <code>null</code>.
	 * @throws IllegalArgumentException if the given URI has no host or if the given host results in an invalid URI.
	 */
	public static URI changeHost(final URI uri, final String newHost) {
		return createURI(uri.getScheme(), uri.getRawUserInfo(), requireNonNull(newHost, "Host cannot be null."), uri.getPort(), uri.getRawPath(), uri.getRawQuery(),
				uri.getRawFragment()); //construct an identical URI except with a different host
	}

	/**
	 * Creates a new URI identical to the supplied URI with a different raw scheme-specific part.
	 * @param uri The URI to change.
	 * @param newRawSSP The raw, escaped scheme-specific part.
	 * @return A new URI with the new raw scheme-specific part information.
	 * @throws NullPointerException if the given URI or the scheme-specific part is <code>null</code>.
	 * @throws IllegalArgumentException if the given scheme-specific part results in an invalid URI.
	 */
	public static URI changeRawSchemeSpecificPart(final URI uri, final String newRawSSP) {
		requireNonNull(newRawSSP, "a null scheme-specific part is not allowed.");
		final String oldRawSSP = uri.getRawSchemeSpecificPart(); //get the old raw scheme-specific part of the URI
		if(oldRawSSP.equals(newRawSSP)) { //if the scheme-specific part is the same
			return uri; //the URI remains unchanged
		}
		final StringBuilder stringBuilder = new StringBuilder(); //create a new string builder
		stringBuilder.append(uri.getScheme()).append(SCHEME_SEPARATOR).append(newRawSSP); //append the scheme and the scheme-specific part
		final String rawFragment = uri.getRawFragment(); //get the raw fragment, if any
		if(rawFragment != null) { //if there is a raw fragment
			stringBuilder.append(FRAGMENT_SEPARATOR).append(rawFragment); //include the raw fragment
		}
		return URI.create(stringBuilder.toString()); //create a URI from the constructed string
	}

	/**
	 * Forces a URI to represent a collection by appending a trailing path separator to the URI path, if any. If the URI has no path, no change is made.
	 * @apiNote This method is most useful for working with file systems that are imprecise about distinguishing between collection and non-collection nodes.
	 * @param uri The URI to represent a collection.
	 * @return A form of the URI representing a collection.
	 * @throws NullPointerException if the given URI is <code>null</code>.
	 */
	public static URI toCollectionURI(URI uri) {
		final String rawPath = uri.getRawPath(); //get the raw path of the directory URI
		if(rawPath != null) { //if there is a path
			if(!endsWith(rawPath, PATH_SEPARATOR)) { //if the path isn't a collection path
				//create a new URI with the path separator appended
				uri = changeRawPath(uri, rawPath + PATH_SEPARATOR);
			}
		}
		return uri;
	}

	/**
	 * Returns a path object to represent the path of the URI, if it has one.
	 * @param uri The URI for which a path object should be returned.
	 * @return An object representing the path if any.
	 */
	public static Optional<URIPath> findURIPath(@Nonnull final URI uri) {
		return Optional.ofNullable(uri.getRawPath()).map(URIPath::of);
	}

	/**
	 * Returns a path object to represent the path of the URI.
	 * @param uri The URI for which a path object should be returned.
	 * @return An object representing the path, or <code>null</code> if the URI has no path.
	 * @deprecated to remove in favor of {@link #findURIPath(URI)}
	 */
	@Deprecated
	public static URIPath getPath(final URI uri) {
		return findURIPath(uri).orElse(null);
	}

	/**
	 * Returns the raw name of the resource at the given URI's path, which will be the raw name of the last path component. If the path is a collection (i.e. it
	 * ends with slash), the component before the last slash will be returned. As examples, "/path/name.ext" and "name.ext" will return "name.ext". "/path/",
	 * "path/", and "path" will all return "path". A empty path will return "", while the root path will return "/".
	 * @apiNote The return value of "/" for <code>http://example.com</code> indicates that it is a special resource that doesn't have a name, but still
	 *          distinguishes between that case and the resource <code>http://example.com</code>, which has an empty relative path and is not equivalent to the
	 *          root. Thus "" and "/" should be considered special values indicating certain conditions, not actual names.
	 * @implSpec This method correctly handles {@value URIs#INFO_SCHEME} URIs.
	 * @implSpec This implementation calls {@link #getName(String)}.
	 * @param uri The URI the path of which will be examined.
	 * @return The name of the last last path component, the empty string if the path is the empty string, "/" if the path is the root path, or empty if the URI
	 *         has no path.
	 * @throws NullPointerException if the given URI is <code>null</code>.
	 */
	public static Optional<String> findRawName(@Nonnull final URI uri) {
		final String rawPath = uri.isOpaque() && INFO_SCHEME.equals(uri.getScheme()) ? uri.getRawSchemeSpecificPart() : uri.getRawPath(); //get the raw path, using the scheme-specific part of any info URI
		return Optional.ofNullable(rawPath).map(URIs::getName); //if we have a raw path, return the name
	}

	/**
	 * Returns the raw name of the resource at the given URI's path, which will be the raw name of the last path component. If the path is a collection (i.e. it
	 * ends with slash), the component before the last slash will be returned. As examples, "/path/name.ext" and "name.ext" will return "name.ext". "/path/",
	 * "path/", and "path" will all return "path".
	 * @apiNote The return value of "/" for <code>http://example.com</code> indicates that it is a special resource that doesn't have a name, but still
	 *          distinguishes between that case and the resource <code>http://example.com</code>, which has an empty relative path and is not equivalent to the
	 *          root. Thus "" and "/" should be considered special values indicating certain conditions, not actual names.
	 * @implSpec This method correctly handles {@value URIs#INFO_SCHEME} URIs.
	 * @param uri The URI the path of which will be examined.
	 * @return The name of the last last path component, the empty string if the path is the empty string, "/" if the path is the root path, or <code>null</code>
	 *         if the URI has no path.
	 * @throws NullPointerException if the given URI is <code>null</code>.
	 * @deprecated to be removed in favor of {@link #findRawName(URI)}.
	 */
	@Deprecated
	public static String getRawName(final URI uri) {
		return findRawName(uri).orElse(null);
	}

	/**
	 * Returns the decoded name of the resource at the given URI's path, which will be the decoded name of the last path component. If the path is a collection
	 * (i.e. it ends with slash), the component before the last slash will be returned. As examples, "/path/name.ext" and "name.ext" will return "name.ext".
	 * "/path/", "path/", and "path" will all return "path". The path name is first extracted from the URI's raw path and then decoded so that encoded
	 * {@value URIs#PATH_SEPARATOR} characters will not prevent correct parsing.
	 * @apiNote The return value of "/" for <code>http://example.com</code> indicates that it is a special resource that doesn't have a name, but still
	 *          distinguishes between that case and the resource <code>http://example.com</code>, which has an empty relative path and is not equivalent to the
	 *          root. Thus "" and "/" should be considered special values indicating certain conditions, not actual names.
	 * @implSpec This method correctly handles {@value URIs#INFO_SCHEME} URIs.
	 * @param uri The URI the path of which will be examined.
	 * @return The name of the last path component, the empty string if the path is the empty string, "/" if the path is the root path, or empty if the URI has no
	 *         path.
	 * @throws NullPointerException if the given URI is <code>null</code>.
	 */
	public static Optional<String> findName(final URI uri) {
		return findRawName(uri).map(URIs::decode);
	}

	/**
	 * Returns the decoded name of the resource at the given URI's path, which will be the decoded name of the last path component. If the path is a collection
	 * (i.e. it ends with slash), the component before the last slash will be returned. As examples, "/path/name.ext" and "name.ext" will return "name.ext".
	 * "/path/", "path/", and "path" will all return "path". The path name is first extracted from the URI's raw path and then decoded so that encoded
	 * {@value URIs#PATH_SEPARATOR} characters will not prevent correct parsing.
	 * @apiNote The return value of "/" for <code>http://example.com</code> indicates that it is a special resource that doesn't have a name, but still
	 *          distinguishes between that case and the resource <code>http://example.com</code>, which has an empty relative path and is not equivalent to the
	 *          root. Thus "" and "/" should be considered special values indicating certain conditions, not actual names.
	 * @implSpec This method correctly handles {@value URIs#INFO_SCHEME} URIs.
	 * @param uri The URI the path of which will be examined.
	 * @return The name of the last path component, the empty string if the path is the empty string, "/" if the path is the root path, or <code>null</code> if
	 *         the URI has no path.
	 * @throws NullPointerException if the given URI is <code>null</code>.
	 * @deprecated to be removed in favor of {@link #findName(URI)}.
	 */
	@Deprecated
	public static String getName(final URI uri) {
		return findName(uri).orElse(null);
	}

	/**
	 * Changes the name of the path of the given URI to the given name. If the path is a collection (i.e. it ends with slash), the name is the last component
	 * before the last slash. As examples, "/path/name.ext" and "name.ext" will change "name.ext". "/path/", "path/", and "path" will all change "path". For a
	 * requested name of "name", a path of "" will return "name", and a path of "/" will return "/name/".
	 * @apiNote Neither an empty path or the root path are considered to have names, and thus are not allowed to have their names changed.
	 * @param path The path, which should be encoded if {@value URIs#PATH_SEPARATOR} characters are present.
	 * @param name The new name of the path.
	 * @return A new path with the name changed to the given name.
	 * @throws NullPointerException if the given path and/or name is <code>null</code>.
	 * @throws IllegalArgumentException if the given path is the empty string or is the root path.
	 * @see #getName(String)
	 */
	public static String changePathName(final String path, final String name) {
		requireNonNull(name, "Name cannot be null."); //TODO check to see if the name has illegal characters
		final int length = requireNonNull(path, "Path cannot be null.").length(); //get the length of the path
		if(length == 0) { //if there are no characters
			return name; //the empty path becomes the name itself
		}
		int endIndex = length; //start at the end of the path (endIndex will always be one position after the ending character)
		if(path.charAt(endIndex - 1) == PATH_SEPARATOR) { //if the path ends with a path separator
			--endIndex; //skip the ending path separator
		}
		final int beginIndex = path.lastIndexOf(PATH_SEPARATOR, endIndex - 1) + 1; //get the index after the previous separator; if there are no previous separators, this will correctly yield index 0
		final StringBuilder pathStringBuilder = new StringBuilder(path); //create a new string builder from the given path
		if(endIndex - beginIndex > 1) { //if there are characters to collect (there must be more than one position difference in the start and end positions, because the end position is the index after the last character)
			pathStringBuilder.replace(beginIndex, endIndex, name); //replace the found name with the new name
		} else { //if there are no characters to collect, this must be the root path ("/")
			assert ROOT_PATH.equals(path) : "Path unexpectedly not the root path.";
			pathStringBuilder.append(name).append(PATH_SEPARATOR); //append "name/" to the root path to yield "/name/"
		}
		return pathStringBuilder.toString(); //return the new path we determined
	}

	/**
	 * Changes the name of the path of the given URI to the given name. If the path is a collection (i.e. it ends with slash), the name is the last component
	 * before the last slash. As examples, "/path/name.ext" and "name.ext" will change "name.ext". "/path/", "path/", and "path" will all change "path". For a
	 * requested name of "name", a path of "" will return "name", and a path of "/" will return "/name/".
	 * @apiNote Neither an empty path or the root path are considered to have names, and thus are not allowed to have their names changed.
	 * @param path The path, which should be encoded if {@value URIs#PATH_SEPARATOR} characters are present.
	 * @param name The new name of the path.
	 * @return A new path with the name changed to the given name.
	 * @throws NullPointerException if the given path and/or name is <code>null</code>.
	 * @throws IllegalArgumentException if the given path is the empty string or is the root path.
	 * @see #getName(String)
	 * @deprecated to be removed in favor of {@link #changePathName(String, String)}
	 */
	@Deprecated
	public static String changeName(final String path, final String name) {
		return changePathName(path, name);
	}

	/**
	 * Changes the raw name of the path of the given URI to the given raw name. If the path is a collection (i.e. it ends with slash), the name is the last
	 * component before the last slash. As examples, "/path/name.ext" and "name.ext" will change "name.ext". "/path/", "path/", and "path" will all change "path".
	 * For a requested name of "name", a path of "" will return "name", and a path of "/" will return "/name/".
	 * @apiNote Neither an empty path or the root path are considered to have names, and thus are not allowed to have their names changed.
	 * @implSpec This method correctly handles {@value URIs#INFO_SCHEME} URIs.
	 * @param uri The URI the raw name of which to change.
	 * @param rawName The new raw name of the URI.
	 * @return A new URI with the raw name changed to the given raw name.
	 * @throws NullPointerException if the given URI and/or name is <code>null</code>.
	 * @throws IllegalArgumentException if the given URI has no path, if the path is empty, or if the path is just a "/".
	 * @see #findRawName(URI)
	 */
	public static URI changeRawName(final URI uri, final String rawName) {
		if(uri.isOpaque() && INFO_SCHEME.equals(uri.getScheme())) { //if this is an info URI
			final String rawSSP = uri.getRawSchemeSpecificPart(); //get the raw scheme-specific part
			final String newRawSSP = changePathName(rawSSP, rawName); //change the name to the given name
			return changeRawSchemeSpecificPart(uri, newRawSSP); //change the URI's scheme-specific part to the new scheme-specific part			
		} else { //if this is not an info URI
			final String rawPath = requireNonNull(uri, "URI cannot be null").getRawPath(); //get the raw path
			checkArgument(rawPath != null, "URI <%s> has no path.", uri);
			checkArgument(!rawPath.isEmpty(), "URI <%s> has no raw name, which cannot be changed to `%s`.", uri, rawName);
			checkArgument(!rawPath.equals(ROOT_PATH), "URI <%s> has only a root path with no raw name, which cannot be changed to `%s`.", uri, rawName);
			final String newRawPath = changePathName(rawPath, rawName); //change the name to the given name
			return changeRawPath(uri, newRawPath); //change the URI's raw path to the new raw path
		}
	}

	/**
	 * Changes the name of the path of the given URI to the given name. If the path is a collection (i.e. it ends with slash), the name is the last component
	 * before the last slash. As examples, "/path/name.ext" and "name.ext" will change "name.ext". "/path/", "path/", and "path" will all change "path". For a
	 * requested name of "name", a path of "" will return "name", and a path of "/" will return "/name/".
	 * @apiNote Neither an empty path or the root path are considered to have names, and thus are not allowed to have their names changed.
	 * @implSpec This method correctly handles {@value URIs#INFO_SCHEME} URIs.
	 * @param uri The URI the name of which to change.
	 * @param name The new unencoded name of the URI, which will be encoded.
	 * @return A new URI with the name changed to the given name.
	 * @throws NullPointerException if the given URI and/or name is <code>null</code>.
	 * @throws IllegalArgumentException if the given URI has no path, if the path is empty, or if the path is just a "/".
	 * @see URIPath#encodeSegment(String)
	 * @see #findName(URI)
	 */
	public static URI changeName(final URI uri, final String name) {
		return changeRawName(uri, URIPath.encodeSegment(name)); //encode the name and change the name of the URI's path
	}

	/**
	 * Appends a given string to the end of the URI's raw name before the extension, if any.
	 * @apiNote This is useful for forming a locale-aware name, such as <code>test_fr.txt</code> from <code>test.txt</code>.
	 * @apiNote Here "base name" refers to the name with <em>all</em> extensions removed. That is both <code>example.bar</code> and <code>example.foo.bar</code>
	 *          would result in a base name of <code>example</code>.
	 * @implSpec This implementation delegates to {@link Filenames#appendBase(String, CharSequence)}.
	 * @param uri The URI to examine.
	 * @param charSequence The characters to append to the raw name.
	 * @return A URI with the given character sequence appended before the name extension, if any.
	 * @throws IllegalArgumentException if the given URI has no raw name.
	 */
	public static URI appendRawNameBase(@Nonnull final URI uri, @Nonnull final CharSequence charSequence) {
		final String oldRawName = findRawName(uri)
				.orElseThrow(() -> new IllegalArgumentException(String.format("Cannot append to the name base of URI <%s>, which has no name.", uri)));
		if(charSequence.length() == 0) { //if there are no characters to add, short-circuit for efficiency
			return requireNonNull(uri);
		}
		final String newRawName = Filenames.appendBase(oldRawName, charSequence);
		return changeRawName(uri, newRawName);
	}

	/**
	 * Changes the base of a URI name, preserving the extension(s), if any.
	 * @apiNote Here "base name" refers to the filename with <em>all</em> extensions removed. That is both <code>example.bar</code> and
	 *          <code>example.foo.bar</code> would result in a base name of <code>example</code>.
	 * @implSpec This implementation delegates to {@link Filenames#changeBase(String, String)}.
	 * @param uri The URI to examine.
	 * @param base The new base to set.
	 * @return A URI with a raw name with the new base.
	 * @throws NullPointerException if the given URI's raw name and/or the new base is <code>null</code>.
	 * @throws IllegalArgumentException if the given URI's raw name and/or the new base is empty.
	 */
	public static URI changeRawNameBase(@Nonnull URI uri, @Nonnull final String base) {
		final String oldRawName = findRawName(uri)
				.orElseThrow(() -> new IllegalArgumentException(String.format("Cannot change the name base of URI <%s>, which has no name.", uri)));
		final String newRawName = Filenames.changeBase(oldRawName, base); //change the base of the name
		return changeRawName(uri, newRawName); //change the raw name of the URI
	}

	/**
	 * Adds the given extension to a URI name and returns the new URI with the new extension. The URI name is not checked to see if it currently has an extension.
	 * @param uri The URI the name of which an extension should be added.
	 * @param extension The raw, encoded extension to add.
	 * @return The URI with the new extension.
	 * @throws NullPointerException if the given extension is <code>null</code>.
	 * @throws IllegalArgumentException if the given URI has no path, if the path is empty, or if the path is just a "/".
	 */
	public static URI addRawNameExtension(final URI uri, final String extension) {
		final String rawName = findRawName(uri)
				.orElseThrow(() -> new IllegalArgumentException(String.format("Cannot add name extension to URI <%s>, which has no name.", uri)));
		return changeRawName(uri, Filenames.addExtension(rawName, extension));
	}

	/**
	 * Extracts the extension from a URI's name.
	 * @apiNote This this the preferred method for extracting an extension from a URI, as this method correctly parses the raw form of the URI path to find the
	 *          extension before decoding.
	 * @param uri The URI to examine.
	 * @return The extension of the URI's name (not including '.'), which may not be present.
	 * @see #findName(URI)
	 */
	public static Optional<String> findNameExtension(@Nonnull final URI uri) {
		return findRawNameExtension(uri).map(URIs::decode); //if there is a raw extension, decode it
	}

	/**
	 * Extracts the extension from a URI's name. This this the preferred method for extracting an extension from a URI, as this method correctly parses the raw
	 * form of the URI path to find the extension before decoding.
	 * @param uri The URI to examine.
	 * @return The extension of the URI's name (not including '.'), or <code>null</code> if no extension is present.
	 * @deprecated to be removed in favor of {@link #findNameExtension(URI)}.
	 */
	@Deprecated
	public static String getNameExtension(@Nonnull final URI uri) {
		return findNameExtension(uri).orElse(null);
	}

	/**
	 * Extracts the raw, encoded extension from a URI's name.
	 * @param uri The URI to examine.
	 * @return The raw, encoded extension of the URI's name (not including '.'), which may not be present.
	 * @see #findRawName(URI)
	 */
	public static Optional<String> findRawNameExtension(@Nonnull final URI uri) {
		return findRawName(uri).flatMap(Filenames::findExtension);
	}

	/**
	 * Extracts the raw, encoded extension from a URI's name.
	 * @param uri The URI to examine.
	 * @return The raw, encoded extension of the URI's name (not including '.'), or <code>null</code> if no extension is present.
	 * @deprecated to be removed in favor of {@link #findRawNameExtension(URI)}.
	 */
	@Deprecated
	public static String getRawNameExtension(@Nonnull final URI uri) {
		return findRawNameExtension(uri).orElse(null);
	}

	/**
	 * Changes the extension of a URI name and returns a new URI with the new name extension. If the URI name does not currently have an extension, one will be
	 * added.
	 * @param uri The URI to examine.
	 * @param extension The raw extension to set, or <code>null</code> if the extension should be removed.
	 * @return A URI with a raw name with the new extension.
	 * @throws IllegalArgumentException if the given URI has no path, if the path is empty, or if the path is just a "/".
	 */
	public static URI changeRawNameExtension(final URI uri, final String extension) {
		final String oldRawName = findRawName(uri)
				.orElseThrow(() -> new IllegalArgumentException(String.format("Cannot change the name extension of URI <%s>, which has no name.", uri)));
		final String newRawName = Filenames.changeExtension(oldRawName, extension); //change the extension of the name
		return changeRawName(uri, newRawName); //change the raw name of the URI
	}

	/**
	 * Adds the extension, if any, to a name and returns the new URI. This is a convenience method that delegates to {@link #addRawNameExtension(URI, String)} if
	 * a non-<code>null</code> extension is given.
	 * @param uri The URI to examine.
	 * @param extension The raw, encoded extension to add, or <code>null</code> if no extension should be added.
	 * @return A URI with a raw name with the new extension, if any.
	 */
	public static URI setRawNameExtension(final URI uri, final String extension) {
		return extension != null ? addRawNameExtension(uri, extension) : uri; //if an extension was given, add it; otherwise, return the URI unmodified
	}

	/**
	 * Removes the extension, if any, of a URI name and returns a new URI with no extension. This is a convenience method that delegates to
	 * {@link #changeRawNameExtension(URI, String)}.
	 * @param uri The URI to examine.
	 * @return The URI with no extension.
	 */
	public static URI removeRawNameExtension(final URI uri) {
		return changeRawNameExtension(uri, null); //replace the extension with nothing
	}

	/**
	 * Creates a new URI identical to the supplied URI with no query or fragment.
	 * @param uri The URI from which to remove the query and fragment, if any.
	 * @return A new URI with no query or fragment.
	 */
	public static URI getPlainURI(final URI uri) {
		return createURI(uri.getScheme(), uri.getRawUserInfo(), uri.getHost(), uri.getPort(), uri.getRawPath(), null, null); //construct an identical URI except with no query or fragment
	}

	/**
	 * Constructs a query string for a URI by URI-encoding each name-value pair, separating them with '&amp;', and prepending the entire string (if there is at
	 * least one parameter) with '?', if there are no parameters, it doesn't do anything.
	 * @param params The name-value pairs representing the query parameters.
	 * @return A string representing the constructed query, or the empty string if there were no parameters.
	 */
	public static String constructQuery(final URIQueryParameter... params) {
		return constructQuery(constructQueryParameters(params)); //construct a query, prepended with the query character
	}

	/**
	 * Constructs a query string for a URI by prepending the given query string, if it is not the empty string, with '?'.
	 * @param params The string representing the query parameters.
	 * @return A string representing the constructed query, or the empty string if there were no parameters.
	 */
	@Deprecated
	public static String constructQuery(final String params) {
		final StringBuilder query = new StringBuilder();
		if(params.length() > 0) { //if there is at least one parameter character
			query.append(QUERY_SEPARATOR); //append the query prefix
			query.append(params); //append the params
		}
		return query.toString(); //return the query string we constructed
	}

	/**
	 * Appends a query string to a URI.
	 * @param uri The existing URI.
	 * @param rawQuery The encoded query information, without a beginning query separator.
	 * @return A URI representing the URI with the appended query parameters.
	 * @throws NullPointerException if the given URI and/or query is <code>null</code>.
	 */
	public static URI appendRawQuery(final URI uri, final String rawQuery) {
		final StringBuilder stringBuilder = new StringBuilder(uri.toString()); //create a string builder from the URI
		stringBuilder.append(uri.getRawQuery() != null ? QUERY_NAME_VALUE_PAIR_DELIMITER : QUERY_SEPARATOR); //if there already is a query, separate the new parameters from the existing ones; otherwise, add the query introduction character
		stringBuilder.append(requireNonNull(rawQuery, "Query cannot be null.")); //add the new query information
		return URI.create(stringBuilder.toString()); //return the new URI
	}

	/**
	 * Constructs a query string for a URI with a single name/value parameter, and appends it to the query of the given URI, if any.
	 * @param uri The existing URI.
	 * @param paramName The name of the parameter.
	 * @param paramValue The value of the parameter.
	 * @return A URI representing the URI with the appended query parameter.
	 * @throws NullPointerException if the given URI, param name, and/or param value is <code>null</code>.
	 */
	public static URI appendQueryParameter(final URI uri, final String paramName, final String paramValue) {
		return appendQueryParameters(uri, new URIQueryParameter(paramName, paramValue));
	}

	/**
	 * Constructs a query string for a URI and appends it to the query of the given URI, if any.
	 * @param uri The existing URI.
	 * @param params The name-value pairs representing the query parameters.
	 * @return A URI representing the URI with the appended query parameters.
	 * @throws NullPointerException if the given URI and/or params is <code>null</code>.
	 */
	public static URI appendQueryParameters(final URI uri, final URIQueryParameter... params) {
		if(params.length > 0) { //if there are parameters
			return appendRawQuery(uri, constructQueryParameters(params)); //add the new query parameters and return the resulting URI
		} else { //if there are no parameters
			return uri; //return the URI as-is
		}
	}

	/**
	 * Constructs a query string for a URI and appends it to the given query, if any.
	 * @param query The existing query parameters, or <code>null</code> or the empty string if there is no query.
	 * @param params The name-value pairs representing the query parameters.
	 * @return A string representing the query with the appended parameters, or the empty string if there was no query and there were no parameters.
	 */
	public static String appendQueryParameters(final String query, final URIQueryParameter... params) {
		final String queryParameters = constructQueryParameters(params); //get query parameters
		return query != null && query.length() > 0 ? query + QUERY_NAME_VALUE_PAIR_DELIMITER + queryParameters : queryParameters; //if there was a query, append the new parameters; otherwise, just return the parameters
	}

	/**
	 * Constructs a query string for a URI by URI-encoding each name-value pair, separating them with '&amp;'. Parameters are allowed to have <code>null</code>
	 * values, in which case no '=' delimiter will be used.
	 * @param params The name-value pairs representing the query parameters.
	 * @return A string representing the constructed query, or the empty string if there were no parameters.
	 */
	public static String constructQueryParameters(final URIQueryParameter... params) {
		final StringBuilder paramStringBuilder = new StringBuilder();
		if(params.length > 0) { //if there is at least one parameter
			for(NameValuePair<String, String> param : params) { //look at each parameter
				paramStringBuilder.append(encode(param.getName())); //append the parameter name
				final String value = param.getValue();
				if(value != null) { //if there is a value
					paramStringBuilder.append(QUERY_NAME_VALUE_ASSIGNMENT); //append the value-assignment character
					paramStringBuilder.append(encode(param.getValue())); //append the parameter value
				}
				paramStringBuilder.append(QUERY_NAME_VALUE_PAIR_DELIMITER); //append the name-value pair delimiter
			}
			paramStringBuilder.delete(paramStringBuilder.length() - 1, paramStringBuilder.length()); //remove the last name-value pair delimiter
		}
		return paramStringBuilder.toString(); //return the query parameter string we constructed
	}

	/**
	 * Creates a path-based query from a standard URI query. A query in the form <code>?var1=value1&amp;var2=value2</code> will be converted to the form
	 * <code>/var1%3Dvalue1/var2&3Dvalue2</code>.
	 * @param query The standard URI query string, optionally beginning with '?'.
	 * @return A query string converted to a path. A query string beginning with '?' will be converted into an absolute path.
	 */
	/*TODO fix; do *not* use URLEncoder, which is for www-encoded forms	
		public static String createPathQuery(final String query)
		{
			if(query.length()>0) {	//if the query has at least one character
				final StringBuilder stringBuilder=new StringBuilder();	//create a string builder for creating a path query
				final int startIndex;	//find out if we should skip the first character			
				if(query.charAt(0)==QUERY_SEPARATOR) {	//if the first character is '?'
					stringBuilder.append(PATH_SEPARATOR);	//convert it to a '/'
					startIndex=1;	//skip the introductory character
				}
				else {	//if the string doesn't begin with '?'
					startIndex=0;	//we'll just start at the first
				}
					//tokenize the string on the attribute delimiter, '&'
				final StringTokenizer stringTokenizer=new StringTokenizer(query.substring(startIndex), String.valueOf(QUERY_NAME_VALUE_PAIR_DELIMITER));
				while(stringTokenizer.hasMoreTokens()) {	//while there are more tokens
					final String token=stringTokenizer.nextToken();	//get the next token
					try
					{
						stringBuilder.append(URLEncoder.encode(token, UTF_8));	//encode and append the next token
					}
					catch(UnsupportedEncodingException unsupportedEncodingException) {	//we should always support UTF-8
						throw new AssertionError(unsupportedEncodingException);
					}					
					if(stringTokenizer.hasMoreTokens()) {	//if there are more tokens
						stringBuilder.append(PATH_SEPARATOR);	//add a path separator, '/'					
					}
				}
				return stringBuilder.toString();	//return the string we constructed
			}
			else {	//if the query is empty
				return query;	//return the query as it is
			}		
		}
	*/

	/**
	 * Creates a standard URI based query from a path-based query. A query in the form <code>/var1%3Dvalue1/var2&3Dvalue2</code> will be converted to the form
	 * <code>?var1=value1&amp;var2=value2</code>.
	 * @param pathQuery The standard URI query string, optionally beginning with '?'.
	 * @return A query string converted to a path. A query string beginning with '?' will be converted into an absolute path.
	 */
	/*TODO fix; do *not* use URLEncoder, which is for www-encoded forms	
		public static String createQuery(final String pathQuery)
		{
			if(pathQuery.length()>0) {	//if the query has at least one character
				final StringBuilder stringBuilder=new StringBuilder();	//create a string builder for creating a query
				final int startIndex;	//find out if we should skip the first character			
				if(pathQuery.charAt(0)==PATH_SEPARATOR) {	//if the first character is '/'
					stringBuilder.append(QUERY_SEPARATOR);	//convert it to a '?'
					startIndex=1;	//skip the introductory character
				}
				else {	//if the string doesn't begin with '/'
					startIndex=0;	//we'll just start at the first
				}
					//tokenize the string on path separators, '/'
				final StringTokenizer stringTokenizer=new StringTokenizer(pathQuery.substring(startIndex), String.valueOf(PATH_SEPARATOR));
				while(stringTokenizer.hasMoreTokens()) {	//while there are more tokens
					final String token=stringTokenizer.nextToken();	//get the next token
					try
					{
						stringBuilder.append(URLDecoder.decode(token, UTF_8));	//encode and append the next token
					}
					catch(UnsupportedEncodingException unsupportedEncodingException) {	//we should always support UTF-8
						throw new AssertionError(unsupportedEncodingException);
					}					
					if(stringTokenizer.hasMoreTokens()) {	//if there are more tokens
						stringBuilder.append(QUERY_NAME_VALUE_PAIR_DELIMITER);	//add a query name/value pair separator, '&'					
					}
				}
				return stringBuilder.toString();	//return the string we constructed
			}
			else {	//if the path query is empty
				return pathQuery;	//return the path query as it is
			}		
		}
	*/

	/**
	 * Retrieves name-value parameters from a standard URI query string.
	 * @param query The URI query string, optionally beginning with a '?' character.
	 * @return An array of name-value pairs representing query parameters.
	 */
	/*TODO fix; do *not* use URLEncoder, which is for www-encoded forms	
		public static NameValuePair<String, String>[] getQueryParameters(final String query)
		{
			final List<NameValuePair<String, String>> parameterList=new ArrayList<NameValuePair<String, String>>();	//create a list to hold our parameters
			if(query.length()>0) {	//if the query has at least one character
				final int startIndex;	//find out if we should skip the first character			
				if(query.charAt(0)==QUERY_SEPARATOR) {	//if the first character is '?'
					startIndex=1;	//skip the introductory character
				}
				else {	//if the string doesn't begin with '?'
					startIndex=0;	//we'll just start at the first
				}
					//tokenize the string on the attribute delimiter, '&'
				final StringTokenizer stringTokenizer=new StringTokenizer(query.substring(startIndex), String.valueOf(QUERY_NAME_VALUE_PAIR_DELIMITER));
				while(stringTokenizer.hasMoreTokens()) {	//while there are more tokens
					final String token=stringTokenizer.nextToken();	//get the next token
					final int equalsIndex=token.indexOf(QUERY_NAME_VALUE_ASSIGNMENT);	//get the index of the '=' character
					final String name;	//we'll determine the name and the value
					final String value;
					if(equalsIndex>=0) {	//if there is an equals character
						name=token.substring(0, equalsIndex);	//the name is everything up to but not including the '='
						value=token.substring(equalsIndex+1);	//the value is everything after the '='
					}
					else {	//if there is no equals character
						name=token;	//take the token as it is for the name
						value=null;	//there is no value TODO is this the correct thing to do? should it be ""?
					}
					parameterList.add(new NameValuePair<String, String>(name, value));	//add this parameter to the list 
				}
			}
			return parameterList.toArray(new NameValuePair[parameterList.size()]);	//return the list as an array
		}
	*/

	/**
	 * Retrieves the parameters from the query of a URI, if present.
	 * @param uri The URI from which to extract parameters.
	 * @return An array of parameters.
	 */
	public static CollectionMap<String, String, List<String>> getParameterMap(final URI uri) {
		final NameValuePair<String, String>[] parameters = getParameters(uri); //get the parameters from the URI
		final CollectionMap<String, String, List<String>> parameterListMap = new ArrayListHashMap<String, String>(); //create a new list map in which to store the parameters
		if(parameters != null) { //if this URI specified a query
			for(final NameValuePair<String, String> parameter : parameters) { //for each parameter
				parameterListMap.addItem(parameter.getName(), parameter.getValue()); //add this name and value, each of which may have been encoded
			}
		}
		return parameterListMap; //return the parameters, if any
	}

	/**
	 * Retrieves the query parameters from a URI.
	 * @param uri The URI which may contain a query.
	 * @return An array of parameters represented by the URI query, or <code>null</code> if the given URI does not contain a query.
	 */
	public static NameValuePair<String, String>[] getParameters(final URI uri) {
		return getParameters(uri.getRawQuery()); //return the parameters for this URI query, if there is a query
	}

	/**
	 * Retrieves the parameters from a URI query. An empty string query will return an empty array of name/value pairs.
	 * @param query The string containing URI query parameters (without the '?' prefix), or <code>null</code>.
	 * @return An array of parameters represented by the query, or <code>null</code> if the given query is <code>null</code>.
	 */
	@SuppressWarnings("unchecked")
	//we can't check the creation of a generic array
	public static NameValuePair<String, String>[] getParameters(final String query) {
		if(query != null) { //if a query was given
			if(query.length() == 0) { //if there is no query in the string
				return new NameValuePair[0]; //return an empty array
			}
			final String[] parameterStrings = query.split(String.valueOf(QUERY_NAME_VALUE_PAIR_DELIMITER)); //split the query into parameters
			final NameValuePair<String, String>[] parameters = new NameValuePair[parameterStrings.length]; //create an array to hold parameters
			int i = 0;
			for(final String parameterString : parameterStrings) { //for each parameters
				final String[] nameValue = parameterString.split(String.valueOf(QUERY_NAME_VALUE_ASSIGNMENT)); //split the parameter into its name and value
				final String name; //we'll get the parameter name
				final String value; //we'll get the parameter value
				if(nameValue.length > 0) { //if there was at least one token
					name = decode(nameValue[0]); //the first token is the name
					value = nameValue.length > 1 ? decode(nameValue[1]) : ""; //use the empty string for the value if no value was provided
				} else { //if there wasn't at least one token
					name = value = ""; //there is no name or value
				}
				parameters[i++] = new NameValuePair<String, String>(name, value); //create a new parameter and advance to the next index
			}
			return parameters; //return the parameters
		} else { //if no query is given
			return null; //there are no parameters
		}
	}

	/**
	 * Creates a URI from the given path, verifying that the string contains only a path.
	 * @param path The string version of a path to convert to a URI form of that same path.
	 * @return The URI constructed.
	 * @throws NullPointerException if the given path is <code>null</code>.
	 * @throws IllegalArgumentException if the provided path specifies a URI scheme (i.e. the URI is absolute) and/or authority.
	 * @see #isPathURI(URI)
	 */
	public static URI createPathURI(final String path) {
		final URI pathURI = URI.create(requireNonNull(path, "Path cannot be null")); //create a URI from the given path
		if(!isPathURI(pathURI)) { //if there is a scheme or an authority
			throw new IllegalArgumentException("Path cannot have a URI scheme or authority, and must include a path: " + path);
		}
		return pathURI; //return the URI we created
	}

	/**
	 * Checks to see if a given URI has the root path. If the given URI does not have the root path, i.e., if it's missing or if it's not composed only by the
	 * root path, an exception is thrown.
	 * @param uri The URI to check to see if it has the root path.
	 * @return The given root URI.
	 * @throws NullPointerException if the given URI is <code>null</code>.
	 * @throws IllegalArgumentException if the given URI does not have the root path.
	 * @see URI#getPath()
	 * @see #ROOT_PATH
	 */
	public static URI checkRoot(final URI uri) throws IllegalArgumentException {
		if(!ROOT_PATH.equals(uri.getPath())) { //if the given URI does not have the root path
			throw new IllegalArgumentException("The given URI " + uri + " is a root URI.");
		}
		return uri; //return the root URI
	}

	/**
	 * Checks to see if a given URI is absolute. If the given URI is not absolute, an exception is thrown.
	 * @param uri The URI to check to see if it is absolute.
	 * @return The given absolute URI.
	 * @throws NullPointerException if the given URI is <code>null</code>.
	 * @throws IllegalArgumentException if the given URI is not absolute.
	 * @see URI#isAbsolute()
	 */
	public static URI checkAbsolute(final URI uri) throws IllegalArgumentException {
		if(!uri.isAbsolute()) { //if the given URI is not absolute
			throw new IllegalArgumentException("The given URI " + uri + " is not absolute.");
		}
		return uri; //return the absolute URI
	}

	/**
	 * Determines if a given URI contains only a path and does not have a scheme, authority, query, and/or fragment.
	 * @param uri The URI to check to for path status.
	 * @throws NullPointerException if the given URI is <code>null</code>.
	 * @return <code>true</code> if the URI has a path and does not specify a scheme (i.e. the URI is not absolute), authority, query, or fragment.
	 */
	public static boolean isPathURI(final URI uri) {
		requireNonNull(uri, "URI cannot be null");
		return uri.getScheme() == null && uri.getRawAuthority() == null && uri.getPath() != null && isPlainURI(uri); //see if there is no scheme, no authority, a path, no query, and no fragment
	}

	/**
	 * Checks to see if a given URI is only a path and not a URI with a scheme, authority, query, and/or fragment. If the given URI is not a path, an exception is
	 * thrown.
	 * @param uri The URI to check to for path status.
	 * @return The given path URI.
	 * @throws NullPointerException if the given URI is <code>null</code>.
	 * @throws IllegalArgumentException if the provided URI specifies a URI scheme (i.e. the URI is absolute), authority, query, and/or fragment.
	 * @throws IllegalArgumentException if the given URI is not a path.
	 * @see #isPath(String)
	 */
	public static URI checkPathURI(final URI uri) {
		if(!isPathURI(uri)) { //if the URI is not a path
			throw new IllegalArgumentException("The given URI " + uri + " is not a valid sole path URI.");
		}
		return uri; //return the path URI
	}

	/**
	 * Determines if a given URI is plain, i.e. it does not contain a query or a fragment.
	 * @param uri The URI to check to for plainness.
	 * @throws NullPointerException if the given URI is <code>null</code>.
	 * @return <code>true</code> if the URI has no query or fragment.
	 */
	public static boolean isPlainURI(final URI uri) {
		requireNonNull(uri, "URI cannot be null");
		return uri.getRawQuery() == null && uri.getRawFragment() == null; //see if there is no query and no fragment
	}

	/**
	 * Checks to see if a given URI is plain, i.e. it does not contain a query or a fragment. If the given URI is not a plain URI, an exception is thrown.
	 * @param uri The URI to check to for plainness.
	 * @return The given plain URI.
	 * @throws NullPointerException if the given URI is <code>null</code>.
	 * @throws IllegalArgumentException if the provided URI specifies a query and/or fragment.
	 * @see #isPlainURI(URI)
	 */
	public static URI checkPlainURI(final URI uri) {
		checkArgument(isPlainURI(uri), "The given URI %s is not a plain URI.", uri);
		return uri; //return the plain URI
	}

	/**
	 * Determines the current level of a hierarchical URI. This is equivalent to resolving the path {@value URIs#CURRENT_LEVEL_PATH_SEGMENT} to the URI.
	 * @param uri The URI to examine.
	 * @return A URI representing the current hierarchical level of a hierarchical URI.
	 */
	public static URI getCurrentLevel(final URI uri) {
		return resolve(uri, CURRENT_LEVEL_PATH_SEGMENT); //resolve the URI to "."
	}

	/**
	 * Determines the parent level of a hierarchical URI. This is equivalent to resolving the path {@value URIs#PARENT_LEVEL_PATH_SEGMENT} to the URI for all
	 * non-root paths.
	 * @param uri The URI to examine.
	 * @return A URI representing the parent hierarchical level of a hierarchical URI, or <code>null</code> if the given URI's path equates to the root path.
	 */
	public static URI getParentLevel(final URI uri) {
		final URI parentLevel = resolve(uri, PARENT_LEVEL_PATH_SEGMENT); //resolve the URI to ".."
		if(ROOT_PATH_PARENT_LEVEL.equals(parentLevel.getRawPath())) { //if the result is /..
			return null; //the original path indicated the root
		}
		return parentLevel;
	}

	/**
	 * Determines the parent collection of a hierarchical URI.
	 * <p>
	 * Non-normalized collection paths (e.g. ending with <code>/.</code>) are not supported.
	 * </p>
	 * @param uri The URI to examine.
	 * @return A URI representing the parent collection of a hierarchical URI; if the URI ends in '/', equivalent to resolving the path ".." to the URI; if the
	 *         URI does not end in '/', equivalent to resolving the path "." to the URI; or <code>null</code> if the given URI's path equates to the root path.
	 * @throws IllegalArgumentException if the URI does not have a path component.
	 * @see #isCollectionURI(URI)
	 * @see #getParentLevel(URI)
	 * @see #getCurrentLevel(URI)
	 */
	public static URI getParentURI(final URI uri) {
		return isCollectionURI(uri) ? getParentLevel(uri) : getCurrentLevel(uri); //if the path ends with a slash, get the parent level; otherwise, get the current level
	}

	/**
	 * Determines the canonical root URI of a URI.
	 * @param uri The URI to examine.
	 * @return A URI representing the URI with no path and no query or fragment.
	 */
	public static URI getRootURI(final URI uri) {
		return createURI(uri.getScheme(), uri.getRawUserInfo(), uri.getHost(), uri.getPort(), (URIPath)null, null, null);
	}

	/**
	 * Returns a URI relative to the given parent URI. A collection URI relativized against itself will return an empty URI.
	 * <p>
	 * This method does not support backtracking, that is, creating a path to a parent or child path. For that functionality use
	 * {@link #findRelativePath(URI, URI)} instead.
	 * </p>
	 * @apiNote This method differs from {@link URI#relativize(URI)} in that the parent URI must be a collection URI (i.e. end with a slash) for a relative child
	 *          path to be found.
	 * @implSpec This method delegates to {@link #findRelativeChildPath(URI, URI)}
	 * @param parentURI The URI to which the child URI is relative.
	 * @param childURI The URI that will be relativized against the parent URI.
	 * @return The URI representing path of the child URI to the parent URI, or the child URI if the given child URI is not a child of the given parent URI.
	 * @see #normalize(URI)
	 */
	public static URI relativizeChildPath(final URI parentURI, final URI childURI) {
		return findRelativeChildPath(parentURI, childURI).orElse(childURI);
	}

	/**
	 * Returns a URI relative to the given parent URI. A collection URI relativized against itself will return an empty URI.
	 * <p>
	 * This method does not support backtracking, that is, creating a path to a parent or child path. For that functionality use
	 * {@link #findRelativePath(URI, URI)} instead.
	 * </p>
	 * @apiNote This method differs from {@link URI#relativize(URI)} in that the parent URI must be a collection URI (i.e. end with a slash) for a relative child
	 *          path to be found.
	 * @implSpec This method first normalizes both URIs.
	 * @param parentURI The URI to which the child URI is relative.
	 * @param childURI The URI that will be relativized against the parent URI.
	 * @return The URI representing path of the child URI to the parent URI, which will not be present if the given child URI is not a child of the given parent
	 *         URI.
	 * @see #normalize(URI)
	 */
	public static Optional<URI> findRelativeChildPath(@Nonnull final URI parentURI, @Nonnull final URI childURI) {
		if(isCollectionURI(parentURI)) {
			final URI relativeURI = normalize(parentURI).relativize(normalize(childURI)); //get a relative URI
			if(!relativeURI.isAbsolute()) {
				return Optional.of(relativeURI);
			}
		}
		return Optional.empty();
	}

	/**
	 * Returns the path of a target URI relative to some source URI, which may be a sibling URI or even a child URI. A collection URI relativized against itself
	 * will return an empty URI. A non-collection URI relativized against its parent will return <code>./</code>. Otherwise if the source URI is not a parent of
	 * (or the same URI as) the target URI, the path will backtrack using <code>..</code> path segments as appropriate.
	 * @implSpec This method delegates to {@link #findRelativePath(URI, URI)}
	 * @implNote This implementation properly relativizes URIs that require backtracking, such as siblings, unlike Java URI relativization methods; see
	 *           <a href="https://bugs.java.com/bugdatabase/view_bug.do?bug_id=6226081">JDK-6226081</a>.
	 * @implNote This method differs from {@link URI#relativize(URI)}, which would return an empty URI when relativizing <code>foo/bar</code> against
	 *           <code>foo/</code>. This method instead would return <code>./</code>, compliant with browser relative resolution behavior and with
	 *           <a href="https://tools.ietf.org/html/rfc3986">RFC 3986</a>, as discussed at <a href="https://stackoverflow.com/q/22203111/421049">Is Java's
	 *           URI.resolve incompatible with RFC 3986 when the relative URI contains an empty path?</a>.
	 * @param sourceURI The URI to which the other URI will be relativized.
	 * @param targetURI The URI that will be relativized against the base URI.
	 * @return The relative path of the source URI to the target URI, or the target URI if the two URIs have no base in common.
	 * @see #normalize(URI)
	 */
	public static URI relativizePath(@Nonnull final URI sourceURI, @Nonnull final URI targetURI) {
		return findRelativePath(sourceURI, targetURI).orElse(targetURI);
	}

	/**
	 * Returns the path of a target URI relative to some source URI, which may be a sibling URI or even a child URI. A collection URI relativized against itself
	 * will return an empty URI. A non-collection URI relativized against its parent will return <code>./</code>. Otherwise if the source URI is not a parent of
	 * (or the same URI as) the target URI, the path will backtrack using <code>..</code> path segments as appropriate.
	 * @implSpec This method first normalizes both URIs.
	 * @implNote This implementation properly relativizes URIs that require backtracking, such as siblings, unlike Java URI relativization methods; see
	 *           <a href="https://bugs.java.com/bugdatabase/view_bug.do?bug_id=6226081">JDK-6226081</a>.
	 * @implNote This method differs from {@link URI#relativize(URI)}, which would return an empty URI when relativizing <code>foo/bar</code> against
	 *           <code>foo/</code>. This method instead would return <code>./</code>, compliant with browser relative resolution behavior and with
	 *           <a href="https://tools.ietf.org/html/rfc3986">RFC 3986</a>, as discussed at <a href="https://stackoverflow.com/q/22203111/421049">Is Java's
	 *           URI.resolve incompatible with RFC 3986 when the relative URI contains an empty path?</a>.
	 * @param sourceURI The URI to which the other URI will be relativized.
	 * @param targetURI The URI that will be relativized against the base URI.
	 * @return The relative path of the source URI to the target URI, which will not be present if the two URIs have no base in common.
	 * @throws IllegalArgumentException if the two URIs have no base URI in common.
	 * @see #normalize(URI)
	 */
	public static Optional<URI> findRelativePath(@Nonnull URI sourceURI, @Nonnull URI targetURI) {
		sourceURI = normalize(sourceURI);
		targetURI = normalize(targetURI);
		StringBuilder backtrackPathBuilder = null; //only backtrack if we need to, but keep the string builder around for efficiency each time
		final URI currentLevelURI = getCurrentLevel(sourceURI); //normalize and remove any file from the base path TODO but will file URIs for directories end in /?
		URI parentURI = currentLevelURI;
		URI relativeURI;
		while((relativeURI = parentURI.relativize(targetURI)).isAbsolute()) { //keep looking for a relative path
			if(backtrackPathBuilder == null) { //if this is the first attempt and finding a common parent
				backtrackPathBuilder = new StringBuilder(); //lazily create the string builder for back tracking
			}
			backtrackPathBuilder.append(PARENT_LEVEL_PATH); //<../>
			parentURI = getParentLevel(parentURI); //move the parent URI up a level; equivalent to getParentURI(parentURI)
			if(parentURI == null) { //if we ran out of parents
				return Optional.empty();
			}
		}
		//At this point relativeURI will have a relative path to the URI _from the common parent_,
		//so we need to prepend enough backtracking to get to the common parent.
		assert parentURI != null && parentURI.isAbsolute();
		assert relativeURI != null && !relativeURI.isAbsolute();
		if(backtrackPathBuilder != null) { //if we backtracked, add the extra resolution back to the original base
			assert backtrackPathBuilder.length() > 0;
			relativeURI = URI.create(backtrackPathBuilder.toString()).resolve(relativeURI); //prepend backtracking
		}
		//if Java relativizes the relative path using "" but the source URI (e.g. `foo/bar`) was a child of a collection (e.g. `foo/`)
		if(relativeURI.equals(EMPTY_PATH_URI) && !sourceURI.equals(currentLevelURI)) {
			relativeURI = CURRENT_LEVEL_PATH_URI; //switch to using the form `./` for RFC 3986 compliance
		}
		return Optional.of(relativeURI);
	}

	/**
	 * Creates a URN in the form <code>urn:<var>nid</var>:nss</code>.
	 * @param nid The namespace identifier.
	 * @param nss The namespace-specific string.
	 * @return A URN based upon the given parameters.
	 * @see <a href="http://www.ietf.org/rfc/rfc2141.txt">RFC 2141</a>
	 * @throws IllegalArgumentException if the resulting string violates RFC 2396.
	 */
	public static URI createURN(final String nid, final String nss) {
		return URI.create(URN_SCHEME + SCHEME_SEPARATOR + nid + SCHEME_SEPARATOR + nss); //construct and return the URN
	}

	/**
	 * Creates an {@value URIs#INFO_SCHEME} URI with the given info namespace and identifier with no fragment.
	 * @param namespace The info namespace.
	 * @param rawIdentifier The raw, encoded info identifier.
	 * @return An info URI based upon the given parameters.
	 * @see <a href="http://www.ietf.org/rfc/rfc4452.txt">RFC 4452</a>
	 * @throws NullPointerException if the given namespace and/or identifier is <code>null</code>.
	 * @throws IllegalArgumentException if the given namespace, and/or identifier result in an invalid URI.
	 */
	public static URI createInfoURI(final String namespace, final String rawIdentifier) {
		return createInfoURI(namespace, rawIdentifier, null); //create an info URI with no fragment
	}

	/**
	 * Creates an {@value URIs#INFO_SCHEME} URI with the given info namespace, identifier, and optional fragment.
	 * @param namespace The info namespace.
	 * @param rawIdentifier The raw, encoded info identifier.
	 * @param rawFragment The raw, encoded fragment, or <code>null</code> if there should be no fragment
	 * @return An info URI based upon the given parameters.
	 * @see <a href="http://www.ietf.org/rfc/rfc4452.txt">RFC 4452</a>
	 * @throws NullPointerException if the given namespace and/or identifier is <code>null</code>.
	 * @throws IllegalArgumentException if the given namespace, identifier, and/or fragment result in an invalid URI.
	 */
	public static URI createInfoURI(final String namespace, final String rawIdentifier, final String rawFragment) {
		final StringBuilder stringBuilder = new StringBuilder(); //create a string builder
		stringBuilder.append(INFO_SCHEME).append(SCHEME_SEPARATOR).append(requireNonNull(namespace, "Namespace cannot be null."))
				.append(INFO_SCHEME_NAMESPACE_DELIMITER); //info:namespace/
		stringBuilder.append(rawIdentifier); //identifier
		if(rawFragment != null) { //if there is a fragment
			stringBuilder.append(FRAGMENT_SEPARATOR).append(rawFragment); //#fragment
		}
		return URI.create(stringBuilder.toString()); //construct and return an info URI from the string builder
	}

	/**
	 * Creates a {@value URIs#MAILTO_SCHEME} URI in the form <code>mailto:<var>username</var>@<var>domain</var></code>. The username and domain will be
	 * URI-encoded.
	 * @param username The mail username.
	 * @param domain The mail domain.
	 * @return A <code>mailto</code> URI based upon the given parameters.
	 * @see <a href="http://www.ietf.org/rfc/rfc2368.txt">RFC 2368</a>
	 * @throws NullPointerException if the given username and/or domain is <code>null</code>.
	 */
	public static URI createMailtoURI(final String username, final String domain) {
		return URI.create(MAILTO_SCHEME + SCHEME_SEPARATOR + encode(username) + MAILTO_USERNAME_DOMAIN_SEPARATOR + encode(domain)); //construct and return the mailto URI
	}

	/**
	 * Creates a URI from the given URI string relative to the given context object.
	 * @param contextObject The source context, such as a URL, a URI, a File, or <code>null</code> if the filename should not be referenced from any object.
	 * @param string The string version of a URI, either relative or absolute, or a URI fragment beginning with "#".
	 * @return A URI constructed from the URI string and context object.
	 * @throws URISyntaxException Thrown if the context object and string cannot be used to create a valid URI.
	 * @see File
	 * @see URI
	 * @see URL
	 * @deprecated because this ancient code doesn't have an obvious utility, is confusing, and jumbles various types, many of them legacy.
	 */
	@Deprecated
	public static URI createURI(final Object contextObject, final String string) throws URISyntaxException {
		if(contextObject instanceof URI) { //if the context is a URI
			//TODO if the string contains illegal URI characters, such as spaces, this won't work
			//TODO also check to see if the string is null.
			return resolve((URI)contextObject, new URI(string)); //resolve the URI form of the string, creating a URISyntaxException if there is a problem
		} else if(contextObject instanceof URL) { //if the context is a URL
			return resolve(((URL)contextObject).toURI(), string); //convert the URL to a URI and use it as a context
		} else if(contextObject instanceof File) { //if the context object is a file
			return createURI(Files.toURI(((File)contextObject)), string); //convert the File to a URI and use it as a context
		} else { //if we don't recognize the context object
			return new URI(string); //create a new URI from the string, ignoring the context object
		}
	}

	/**
	 * Creates an absolute URI from the given string, guessing what the string represents.
	 * <p>
	 * If the string is not a valid URL (e.g. it contains a space), this method assumes that a file was intended and a file URI is constructed.
	 * </p>
	 * <p>
	 * This method is convenient for creating URIs based upon user input.
	 * </p>
	 * @param string The string to convert to a URI.
	 * @return A URI representing the contents of the string, interpreted in a lenient fashion.
	 */
	public static URI guessAbsoluteURI(final String string) {
		//TODO del Log.trace("guessing URI: ", string);
		try {
			final URI uri = new URI(string); //see if the string is already a valid URI
			if(uri.isAbsolute()) { //if the URI is absolute
				return uri; //return the URI
			} else { //if the URI is not absolute
				return Files.toURI(new File(string)); //a local file must have been requested				
			}
		}
		/*TODO del if not needed
				catch(IllegalArgumentException illegalArgumentException) {	//if the string is not an absolute URI
					return Files.toURI(new File(string));	//construct a file object and convert that to a URI
				}
		*/
		catch(URISyntaxException uriSyntaxException) { //if the string is not a valid URI
			return Files.toURI(new File(string)); //construct a file object and convert that to an absolute URI
		}
	}

	/**
	 * Returns a URL representing the directory of the given file URL. (It is assumed that the given URL represents a file.)
	 * @param url The URL of a file.
	 * @return A URL of the file's directory, ending with '/'.
	 * @throws MalformedURLException Thrown if a directory URL cannot be created.
	 */
	public static URL getDirectoryURL(final URL url) throws MalformedURLException {
		return new URL(url, "."); //create a new URL from the directory of the URL TODO use a constant here
	}

	/**
	 * Returns the unencoded host and optional port of the given URI.
	 * @param uri The URI from which to extract the host and optional port.
	 * @return The host name and optional port of the given URI, or <code>null</code> if there is no host specified in the given URI.
	 */
	public static Host getHost(final URI uri) {
		final String host = uri.getHost(); //get the host
		final int port = uri.getPort(); //get the port
		return host != null ? new Host(host, port) : null; //if there is a hostname, return the host information
		/*TODO del		
				if(host!=null) {	//if a host is given
					final int port=uri.getPort();	//get the port
					if(port>=0) {	//if a port is given
						return new StringBuilder(host).append(PORT_SEPARATOR).append(port).toString();	//append the port
					}
					else {	//if no port is given
						return host;	//just return the host
					}
				}
				else {	//if no host was given
					return null;	//show that no host can be returned
				}
		*/

	}

	/**
	 * Returns the unencoded path, optional unencoded query, and optional unencoded fragment of the given URI.
	 * @param uri The URI from which to extract the path, optional query, and optional fragment.
	 * @return An unencoded path in the form <code>[<var>path</var>][?<var>query</var>][#<var>fragment</var>]</code>, or <code>null</code> if none of those
	 *         components are present in the given URI.
	 */
	public static String getRawPathQueryFragment(final URI uri) {
		final StringBuilder stringBuilder = new StringBuilder();
		final String rawPath = uri.getRawPath(); //get the path
		if(rawPath != null) { //if there is a path
			stringBuilder.append(rawPath); //path
		}
		final String rawQuery = uri.getRawQuery(); //get the query
		if(rawQuery != null) { //if there is a query
			stringBuilder.append(QUERY_SEPARATOR).append(rawQuery); //?query
		}
		final String rawFragment = uri.getRawFragment(); //get the fragment
		if(rawFragment != null) { //if there is a fragment
			stringBuilder.append(FRAGMENT_SEPARATOR).append(rawFragment); //#query			
		}
		//if any of the components were present (which is distinct from them having string content), return the constructed string; otherwise, return null
		return rawPath != null || rawFragment != null || rawFragment != null ? stringBuilder.toString() : null;
	}

	/**
	 * Returns a relative path to the URL from the given context URL. This version requires the file to be on the same branch of the context path (e.g.
	 * "http://abc.de/a/c/d.html" is not on the same branch of "http://abc.de/a/b"). TODO del The context URL must be a URL of a directory, ending with the
	 * directory divider character '/'
	 * @param contextURL The reference URL to use in making the relative path.
	 * @param url The URL for which a relative path should be returned, in relation to the context URL.
	 * @return A relative path to the URL in relation to the context URL.
	 * @throws MalformedURLException Thrown if a relative URL cannot be determined from the context URL.
	 */
	/*TODO fix
		public static String getRelativePath(final URL contextURL, final URL url) throws MalformedURLException
		{
	
			  //TODO check this new implementation; this simply chops off everything that matches
	
			if(urlPath.startsWith(directoryURLPath)) {	//if the directory URL path is at the beginning of the URL path
				final String relativePath=urlPath.substring(directoryURLPath.length());  //get everything after the directory URL
				return relativePath;  //return the relative path
			}
			throw new MalformedURLException("Cannot create relative path for "+url+" from context "+contextURL);  //show that we couldn't determine a relative path
		}
	*/

	/**
	 * Opens a connection to the given URL, recognizing redirects. This method was inspired by the source code to <code>javax.swing.JTextPane.getStream()</code>.
	 * @param url The URL a connection to which should be opened.
	 * @return A connection to the given URL or the URL to which it redirects.
	 */
	/*TODO fix; we need to leave the old version in XMLTextPane because it changes the URL appropriately instead of just automatically redirecting
		public static URLConnection openConnection(final URL url) {	//TODO fix throws IOException
			final URLConnection urlConnection=url.openConnection(); //open a connection to the URL
			if(urlConnection instanceof HttpURLConnection) {	//if this is a HTTP connection
			  final HttpURLConnection httpURLConnectoin=(HttpURLConnection)urlConnection; //cast the
		    hconn.setInstanceFollowRedirects(false);
		    int response = hconn.getResponseCode();
		    boolean redirect = (response >= 300 && response <= 399);
	
	//TODO del In the case of a redirect, we want to actually change the URL
	//TODO del that was input to the new, redirected URL
	
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
		if(contentType!=null) {	//if we receive at least a guess of the content type
	*/

	/**
	 * Loads the contents of a URL into an array of bytes.
	 * @param url The URL from which to read.
	 * @return An array of bytes from the URL.
	 * @throws IOException Thrown if there is an error loading the bytes.
	 * @see InputStreamUtilities#getBytes
	 */
	/*TODO fix
		public static byte[] readBytes(final URL url) throws IOException
		{
			try (final InputStream urlInputStream=url.openConnection().getInputStream()) {  //create an input stream to the URL
				return InputStreamUtilities.getBytes(urlInputStream);  //convert the URL to an array of bytes
				urlInputStream.close();  //always close the URL input stream
			}
		}
	*/

	/**
	 * Loads the contents of a URL into a string.
	 * @param url The URL from which to read.
	 * @param encoding The encoding (such as UTF-8) used to store the string.
	 * @return A string containing the contents of the URL.
	 * @throws IOException Thrown if there is an error loading the bytes.
	 */
	/*TODO fix
		public static String readString(final URL url, final String encoding) throws IOException
		{
			final byte[] bytes=readBytes(url); //load the contents of the URL
			return new String(bytes, encoding); //convert the bytes into a string, using the given encoding
		}
	*/

	/**
	 * Determines the relative path of the given absolute path by removing the root path '/' character from the beginning of the path.
	 * @param absolutePath The absolute path to convert to a relative path.
	 * @return A relative path from the root of the absolute path.
	 * @throws IllegalArgumentException if the given path is not absolute.
	 */
	public static String getRelativePath(final String absolutePath) {
		if(!isPathAbsolute(absolutePath)) { //if the path is not really absolute
			throw new IllegalArgumentException("Path is not absolute: " + absolutePath);
		}
		return absolutePath.substring(ROOT_PATH.length()); //remove the beginning root path indicator
	}

	/**
	 * Determines whether the URI represents a canonical collection, that is, it has a path that ends with a slash ('/'). This method returns <code>false</code>
	 * for URIs with no path component.
	 * @param uri The URI the raw path of which to examine.
	 * @return <code>true</code> if the given URI has a path that ends with a slash ('/').
	 * @see #isCollectionPath(String)
	 */
	public static boolean isCollectionURI(final URI uri) {
		final String rawPath = uri.getRawPath(); //get the raw path (use the raw path in case the last character is an encoded slash)
		return rawPath != null ? isCollectionPath(rawPath) : false; //see if the path ends with '/'		
	}

	/**
	 * Checks to see if a given URI represents a canonical collection, that is, it has a path that ends with a slash ('/'). If the given URI is not a collection
	 * URI, an exception is thrown.
	 * @param uri The URI to check to for a collection.
	 * @return The given collection URI.
	 * @throws NullPointerException if the given URI is <code>null</code>.
	 * @throws IllegalArgumentException if the provided URI does not have a path that ends with a slash ('/').
	 * @see #isCollectionURI(URI)
	 */
	public static URI checkCollectionURI(final URI uri) {
		if(!isCollectionURI(uri)) { //if the URI is not a collection URI
			throw new IllegalArgumentException("The given URI " + uri + " is not a collection URI.");
		}
		return uri; //return the collection URI
	}

	/**
	 * Checks to see if a given URI does not represents a canonical collection, that is, it does not have a path that ends with a slash ('/'). If the given URI is
	 * not a collection URI, an exception is thrown.
	 * @param uri The URI to check to for a collection.
	 * @return The given non-collection URI.
	 * @throws NullPointerException if the given URI is <code>null</code>.
	 * @throws IllegalArgumentException if the provided URI has path that ends with a slash ('/').
	 * @see #isCollectionURI(URI)
	 */
	public static URI checkNotCollectionURI(final URI uri) {
		if(isCollectionURI(uri)) { //if the URI is a collection URI
			throw new IllegalArgumentException("The given URI " + uri + " is a collection URI.");
		}
		return uri; //return the collection URI
	}

	/**
	 * Determines whether the URI has a path. This is a convenience method that delegates to {@link URI#getRawPath()}.
	 * @param uri The URI which should be checked for a path.
	 * @return <code>true</code> if the given URI has a path.
	 */
	public static boolean hasPath(@Nonnull final URI uri) {
		return uri.getRawPath() != null; //check the raw path; no need for encoding just to check the presence of the path
	}

	/**
	 * Determines whether the path of the URI (which may or may not be absolute) is absolute.
	 * @param uri The URI the path of which to examine.
	 * @return <code>true</code> if the URI has a path that begins with a slash ('/').
	 * @see #hasPath(URI)
	 * @see #isPathAbsolute(String)
	 */
	public static boolean hasAbsolutePath(@Nonnull final URI uri) {
		final String rawPath = uri.getRawPath(); //use the raw path in case the first character is an encoded slash
		return rawPath != null && isPathAbsolute(rawPath); //see if the path begins with '/'
	}

	/**
	 * Determines whether the path of the URI is a relative path that does not backtrack to a higher level. This method considers a URI referring to the current
	 * level as a "subpath".
	 * @implSpec This method first normalizes the URI using {@link URI#normalize()} and then checks to see if the path is {@value #PARENT_LEVEL_PATH_SEGMENT} or
	 *           begins with a backtrack {@value #PARENT_LEVEL_PATH} path segment.
	 * @param uri The URI the path of which to examine.
	 * @return <code>true</code> if the URI has a relative path that, when normalized, refers to a location at a higher relative level; that is, when normalized
	 *         it begins with a backtrack {@value #PARENT_LEVEL_PATH_SEGMENT} path segment.
	 * @see #hasPath(URI)
	 * @see #PARENT_LEVEL_PATH_SEGMENT
	 */
	public static boolean hasSubPath(@Nonnull final URI uri) {
		final String rawPath = uri.normalize().getRawPath(); //normalize the URI first, to put all backtrack segments at the first
		return rawPath != null && !isPathAbsolute(rawPath) && !rawPath.equals(PARENT_LEVEL_PATH_SEGMENT) && !rawPath.startsWith(PARENT_LEVEL_PATH);
	}

	/**
	 * Determines whether the URI contains only a host and optional port.
	 * @param uri The URI the path of which to examine.
	 * @return <code>true</code> if the URI contains only a host and optionally a port.
	 */
	public static boolean isHost(final URI uri) {
		return uri.getHost() != null //a host URI contains only a host and nothing else except maybe a port
				&& uri.getScheme() == null && uri.getUserInfo() == null && uri.getPath() == null && uri.getQuery() == null && uri.getFragment() == null;
	}

	/**
	 * Creates a URL from a URI. If a valid URL cannot be formed, <code>null</code> is returned.
	 * @param uri The URI to convert to a URL, or <code>null</code> if no URI is available (in which case <code>null</code> will be returned).
	 * @return The URL form of the URI, or <code>null</code> if the URI cannot be converted to a valid URL.
	 */
	public static URL toValidURL(final URI uri) {
		try {
			//TODO we probably want to check for the condition java.lang.IllegalArgumentException: URI is not absolute
			return uri != null ? uri.toURL() : null; //convert the URI to a URL, if we have a URI	
		} catch(MalformedURLException e) { //if there was an error converting to a URL
			return null; //show that we couldn't create a valid URL from the given URI
		}
	}

	/** The prefix used in the scheme-specific part by Java for Windows UNC paths in file URIs. */
	public static final String WINDOWS_UNC_PATH_URI_SSP_PREFIX = ROOT_PATH + PATH_SEPARATOR + PATH_SEPARATOR + PATH_SEPARATOR;

	/**
	 * Determines whether the given URI is a UNC file path URI in the form <code>file:////server/file.ext</code>.
	 * <p>
	 * Strangely, the Java URI form of a UNC path will contain a path prefixed with <code>//</code>, but the entire scheme-specific part will be prefixed with
	 * <code>////</code>.
	 * </p>
	 * @param uri The URI to test.
	 * @return <code>true</code> if the given URI has a scheme of {@value #FILE_SCHEME} and its scheme-specific part begins with four slashes.
	 * @see #WINDOWS_UNC_PATH_URI_SSP_PREFIX
	 */
	public static boolean isUNCFileURI(final URI uri) {
		return FILE_SCHEME.equals(uri.getScheme()) && uri.getRawSchemeSpecificPart().startsWith(WINDOWS_UNC_PATH_URI_SSP_PREFIX);
	}

	/**
	 * Fixes a file URI that the caller knows originated as a Windows UNC path (e.g. <code>file:////server/file.ext</code>) and that has been corrupted by Java
	 * (e.g. <code>file:/server/file.ext</code>) via, for example, {@link URI#normalize()} or {@link URI#resolve(String)}.
	 * @param uri The path to be restored to a Java URI form of the UNC path.
	 * @return The original UNC path file URI as would have been given by {@link File#toURI()}.
	 * @throws IllegalArgumentException if the given path does not have a {@value #FILE_SCHEME} scheme and/or does not have a path that begins with
	 *           {@value #ROOT_PATH}.
	 * @see #isUNCFileURI(URI)
	 */
	private static URI fixUNCPathFileURI(final URI uri) {
		if(!FILE_SCHEME.equals(uri.getScheme())) {
			throw new IllegalArgumentException("Cannot fix UNC path of non-file URI: " + uri);
		}
		final String rawPath = uri.getRawPath(); //get the path of the URI
		if(rawPath == null || !rawPath.startsWith(ROOT_PATH)) { //double-check the path---it should still start with a slash
			throw new IllegalArgumentException("Cannot fix UNC path of a relative path URI: " + uri);
		}
		return changeRawSchemeSpecificPart(uri, ROOT_PATH + PATH_SEPARATOR + PATH_SEPARATOR + rawPath); //prepend "///" to the scheme-specific part
	}

	/**
	 * Normalizes a URI.
	 * @apiNote This method has the same semantics as {@link URI#normalize()}, except that this method has improvements and bug fixes. For example, a UNC path
	 *          such as <code>file:////server/file.ext</code> will retain its correct path, unlike {@link URI#normalize()}, which would reduce this to
	 *          <code>file:/server/file.ext</code>.
	 * @param uri The URI to normalize.
	 * @return The normalized URI.
	 * @see #isUNCFileURI(URI)
	 * @see <a href="https://bugs.java.com/bugdatabase/view_bug.do?bug_id=4723726">Java Bug ID: 4723726</a>
	 */
	public static URI normalize(URI uri) {
		final boolean wasUNCFileURI = isUNCFileURI(uri);
		uri = uri.normalize(); //normalize the URI using Java's normalization
		if(wasUNCFileURI && !isUNCFileURI(uri)) { //if a UNC file URI is no longer a UNC file URI (i.e. it has lost its preceding slashes)
			assert FILE_SCHEME.equals(uri.getScheme());
			assert uri.getRawPath() != null && uri.getRawPath().startsWith(ROOT_PATH);
			uri = fixUNCPathFileURI(uri); //convert the URI back to a UNC path URI
		}
		return uri; //return the normalized, possibly fixed-up URI
	}

	/**
	 * Resolves a string against a base URI with added functionality and bug fixes over {@link URI#resolve(String)}, following
	 * <a href="https://tools.ietf.org/html/rfc3986">RFC 3986</a>.
	 * <p>
	 * This method correctly resolves fragment URIs against opaque base URIs.
	 * </p>
	 * <p>
	 * This method corrects Java's erroneous collapsing of slashes in UNC paths, so that for example <code>file:////server/file.ext</code> can successfully be
	 * resolved against.
	 * </p>
	 * @apiNote This method follows differs from {@link URI#resolve(URI)}, which would resolve the empty path <code>""</code> as if it were <code>.</code>,
	 *          following RFC 2396. That is, <code>http://example.com/foo/bar</code> resolved against <code>""</code> would return
	 *          <code>http://example.com/foo/</code>. This method instead follows <a href="https://tools.ietf.org/html/rfc3986">RFC 3986</a>, as discussed at
	 *          <a href="https://stackoverflow.com/q/22203111/421049">Is Java's URI.resolve incompatible with RFC 3986 when the relative URI contains an empty
	 *          path?</a>, and returns the URI itself, e.g. <code>http://example.com/foo/bar</code>.
	 * @implSpec This method creates a URI from the child URI using {@link URI#create(String)} and then delegates to {@link #resolve(URI, URI)}.
	 * @param baseURI The URI against which the child URI should be resolved.
	 * @param childURI The URI to resolve against the base URI.
	 * @return The child URI resolved against the base URI.
	 * @throws NullPointerException if the base URI and/or the child URI is <code>null</code>.
	 * @see <a href="https://tools.ietf.org/html/rfc3986">RFC 3986: Uniform Resource Identifier (URI): Generic Syntax</a>
	 * @see <a href="http://www.w3.org/TR/rdf-syntax-grammar/#section-baseURIs">RDF/XML Syntax Specification (Revised) 5.3 Resolving URIs</a>
	 * @see #isUNCFileURI(URI)
	 */
	public static URI resolve(final URI baseURI, final String childURI) {
		return resolve(baseURI, URI.create(childURI));
	}

	/**
	 * Resolves a relative URI against a base URI with added functionality and bug fixes over {@link URI#resolve(URI)}, following
	 * <a href="https://tools.ietf.org/html/rfc3986">RFC 3986</a>.
	 * <p>
	 * This method correctly resolves fragment URIs against opaque base URIs.
	 * </p>
	 * <p>
	 * This method corrects Java's erroneous collapsing of slashes in UNC paths in {@link URI#resolve(URI)}, so that for example
	 * <code>file:////server/file.ext</code> can successfully be resolved against.
	 * </p>
	 * @apiNote This method follows differs from {@link URI#resolve(URI)}, which would resolve the empty path <code>""</code> as if it were <code>.</code>,
	 *          following RFC 2396. That is, <code>http://example.com/foo/bar</code> resolved against <code>""</code> would return
	 *          <code>http://example.com/foo/</code>. This method instead follows <a href="https://tools.ietf.org/html/rfc3986">RFC 3986</a>, as discussed at
	 *          <a href="https://stackoverflow.com/q/22203111/421049">Is Java's URI.resolve incompatible with RFC 3986 when the relative URI contains an empty
	 *          path?</a>, and returns the URI itself, e.g. <code>http://example.com/foo/bar</code>.
	 * @param baseURI The URI against which the child URI should be resolved.
	 * @param childURI The URI to resolve against the base URI.
	 * @return The child URI resolved against the base URI.
	 * @throws NullPointerException if the base URI and/or the child URI is <code>null</code>.
	 * @see <a href="https://tools.ietf.org/html/rfc3986">RFC 3986: Uniform Resource Identifier (URI): Generic Syntax</a>
	 * @see <a href="http://www.w3.org/TR/rdf-syntax-grammar/#section-baseURIs">RDF/XML Syntax Specification (Revised) 5.3 Resolving URIs</a>
	 * @see #isUNCFileURI(URI)
	 */
	public static URI resolve(final URI baseURI, final URI childURI) {
		return resolve(baseURI, childURI, false);
	}

	/**
	 * Resolves a relative URI against a base URI with added functionality and bug fixes over {@link URI#resolve(URI)}, following
	 * <a href="https://tools.ietf.org/html/rfc3986">RFC 3986</a>. If a deep resolution is requested, a URI's contents will be further resolved if possible, even
	 * if the URI itself is already absolute. For example, a deep resolution of <code>file:/foo/bar.txt</code> against <code>file:///C:/test</code> would yield
	 * <code>file:///C:/test/foo/bar.txt</code>.
	 * <p>
	 * This implementation supports deep resolution of the following URI schemes:
	 * </p>
	 * <ul>
	 * <li>Files{@value #FILE_SCHEME}</li>
	 * </ul>
	 * <p>
	 * This method correctly resolves fragment URIs against opaque base URIs.
	 * </p>
	 * <p>
	 * This method corrects Java's erroneous collapsing of slashes in UNC paths in {@link URI#resolve(URI)}, so that for example
	 * <code>file:////server/file.ext</code> can successfully be resolved against.
	 * </p>
	 * @apiNote This method follows differs from {@link URI#resolve(URI)}, which would resolve the empty path <code>""</code> as if it were <code>.</code>,
	 *          following RFC 2396. That is, <code>http://example.com/foo/bar</code> resolved against <code>""</code> would return
	 *          <code>http://example.com/foo/</code>. This method instead follows <a href="https://tools.ietf.org/html/rfc3986">RFC 3986</a>, as discussed at
	 *          <a href="https://stackoverflow.com/q/22203111/421049">Is Java's URI.resolve incompatible with RFC 3986 when the relative URI contains an empty
	 *          path?</a>, and returns the URI itself, e.g. <code>http://example.com/foo/bar</code>.
	 * @param baseURI The URI against which the child URI should be resolved.
	 * @param childURI The URI to resolve against the base URI.
	 * @param deep Whether the relative contents of the URI should also be resolved, even if the URI itself is absolute.
	 * @return The child URI resolved against the base URI.
	 * @throws NullPointerException if the base URI and/or the child URI is <code>null</code>.
	 * @see <a href="https://tools.ietf.org/html/rfc3986">RFC 3986: Uniform Resource Identifier (URI): Generic Syntax</a>
	 * @see <a href="http://www.w3.org/TR/rdf-syntax-grammar/#section-baseURIs">RDF/XML Syntax Specification (Revised) 5.3 Resolving URIs</a>
	 * @see #isUNCFileURI(URI)
	 */
	public static URI resolve(final URI baseURI, final URI childURI, final boolean deep) {
		if(baseURI.isOpaque()) { //if the base URI is opaque, do special processing
			final String childURIString = childURI.toString(); //get the child URI as a string
			if(startsWith(childURIString, FRAGMENT_SEPARATOR)) { //if the child URI is a fragment
				return URI.create(removeFragment(baseURI).toString() + childURIString); //remove the fragment, if any, from the base URI, and append the fragment
			}
		}
		if(isPathURI(childURI)) { //if the given URI is only a path (with no fragment)
			final String rawPath = childURI.getRawPath(); //get the raw path of the URI
			if(rawPath.length() == 0) { //if this URI is ""
				return removeFragment(baseURI); //return the base URI with no fragment
			}
		}
		URI resolvedURI = baseURI.resolve(childURI); //resolve the child URI against the base normally
		if(isUNCFileURI(baseURI) && !childURI.isAbsolute()) { //if we resolved a relative URI against a Windows UNC path file URI, the resulting URI should also be a UNC path file URI
			resolvedURI = fixUNCPathFileURI(resolvedURI); //restore the URI to a UNC path file URI, as Java will have collapsed several slashes
		}
		if(deep) { //if we are doing a deep resolve
			if(FILE_SCHEME.equals(resolvedURI.getScheme()) && FILE_SCHEME.equals(baseURI.getScheme())) { //if both URIs are file URIs
				final String resolvedFilePath = resolvedURI.getSchemeSpecificPart(); //determine the file's path from its SSP; if the file is relative, the URI path will be null
				File resolvedFile = new File(resolvedFilePath);
				if(!resolvedFile.isAbsolute()) { //if the resolved file isn't absolute
					final File baseFile = new File(getCurrentLevel(baseURI).getSchemeSpecificPart()); //get the directory (URI current level) of the base file
					resolvedFile = new File(baseFile, resolvedFilePath); //resolve the file against the base file
					try {
						resolvedFile = resolvedFile.getCanonicalFile(); //try to get the canonical form of the file 
					} catch(final IOException ioException) { //if we had a problem getting the canonical form
						//TODO fix log: Log.warn("Error getting canonical form of file: " + resolvedFile, ioException);
					}
					resolvedURI = Files.toURI(resolvedFile); //update the resolved URI to the new resolved file 
				}
			}
		}
		return resolvedURI;
	}

	/**
	 * Resolves a URI path against a base URI.
	 * @param baseURI The base URI against which the path should be resolved.
	 * @param path The path to resolve against the base URI.
	 * @return A URI that represents the path resolved against the base URI.
	 * @throws NullPointerException if the given base URI and/or path is <code>null</code>.
	 */
	public static URI resolve(final URI baseURI, final URIPath path) {
		return resolve(baseURI, path.toURI()); //resolve the path as a URI against the base URI
	}

	/**
	 * Returns a URI constructed from a given URI and a fragment identifier. The fragment will first be URI-encoded.
	 * <p>
	 * If no URI is provided, a URI is created from the fragment itself.
	 * </p>
	 * @param uri The URI to which to add a fragment identifier, or <code>null</code> if a URI should be created from just the fragment.
	 * @param fragment The unencoded fragment to add to the end of the URI.
	 * @return The URI constructed.
	 * @throws IllegalArgumentException if the a URI cannot be constructed from the given information.
	 * @see #encode(String)
	 * @see URI#create(String)
	 */
	public static URI resolveFragment(final URI uri, final String fragment) throws IllegalArgumentException {
		return resolveRawFragment(uri, encode(fragment)); //encode and resolve the fragment
	}

	/**
	 * Returns a URI constructed from a given URI and a raw fragment identifier.
	 * <p>
	 * If the URI is not syntactically correct, an <code>IllegalArgumentException</code>will be thrown.
	 * <p>
	 * This method should normally only be used when the format of the string is known to be a syntactically correct URI.
	 * </p>
	 * <p>
	 * If no URI is provided, a URI is created from the fragment itself.
	 * </p>
	 * @param uri The URI to which to add a fragment identifier, or <code>null</code> if a URI should be created from just the fragment.
	 * @param rawFragment The raw, encoded fragment to add to the end of the URI.
	 * @return The URI constructed.
	 * @throws IllegalArgumentException if the a URI cannot be constructed from the given information.
	 * @see URI#create(String)
	 */
	public static URI resolveRawFragment(final URI uri, final String rawFragment) throws IllegalArgumentException {
		final String fragmentSuffix = new StringBuilder().append(FRAGMENT_SEPARATOR).append(rawFragment).toString(); //create a suffix that includes the fragment separator and the fragment
		final URI fragmentURI = URI.create(fragmentSuffix); //create a URI from the fragment
		return uri != null ? resolve(uri, fragmentURI) : fragmentURI; //if a URI was given, resolve the fragment against the URI; otherwise, just return the fragment suffix itself 
	}

	/**
	 * Returns a URI with its fragment, if any, removed.
	 * @param uri The URI from which a fragment should be removed.
	 * @return The URI with the fragment, if any, removed.
	 * @throws NullPointerException if the given URI is <code>null</code>.
	 * @see #replaceRawFragment(URI, String)
	 */
	public static URI removeFragment(final URI uri) {
		return replaceRawFragment(uri, null); //replace the raw fragment, if any, with nothing
	}

	/**
	 * Returns a URI with its fragment, if any, replaced.
	 * @param uri The URI from which a fragment should be removed.
	 * @param newRawFragment The new encoded fragment, or <code>null</code> if the URI should have no fragment.
	 * @return The URI with the fragment, if any, removed and replaced with the given raw fragment, if any.
	 * @throws NullPointerException if the given URI is <code>null</code>.
	 */
	public static URI replaceRawFragment(final URI uri, final String newRawFragment) {
		final String oldRawFragment = uri.getRawFragment(); //get the raw fragment, if any
		if(oldRawFragment != null) { //if there is currently a fragment
			final int oldRawFragmentLength = oldRawFragment.length(); //get the length of the current raw fragment
			final StringBuilder uriStringBuilder = new StringBuilder(uri.toString()); //get the string representation of the URI
			final int uriLength = uriStringBuilder.length(); //get the length of the URI
			assert uriStringBuilder.toString().endsWith(new StringBuilder().append(FRAGMENT_SEPARATOR).append(oldRawFragment).toString());
			if(newRawFragment != null) { //if a new raw fragment was given
				uriStringBuilder.replace(uriLength - oldRawFragmentLength, uriLength, newRawFragment); //replace the old fragment with the new one
			} else { //if no new raw fragment was given
				uriStringBuilder.delete(uriLength - oldRawFragmentLength - 1, uriLength); //delete the entire fragment
			}
			return URI.create(uriStringBuilder.toString()); //create a URI from the new URI string
		} else { //if there is no fragment
			if(newRawFragment != null) { //if a new raw fragment was given
				return URI.create(uri.toString() + FRAGMENT_SEPARATOR + newRawFragment); //append the new raw fragment
			} else { //if no new raw fragment was given
				return requireNonNull(uri, "URI cannot be null."); //return the original URI
			}
		}
	}

	/**
	 * Returns a URI constructed from the given parts, any of which can be <code>null</code>.
	 * <p>
	 * This method should normally only be used when the format of the string is known to be a syntactically correct URI.
	 * </p>
	 * @param scheme The name of the URI scheme.
	 * @param rawSchemeSpecificPart The raw, encoded scheme-specific part, or <code>null</code> if there is no scheme-specific part.
	 * @return The URI constructed.
	 * @throws IllegalArgumentException if the a URI cannot be constructed from the given strings.
	 */
	public static URI createURI(final String scheme, final String rawSchemeSpecificPart) throws IllegalArgumentException {
		return createURI(scheme, rawSchemeSpecificPart, null); //create a URI with no fragment
	}

	/**
	 * Returns a URI constructed from the given parts, any of which can be <code>null</code>.
	 * <p>
	 * This method should normally only be used when the format of the string is known to be a syntactically correct URI.
	 * </p>
	 * @param scheme The name of the URI scheme.
	 * @param rawSchemeSpecificPart The raw, encoded scheme-specific part, or <code>null</code> if there is no scheme-specific part.
	 * @param rawFragment The raw, encoded fragment at the end of the URI, or <code>null</code> if there is no fragment.
	 * @return The URI constructed.
	 * @throws IllegalArgumentException if the a URI cannot be constructed from the given strings.
	 */
	public static URI createURI(final String scheme, final String rawSchemeSpecificPart, final String rawFragment) throws IllegalArgumentException {
		final StringBuilder stringBuilder = new StringBuilder(); //we'll use this to construct the URI
		if(scheme != null) { //if there is a scheme
			stringBuilder.append(scheme).append(SCHEME_SEPARATOR); //append the scheme and its separator
		}
		if(rawSchemeSpecificPart != null) { //if there is a scheme-specific part
			stringBuilder.append(rawSchemeSpecificPart); //append the scheme-specific part
		}
		if(rawFragment != null) { //if there is a fragment
			stringBuilder.append(FRAGMENT_SEPARATOR).append(rawFragment); //append the fragment
		}
		return URI.create(stringBuilder.toString()); //create and return a new URI
	}

	/**
	 * Returns a URI constructed from the given parts, any of which can be <code>null</code>.
	 * <p>
	 * This method should normally only be used when the format of the string is known to be a syntactically correct URI.
	 * </p>
	 * @param scheme The name of the URI scheme.
	 * @param rawUserInfo The raw, encoded user information, or <code>null</code> if there is no user information.
	 * @param host The host information, or <code>null</code> if there is no host.
	 * @param port The port number, or -1 for no defined port.
	 * @param path The path, or <code>null</code> if there is no path.
	 * @param rawQuery The raw, encoded URI query, or <code>null</code> if there is no query.
	 * @param rawFragment The raw, encoded fragment at the end of the URI, or <code>null</code> if there is no fragment.
	 * @return The URI constructed.
	 * @throws IllegalArgumentException if the a URI cannot be constructed from the given strings.
	 */
	public static URI createURI(final String scheme, final String rawUserInfo, final String host, final int port, final URIPath path, final String rawQuery,
			final String rawFragment) throws IllegalArgumentException {
		return createURI(scheme, rawUserInfo, host, port, path != null ? path.toString() : null, rawQuery, rawFragment);
	}

	/**
	 * Returns a URI constructed from the given parts, any of which can be <code>null</code>.
	 * <p>
	 * This method should normally only be used when the format of the string is known to be a syntactically correct URI.
	 * </p>
	 * @param scheme The name of the URI scheme.
	 * @param rawUserInfo The raw, encoded user information, or <code>null</code> if there is no user information.
	 * @param host The host information, or <code>null</code> if there is no host.
	 * @param port The port number, or -1 for no defined port.
	 * @param rawPath The raw, encoded path, or <code>null</code> if there is no path.
	 * @param rawQuery The raw, encoded URI query, or <code>null</code> if there is no query.
	 * @param rawFragment The raw, encoded fragment at the end of the URI, or <code>null</code> if there is no fragment.
	 * @return The URI constructed.
	 * @throws IllegalArgumentException if the a URI cannot be constructed from the given strings.
	 */
	public static URI createURI(final String scheme, final String rawUserInfo, final String host, final int port, final String rawPath, final String rawQuery,
			final String rawFragment) throws IllegalArgumentException {
		final StringBuilder stringBuilder = new StringBuilder(); //we'll use this to construct the URI
		if(scheme != null) { //if there is a scheme
			stringBuilder.append(scheme).append(SCHEME_SEPARATOR); //append the scheme and its separator
		}
		if(host != null) { //if there is authority information
			stringBuilder.append(AUTHORITY_PREFIX); //append the authority prefix
			if(rawUserInfo != null) { //if there is user information
				stringBuilder.append(rawUserInfo).append(USER_INFO_SEPARATOR); //append the user information
			}
			stringBuilder.append(host); //append the host
			if(port >= 0) { //if there is a port
				stringBuilder.append(PORT_SEPARATOR).append(port); //append the port
			}
		} else { //if there is no host
			if(rawUserInfo != null) { //if user information was given
				throw new IllegalArgumentException("URI cannot have user info without a host.");
			}
			if(port >= 0) { //if a port was given
				throw new IllegalArgumentException("URI cannot have a port without a host.");
			}
		}
		if(rawPath != null) { //if there is a path
			stringBuilder.append(rawPath); //append the path
		}
		if(rawQuery != null) { //if there is a query
			stringBuilder.append(QUERY_SEPARATOR).append(rawQuery); //append the query
		}
		if(rawFragment != null) { //if there is a fragment
			stringBuilder.append(FRAGMENT_SEPARATOR).append(rawFragment); //append the fragment
		}
		return URI.create(stringBuilder.toString()); //create and return a new URI
	}

	/**
	 * Encodes all non-ASCII and URI reserved characters in the URI according to the URI encoding rules in <a href="https://tools.ietf.org/html/rfc3986">RFC
	 * 3986</a>, "Uniform Resource Identifiers (URI): Generic Syntax" using the URI escape character, {@value URIs#ESCAPE_CHAR}.
	 * @param uri The URI to URI-encode.
	 * @return A string containing the escaped data.
	 * @see URIs#ESCAPE_CHAR
	 */
	public static String encode(final URI uri) {
		return encode(uri.toASCIIString()); //encode the string version of the URI
	}

	/**
	 * Encodes all non-ASCII and URI reserved characters in the URI according to the URI encoding rules in <a href="https://tools.ietf.org/html/rfc3986">RFC
	 * 3986</a>, "Uniform Resource Identifiers (URI): Generic Syntax".
	 * @param uri The URI to URI-encode.
	 * @param escapeChar The escape character to use, which will always be escaped.
	 * @return A string containing the escaped data.
	 */
	public static String encode(final URI uri, final char escapeChar) {
		return encode(uri.toASCIIString(), escapeChar); //encode all string version of the URI
	}

	/**
	 * Encodes all URI reserved characters in the string according to the URI encoding rules in <a href="https://tools.ietf.org/html/rfc3986">RFC 3986</a>,
	 * "Uniform Resource Identifiers (URI): Generic Syntax" using the URI escape character, {@value URIs#ESCAPE_CHAR}.
	 * @param string The data to URI-encode.
	 * @return A string containing the escaped data.
	 * @see URIs#ESCAPE_CHAR
	 */
	public static String encode(final String string) { //TODO distinguish between full URI and URI path segment encoding
		return encode(string, ESCAPE_CHAR); //encode the URI using the standard escape character
	}

	/**
	 * Encodes all URI reserved characters in the string according to the URI encoding rules in <a href="https://tools.ietf.org/html/rfc3986">RFC 3986</a>,
	 * "Uniform Resource Identifiers (URI): Generic Syntax".
	 * @param string The data to URI-encode.
	 * @param escapeChar The escape character to use, which will always be escaped.
	 * @return A string containing the escaped data.
	 */
	public static String encode(final String string, final char escapeChar) {
		return encode(string, UNRESERVED_CHARACTERS, escapeChar); //encode all non-unreserved characters
	}

	/**
	 * Encodes the URI reserved characters in the string according to the URI encoding rules in <a href="https://tools.ietf.org/html/rfc3986">RFC 3986</a>,
	 * "Uniform Resource Identifiers (URI): Generic Syntax" using the URI escape character, {@value URIs#ESCAPE_CHAR}.
	 * @param string The data to URI-encode.
	 * @param validCharacters Characters that should not be encoded; all other characters will be encoded.
	 * @return A string containing the escaped data.
	 * @see URIs#ESCAPE_CHAR
	 */
	static String encode(final String string, final Characters validCharacters) {
		return encode(string, validCharacters, ESCAPE_CHAR); //encode the string with the normal escape character
	}

	/**
	 * Encodes the URI reserved characters in the string according to the URI encoding rules in <a href="https://tools.ietf.org/html/rfc3986">RFC 3986</a>,
	 * "Uniform Resource Identifiers (URI): Generic Syntax". Following the examples in RFC 3986, this is guaranteed to produce only <em>uppercase</em> hexadecimal
	 * escape codes.
	 * @param string The data to URI-encode.
	 * @param validCharacters Characters that should not be encoded; all other characters will be encoded.
	 * @param escapeChar The escape character to use, which will always be escaped.
	 * @return A string containing the escaped data.
	 */
	static String encode(final String string, final Characters validCharacters, final char escapeChar) {
		return escapeHex(string, validCharacters, null, Integer.MAX_VALUE, escapeChar, 2, Case.UPPERCASE); //escape the string using two escape hex digits; don't use an upper bound, as the valid characters take inherently care of this
	}

	/**
	 * Decodes the escaped characters in the character iterator according to the URI encoding rules in <a href="http://www.ietf.org/rfc/rfc2396.txt">RFC 2396</a>,
	 * "Uniform Resource Identifiers (URI): Generic Syntax", using the URI escape character, {@value URIs#ESCAPE_CHAR}.
	 * @param uri The data to URI-decode.
	 * @return A character sequence containing the encoded URI data.
	 * @throws IllegalArgumentException if the given URI string contains a character greater than U+00FF.
	 * @throws IllegalArgumentException if a given escape character is not followed by a two-digit escape sequence.
	 * @see URIs#ESCAPE_CHAR
	 */
	public static String decode(final CharSequence uri) {
		return decode(uri, ESCAPE_CHAR); //decode the string using the standard URI escape character
	}

	/**
	 * Decodes the escaped ('%') characters in the character iterator according to the URI encoding rules in <a href="http://www.ietf.org/rfc/rfc2396.txt">RFC
	 * 2396</a>, "Uniform Resource Identifiers (URI): Generic Syntax".
	 * @param uri The data to URI-decode.
	 * @param escapeChar The escape character.
	 * @return A character sequence containing the encoded URI data.
	 * @throws IllegalArgumentException if the given URI string contains a character greater than U+00FF.
	 * @throws IllegalArgumentException if a given escape character is not followed by a two-digit escape sequence.
	 */
	public static String decode(final CharSequence uri, final char escapeChar) {
		return unescapeHex(uri, escapeChar, 2).toString(); //unescape the string using two escape hex digits
	}

	/**
	 * Ensures that the given URI is in canonical form.
	 * <p>
	 * This implementation, following the recommendation of <a href="https://tools.ietf.org/html/rfc3986">RFC 3986</a>, ensures that all hexadecimal escape codes
	 * are in uppercase.
	 * </p>
	 * @apiNote This method should be distinguished from {@link #normalize(URI)}, which normalizes the hierarchy of path segments.
	 * @implNote This implementation does not currently encode any non-ASCII or reserved characters.
	 * @param uri The URI to be returned in canonical form.
	 * @return The canonical form of the given URI.
	 * @throws NullPointerException if the given URI is <code>null</code>.
	 * @throws IllegalArgumentException if the given URI has an invalid escape sequence.
	 */
	public static URI canonicalize(final URI uri) {
		final String uriString = uri.toString(); //get the string version of the URI
		final int uriStringLength = uriString.length(); //get the length of the string
		StringBuilder uriStringBuilder = null; //we'll only create a string builder if we need one
		for(int i = 0; i < uriStringLength; ++i) { //for each character, make sure that the escape sequences are in uppercase
			if(uriString.charAt(i) == ESCAPE_CHAR) { //if this is an escape sequence
				if(i >= uriStringLength - 2) { //if there isn't room for an escape sequence
					throw new IllegalArgumentException("Invalid escape sequence in URI " + uriString + " at index " + i + ".");
				}
				final char hex1 = uriString.charAt(i + 1);
				final char hex2 = uriString.charAt(i + 2);
				if(ASCII.isLowerCase(hex1) || ASCII.isLowerCase(hex2)) { //if the hex code is not in uppercase
					if(uriStringBuilder == null) { //if we haven't yet created a string builder
						uriStringBuilder = new StringBuilder(uriString); //create a new string builder for manipulating the URI
					}
					uriStringBuilder.setCharAt(i + 1, ASCII.toUpperCase(hex1)); //convert any hex characters to uppercase
					uriStringBuilder.setCharAt(i + 2, ASCII.toUpperCase(hex2));
				}
				i += 2; //skip the escape sequence
			}
		}
		return uriStringBuilder != null ? URI.create(uriStringBuilder.toString()) : uri; //if we modified the URI, return a new URI created from the string builder
	}

	/**
	 * Changes a URI from one base to another. For example, <code>http://example.com/base1/test.txt</code> changed from base
	 * <code>http://example.com/base1/</code> to base <code>http://example.com/base2/level2/</code> yields <code>http://example.com/base2/level2/test.txt</code>.
	 * If the old and new base URIs are the same, a URI equal to given URI is returned.
	 * @param uri The URI the base of which to change.
	 * @param oldBaseURI The current base URI.
	 * @param newBaseURI The base URI of the new URI to construct.
	 * @return A new URI constructed by relativizing the URI to the old base URI and resolving the resulting URI against the new base URI.
	 * @see #isUNCFileURI(URI)
	 * @see URI#relativize(URI)
	 * @see #resolve(URI, URI)
	 * @throws IllegalArgumentException if the old base URI is is not a base URI of the given URI.
	 */
	public static URI changeBase(final URI uri, final URI oldBaseURI, final URI newBaseURI) {
		//The documentation at one point said: "This method correctly works with Windows UNC path file URIs,
		// working around a JDK 5.x/6.x bug that chops off the first few forward slashes for Windows network names."
		//TODO see if the JDK Windows UNC path bug still exists, and whether a workaround needs to be added back.
		if(oldBaseURI.equals(newBaseURI)) { //if the old and new base URIs are the same
			return uri; //the URI will not change
		}
		final URI relativeURI = oldBaseURI.relativize(uri); //get a URI relative to the old base URI
		if(relativeURI.isAbsolute()) { //if we couldn't relativize the the URI to the old base URI and come up with a relative URI
			throw new IllegalArgumentException(oldBaseURI.toString() + " is not a base URI of " + uri);
		}
		return resolve(newBaseURI, relativeURI); //resolve the relative URI to the new base URI, using our Windows UNC path-aware resolve method
	}

	/**
	 * Determines whether the given URI is a child relative to the given base URI. The base URI is considered a child of itself. This for the base URI
	 * <code>http://www.example.com/base/</code>, the URIs <code>http://www.example.com/base/</code> and <code>http://www.example.com/base/child/</code> are
	 * considered child URIs, while <code>http://www.example.com/</code> and <code>http://www.example.com/other/</code> are not.
	 * @implNote This implementation relies on the behavior of {@link URI#relativize(URI)} not to relativize URIs that require backtracking, such as siblings, as
	 *           described in <a href="https://bugs.java.com/bugdatabase/view_bug.do?bug_id=6226081">JDK-6226081</a>.
	 * @param baseURI The assumed base URI.
	 * @param uri The URI which may be relative to the given base URI.
	 * @return <code>true</code> if the given URI can be made relative to the given base URI resulting in a non-absolute form.
	 * @throws NullPointerException if the given base URI and/or URI is <code>null</code>.
	 */
	public static boolean isChild(final URI baseURI, final URI uri) {
		final URI relativeURI = baseURI.relativize(uri); //get a URI relative to the base URI
		return !relativeURI.isAbsolute(); //if the relativized URI is not absolute, the URI is relative to the base
	}

	/** Characters that can appear in a URI path with no escape sequences. */
	//TODO del	protected static final String COMPRESS_CHARS=ALPHA_CHARS+DIGIT_CHARS;	//length 49
	/**
	 * Compresses a URI into a shorter string representation. The resulting string consists only of URI <code>xalpha</code> characters with no escape sequences.
	 * @param uri The URI to compress.
	 * @return A compressed string representation of the URI.
	 */
	/*TODO fix
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
				while(accumulator>COMPRESS_BASE) {	//while the value is more than our conversion base
					final index
					
				}
			}
		}
	*/

	/** URI alphabetic and digit characters. */
	private static final Characters COMPRESS_NORMAL_CHARS = ALPHA_CHARACTERS.add(DIGIT_CHARACTERS); //"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" count 62

	/** Characters that will be compressed. */
	private static final String OTHER_CHARS = SAFE_CHARACTERS.add(EXTRA_CHARACTERS).add(ESCAPE_CHAR).add(RESERVED_CHARACTERS).toString(); //"$-_@.&!*\"'(),%=;/#?: " count 21

	/** Characters that can appear in a URI path with no escape sequences. */
	private static final String COMPRESS_ENCODE_CHARS = "-_()@"; //count 5

	/**
	 * Compresses a URI into a shorter string representation.
	 * @param uri The URI to compress.
	 * @return A compressed string representation of the URI.
	 */
	public static String compress(final URI uri) {
		final int ENCODE_BASE = COMPRESS_ENCODE_CHARS.length(); //this is the base into which we'll encode certain characters
		final String uriString = uri.toString();
		final StringBuilder stringBuilder = new StringBuilder();
		for(int i = 0; i < uriString.length(); ++i) { //look at each URI character
			final char character = uriString.charAt(i); //get the next character
			if(COMPRESS_NORMAL_CHARS.contains(character)) { //if this is a normal character
				stringBuilder.append(character); //add the character normally
			} else { //if this is a character to be encoded
				final int index = OTHER_CHARS.indexOf(character); //get the character's index within our set of special characters
				assert index >= 0 : "Invalid URI character: " + character; //if the character came in the URI object, it should always be valid
				final int high = index / ENCODE_BASE; //get the high bits of our encoding
				final int low = index % ENCODE_BASE; //get the high bits of our encoding
				stringBuilder.append(COMPRESS_ENCODE_CHARS.charAt(high)); //add a character to represent our high bits
				stringBuilder.append(COMPRESS_ENCODE_CHARS.charAt(low)); //add a character to represent our low bits
			}
		}
		return stringBuilder.toString(); //return our encoded URI string
	}

	/**
	 * Decompresses a URI from a shorter string representation.
	 * @param string The alphanumeric string.
	 * @return An uncompressed URI from the alphanumeric string.
	 * @throws SyntaxException Thrown if the given string is not correctly encoded.
	 */
	public static URI decompress(final String string) throws SyntaxException {
		final int ENCODE_BASE = COMPRESS_ENCODE_CHARS.length(); //this is the base into which we'll encode certain characters TODO maybe place this outside the method
		final StringBuilder stringBuilder = new StringBuilder();
		for(int i = 0; i < string.length(); ++i) { //look at each character
			final char character = string.charAt(i); //get the next character
			if(COMPRESS_NORMAL_CHARS.contains(character)) { //if this is a normal character
				stringBuilder.append(character); //add the character normally
			} else { //if this is a character to be encoded
				final int high = COMPRESS_ENCODE_CHARS.indexOf(character); //get the high bits
				if(high < 0) { //if the high character wasn't recognized
					throw new SyntaxException("Invalid character.", string); //indicate that an unexpected character was encountered					
				}
				if(i == string.length() - 1) { //if there are no more characters
					throw new SyntaxException("Incomplete encoding sequence.", string); //indicate that the encoding character was not present.
				}
				final int low = COMPRESS_ENCODE_CHARS.indexOf(string.charAt(++i)); //go to the next character and get its index
				if(low < 0) { //if the low character wasn't recognized
					throw new SyntaxException("Invalid character.", string); //indicate that an unexpected character was encountered					
				}
				final int index = high * ENCODE_BASE + low; //get the index of the original character
				if(index >= OTHER_CHARS.length()) { //if the resulting sequence does not match one of our original characters
					throw new SyntaxException("Invalid encoding sequence.", string); //indicate that the encoding resulted in an invalid sequence					
				}
				stringBuilder.append(OTHER_CHARS.charAt(index)); //add the encoded character to our string builder
			}
		}
		return URI.create(stringBuilder.toString()); //return the original URI
	}

	/**
	 * The character used for escaping characters in a URI plain encoding.
	 * @see #plainEncode(URI)
	 */
	public static final char PLAIN_ENCODING_ESCAPE_CHAR = '_';

	/**
	 * The character used for replacing certain characters in a URI plain encoding.
	 * @see #plainEncode(URI)
	 */
	public static final char PLAIN_ENCODING_REPLACE_CHAR = '-';

	/** The characters that, at least initially, should not be encoded. Path separators will be replaced with {@value #PLAIN_ENCODING_REPLACE_CHAR}. */
	protected static final Characters PLAIN_ENCODE_INITIAL_UNRESERVED_CHARACTERS = ALPHA_CHARACTERS.add(DIGIT_CHARACTERS).add('.', PATH_SEPARATOR);

	/** The characters that are allowed in a URI plain encoding. */
	public static final Characters PLAIN_ENCODE_CHARACTERS = PLAIN_ENCODE_INITIAL_UNRESERVED_CHARACTERS.remove(PATH_SEPARATOR).add(PLAIN_ENCODING_ESCAPE_CHAR,
			PLAIN_ENCODING_REPLACE_CHAR);

	/**
	 * Encodes an absolute URI into a plain string that is safe to be used in the path of another URI, for example. The resulting string will only contain
	 * {@link #PLAIN_ENCODE_CHARACTERS}, making it also be a valid XML name.
	 * 
	 * <ol>
	 * <li>The scheme separator character {@value #SCHEME_SEPARATOR} is replaced with {@value #PLAIN_ENCODING_REPLACE_CHAR}.</li>
	 * <li>Every instance of the slash character {@value #PATH_SEPARATOR} is replaced with {@value #PLAIN_ENCODING_REPLACE_CHAR}.</li>
	 * <li>The characters {@value #PLAIN_ENCODING_REPLACE_CHAR}, {@value #PLAIN_ENCODING_ESCAPE_CHAR}, and all other non-URI characters are escaped using
	 * {@value #PLAIN_ENCODING_ESCAPE_CHAR} as the escape character.</li>
	 * </ol>
	 * <p>
	 * This implementation ensures that all hexadecimal escape codes are in lowercase.
	 * </p>
	 * @implNote The implementation encodes all non-ASCII characters.
	 * @param uri The URI to encode
	 * @return A string representing the plain encoding of the URI.
	 * @throws IllegalArgumentException if the given URI is not absolute.
	 * @see #PLAIN_ENCODE_CHARACTERS
	 */
	public static String plainEncode(final URI uri) {
		checkAbsolute(uri);
		final String scheme = uri.getScheme(); //find out the scheme of the URI
		final int schemeLength = scheme.length();
		final String uriString = uri.toASCIIString(); //start with the real string form of the encoded URI
		final StringBuilder stringBuilder = new StringBuilder(); //do the processing within a string builder
		stringBuilder.append(scheme); //encode the scheme first
		escapeHex(stringBuilder, PLAIN_ENCODE_INITIAL_UNRESERVED_CHARACTERS, null, Integer.MAX_VALUE, PLAIN_ENCODING_ESCAPE_CHAR, 2, Case.LOWERCASE); //escape the scheme
		stringBuilder.append(PLAIN_ENCODING_REPLACE_CHAR); //append a '-' in place of ':'
		final int encodedSchemeLength = stringBuilder.length();
		stringBuilder.append(uriString, schemeLength + 1, uriString.length()); //append the rest of the URI
		//TODO normalize existing hex escape values to lowercase
		escapeHex(stringBuilder, encodedSchemeLength + 1, PLAIN_ENCODE_INITIAL_UNRESERVED_CHARACTERS, null, Integer.MAX_VALUE, PLAIN_ENCODING_ESCAPE_CHAR, 2,
				Case.LOWERCASE); //escape the rest of the URI (after the encoded scheme)
		replace(stringBuilder, PATH_SEPARATOR, PLAIN_ENCODING_REPLACE_CHAR); //replace the path characters with '-'
		return stringBuilder.toString(); //at this point, the remaining bare '-' characters will represent '/', except for the first one, which represents ':'
	}

	/**
	 * Decodes a plain-encoded URI.
	 * @param charSequence The string containing the plain-encoded URI.
	 * @return The decoded URI.
	 * @throws IllegalArgumentException if the given string is not a plain-encoded URI.
	 */
	public static URI plainDecode(final CharSequence charSequence) {
		final StringBuilder stringBuilder = new StringBuilder(charSequence); //start processing in a string builder
		final int replaceIndex = indexOf(stringBuilder, PLAIN_ENCODING_REPLACE_CHAR); //get the first '-'
		if(replaceIndex < 0) { //if there is no '-', there can't be a ':', meaning the original URI was not absolute
			throw new IllegalArgumentException("Character sequence " + charSequence + " does not represent a valid plain-encoded absolute URI.");
		}
		stringBuilder.setCharAt(replaceIndex, SCHEME_SEPARATOR); //put ':' back where it belongs
		replace(stringBuilder, PLAIN_ENCODING_REPLACE_CHAR, PATH_SEPARATOR); //the rest of the replacement characters represent path separators
		final String decodedString = unescapeHex(stringBuilder, PLAIN_ENCODING_ESCAPE_CHAR, 2).toString(); //unescape the string using two escape hex digits
		return URI.create(decodedString); //create and return a URI from the resulting string
	}

	//URI paths

	/**
	 * Checks to see if a given path represents a canonical collection, that is, it has a path that ends with a slash ('/'). If the given path is not a collection
	 * path, an exception is thrown.
	 * @param path The raw path to examine.
	 * @return The given collection path.
	 * @throws NullPointerException if the given path is <code>null</code>.
	 * @throws IllegalArgumentException if the provided path does not end with a slash ('/').
	 * @see #isCollectionPath(String)
	 */
	public static String checkCollectionPath(@Nonnull final String path) {
		if(!isCollectionPath(path)) { //if the path is not a collection path
			throw new IllegalArgumentException("The given path " + path + " is not a collection path.");
		}
		return path; //return the collection path
	}

	/**
	 * Checks to see if a given path does not represents a canonical collection, that is, it does not have a path that ends with a slash ('/'). If the given path
	 * is not a collection path, an exception is thrown.
	 * @param path The raw path to examine.
	 * @return The given non-collection path.
	 * @throws NullPointerException if the given path is <code>null</code>.
	 * @throws IllegalArgumentException if the provided path ends with a slash ('/').
	 * @see #isCollectionPath(String)
	 */
	public static String checkNotCollectionPath(final String path) {
		if(isCollectionPath(path)) { //if the path is a collection path
			throw new IllegalArgumentException("The given path " + path + " is a collection path.");
		}
		return path; //return the collection path
	}

	/**
	 * Checks to see if a given path is only a path and not a URI with a scheme and/or authority. If the given string is not a path, an exception is thrown.
	 * @param path The string version of a path to determine if it is indeed only a path.
	 * @return The given path.
	 * @throws NullPointerException if the given path is <code>null</code>.
	 * @throws IllegalArgumentException if the given string is not a path.
	 * @see #isPath(String)
	 */
	public static String checkPath(final String path) throws IllegalArgumentException {
		if(!isPath(path)) { //if the string is not a path
			throw new IllegalArgumentException("The given string " + path + " is not a valid sole path.");
		}
		return path; //return the path
	}

	/**
	 * Checks to see if a given path is only a relative path and not a URI with a scheme and/or authority. If the given string is not a relative path, an
	 * exception is thrown.
	 * @param path The string version of a path to determine if it is indeed only a relative path.
	 * @return The given relative path.
	 * @throws NullPointerException if the given path is <code>null</code>.
	 * @throws IllegalArgumentException if the given string is not a path or the path is not relative.
	 * @see #isPath(String)
	 */
	public static String checkRelativePath(final String path) throws IllegalArgumentException {
		if(isPathAbsolute(checkPath(path))) { //check the path; if it is a path but it is absolute
			throw new IllegalArgumentException("The given path " + path + " is not relative.");
		}
		return path; //return the relative path
	}

	/**
	 * Constructs an absolute path from the given elements in the form: <code>/<var>element1</var>/<var>element2</var></code>. Each element in the path is
	 * URI-encoded using UTF-8.
	 * @param absolute <code>true</code> if the path should be absolute and therefore should begin with '/'.
	 * @param collection <code>true</code> if the path should be a collection and therefore end with `/`.
	 * @param pathElements <code>true</code> if the path represents a collection and therefore should end with '/'.
	 * @return A path constructed according to the given rules.
	 * @throws IllegalArgumentException if there are no path elements and an absolute non-collection or non-absolute collection is requested.
	 * @see #encode(String)
	 */
	public static String constructPath(final boolean absolute, final boolean collection, final String... pathElements) {
		if(pathElements.length == 0 && absolute != collection) { //if there are no path elements, an absolute URI must also be a collection
			throw new IllegalArgumentException("A path with no elements must be an absolute collection or a relative non-collection.");
		}
		final StringBuilder stringBuilder = new StringBuilder(); //create a string builder
		if(absolute) { //if this should be an absolute path
			stringBuilder.append(PATH_SEPARATOR); //prepend '/'
		}
		for(final String pathElement : pathElements) { //look at each path element
			stringBuilder.append(encode(pathElement)); //encode and append this path element
			stringBuilder.append(pathElement); //encode and append this path element
			stringBuilder.append(PATH_SEPARATOR); //separate the path elements
		}
		if(!collection && pathElements.length > 0) { //if there were path elements but this wasn't a collection, we have one too many path separators 
			stringBuilder.deleteCharAt(stringBuilder.length() - 1); //remove the last character, a '/'
		}
		return stringBuilder.toString(); //return the string version of the constructed path
	}

	/**
	 * Returns the name of the resource at the given path, which will be the name of the last path component. If the path is a collection (i.e. it ends with
	 * slash), the component before the last slash will be returned. As examples, "/path/name.ext" and "name.ext" will return "name.ext". "/path/", "path/", and
	 * "path" will all return "path".
	 * @param path The path, which should be encoded if {@value URIs#PATH_SEPARATOR} characters are present within a path component.
	 * @return The name of the last last path component, the empty string if the path is the empty string, or "/" if the path is the root path.
	 * @throws NullPointerException if the given path is <code>null</code>.
	 */
	public static String getName(final String path) {
		final int length = path.length(); //get the length of the path
		if(length > 0) { //if there are path characters
			int endIndex = length; //start at the end of the path (endIndex will always be one position after the ending character)
			if(path.charAt(endIndex - 1) == PATH_SEPARATOR) { //if the path ends with a path separator
				--endIndex; //skip the ending path separator
			}
			final int beginIndex = path.lastIndexOf(PATH_SEPARATOR, endIndex - 1) + 1; //get the index after the previous separator; if there are no previous separators, this will correctly yield index 0
			if(endIndex - beginIndex > 0) { //if there are characters to collect (if not, this is the root path, "/")
				return path.substring(beginIndex, endIndex); //return the name we found
			}
			assert ROOT_PATH.equals(path) : "Path unexpectedly not the root path.";
		}
		return path; //the path (either "" or "/") is already its name
	}

	/**
	 * Determines whether the given path is a canonical collection path.
	 * <p>
	 * Non-normalized collection paths (e.g. ending with <code>/.</code>) are not supported.
	 * </p>
	 * @param path The raw path to examine.
	 * @return <code>true</code> if the path ends with a slash ('/').
	 * @throws NullPointerException if the given path is <code>null</code>.
	 */
	public static boolean isCollectionPath(@Nonnull final String path) {
		return endsWith(path, PATH_SEPARATOR); //see if the path ends with '/'		
	}

	/**
	 * Determines if a given path is only a path and not a URI with a scheme and/or authority.
	 * @param path The string version of a path to determine if it.
	 * @return <code>true</code> if the path is a path and does not specify a scheme (i.e. the URI is not absolute) or authority.
	 * @throws NullPointerException if the given path is <code>null</code>.
	 * @see #isPathURI(URI)
	 */
	public static boolean isPath(final String path) {
		final URI pathURI = URI.create(requireNonNull(path, "Path cannot be null")); //create a URI from the given path
		return isPathURI(pathURI); //indicate whether the constructed URI represents a path
	}

	/**
	 * Determines whether the given path is absolute.
	 * @param path The path to examine.
	 * @return <code>true</code> if the path begins with a slash ('/').
	 * @throws NullPointerException if the path is <code>null</code>.
	 */
	public static boolean isPathAbsolute(@Nonnull final String path) {
		return requireNonNull(path, "Path cannot be null").startsWith(ROOT_PATH); //see if the path begins with '/'		
	}

	/**
	 * Normalizes the given path by resolving the <code>.</code> and <code>..</code> path segments.
	 * @param path The path to normalize.
	 * @return The normalized form of the given path.
	 * @throws NullPointerException if the given path is <code>null</code>.
	 * @throws IllegalArgumentException if the provided path specifies a URI scheme (i.e. the URI is absolute) and/or authority.
	 * @see #normalize(URI)
	 */
	public static String normalizePath(final String path) {
		return normalize(createPathURI(path)).getPath(); //get a URI from the path, normalize that URI, and then return the path of the resulting URI
	}

}
