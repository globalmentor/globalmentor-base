/*
 * Copyright © 1996-2022 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import java.net.URI;
import java.nio.charset.Charset;
import java.util.Objects;
import java.util.regex.Pattern;

import com.globalmentor.java.Characters;

import static com.globalmentor.java.Characters.*;
import static com.globalmentor.net.URIs.*;
import static java.lang.String.format;
import static java.nio.charset.StandardCharsets.*;

/**
 * Constants and utilities relating to the HyperText Transfer Protocol (HTTP) as defined by
 * <a href="https://www.rfc-editor.org/rfc/rfc9110.html#name-status-codes">RFC 9110</a>, <cite>HTTP Semantics</cite> and related specifications.
 * @see <a href="https://www.rfc-editor.org/rfc/rfc6585">RFC 6585: Additional HTTP Status Codes</a>
 * @see <a href="https://www.rfc-editor.org/rfc/rfc9110.html">RFC 9110: HTTP Semantics</a>
 * @see <a href="https://www.rfc-editor.org/rfc/rfc9112.html">RFC 9112: HTTP/1.1</a>
 * @author Garret Wilson
 */
public class HTTP {

	/** The HTTP scheme identifier. */
	public static final String HTTP_URI_SCHEME = "http";
	/** The HTTPS scheme identifier. */
	public static final String HTTPS_URI_SCHEME = "https";

	/** The default HTTP port, 80. */
	public static final int DEFAULT_PORT = 80;
	/** The default HTTPS port, 143. */
	public static final int DEFAULT_SECURE_PORT = 443;

	/** The charset used for HTTP. */
	public static final Charset CHARSET = US_ASCII;

	// response context header fields
	/** The HTTP header field indicating the allowed methods. */
	public static final String ALLOW_HEADER = "Allow";
	/** The HTTP header field indicating the destination of a redirection. */
	public static final String LOCATION_HEADER = "Location";
	/** The HTTP header field indicating how long the user agent ought to wait before making a follow-up request. */
	public static final String RETRY_AFTER_HEADER = "Retry-After";
	/** The HTTP header field containing information about the software used by the origin server to handle the request. */
	public static final String SERVER_HEADER = "Server";

	//content negotiation header fields
	/** The HTTP header field indicating the accepted content types. */
	public static final String ACCEPT_HEADER = "Accept";
	/** The HTTP header field indicating the accepted encodings. */
	public static final String ACCEPT_CHARSET_HEADER = "Accept-Charset";
	/** The HTTP header field indicating the accepted encodings. */
	public static final String ACCEPT_ENCODING_HEADER = "Accept-Encoding";
	/** The HTTP header field indicating the accepted language types. */
	public static final String ACCEPT_LANGUAGE_HEADER = "Accept-Language";
	/** The HTTP header field describing what parts of a request message might have influenced selecting the content of this response. */
	public static final String VARY_HEADER = "Vary";

	/** The HTTP header field indicating authorization information. */
	public static final String AUTHORIZATION_HEADER = "Authorization";
	/** The HTTP header field for cache control. */
	public static final String CACHE_CONTROL_HEADER = "Cache-Control";

	/** The HTTP max-age cache control header. */
	public static final String MAX_AGE_CACHE_CONTROL = "max-age";
	/** The HTTP max-stale cache control header. */
	public static final String MAX_STALE_CACHE_CONTROL = "max-stale";
	/** The HTTP min-fresh cache control header. */
	public static final String MIN_FRESH_CACHE_CONTROL = "min-fresh";
	/** The HTTP must-revalidate cache control header. */
	public static final String MUST_REVALIDATE_CACHE_CONTROL = "must-revalidate";
	/** The HTTP no-cache cache control header. */
	public static final String NO_CACHE_CACHE_CONTROL = "no-cache";
	/** The HTTP no-store cache control header. */
	public static final String NO_STORE_CACHE_CONTROL = "no-store";
	/** The HTTP no-transform cache control header. */
	public static final String NO_TRANSFORM_CACHE_CONTROL = "no-transform";
	/** The HTTP proxy-revalidate cache control header. */
	public static final String PROXY_REVALIDATE_CACHE_CONTROL = "proxy-revalidate";
	/** The HTTP s-maxage cache control header. */
	public static final String S_MAXAGE_CACHE_CONTROL = "s-maxage";

	/** The HTTP header field indicating the connection persistence. */
	public static final String CONNECTION_HEADER = "Connection";

	/** The HTTP "close" Connection header value. */
	public static final String CONNECTION_CLOSE = "close";

	/** The HTTP header field indicating the content description. */
	public static final String CONTENT_DESCRIPTION_HEADER = "Content-Description";
	/** The HTTP header field indicating the content disposition. */
	public static final String CONTENT_DISPOSITION_HEADER = "Content-Disposition";
	/** The HTTP header field indicating the content encoding. */
	public static final String CONTENT_ENCODING_HEADER = "Content-Encoding";

	/** The HTTP compress content coding. */
	public static final String COMPRESS_CONTENT_CODING = "compress";
	/** The HTTP deflate content coding. */
	public static final String DEFLATE_CONTENT_CODING = "deflate";
	/** The HTTP gzip content coding. */
	public static final String GZIP_CONTENT_CODING = "gzip";
	/** The HTTP identity content coding. */
	public static final String IDENTITY_CONTENT_CODING = "identity";

	/** The HTTP header field indicating the natural language(s) of the intended audience for the enclosed entity. */
	public static final String CONTENT_LANGUAGE_HEADER = "Content-Language";
	/** The HTTP header field indicating the size of the entity body. */
	public static final String CONTENT_LENGTH_HEADER = "Content-Length";
	/** The HTTP header field indicating the canonical location of the resource. */
	public static final String CONTENT_LOCATION_HEADER = "Content-Location";
	/** The HTTP header field indicating the MD5 digest of the body. */
	public static final String CONTENT_MD5_HEADER = "Content-MD5";
	/** The HTTP header field indicating the content type. */
	public static final String CONTENT_TYPE_HEADER = "Content-Type";
	/** The HTTP header field indicating the date of the message, in RFC 1123 format. */
	public static final String DATE_HEADER = "Date";
	/** The HTTP header field indicating the expiration of the content. */
	public static final String EXPIRES_HEADER = "Expires";
	/** The HTTP header field indicating Internet host and port number of the resource being requested. */
	public static final String HOST_HEADER = "Host";
	/** The HTTP header field indicating content should be retrieved only if it has been modified after a certain date. */
	public static final String IF_MODIFIED_SINCE_HEADER = "If-Modified-Since";
	/** The HTTP header field indicating content should be retrieved only if it has not been modified after a certain date. */
	public static final String IF_UNMODIFIED_SINCE_HEADER = "If-Unmodified-Since";
	/** The HTTP header field indicating the date and time at which the origin server believes the variant was last modified. */
	public static final String LAST_MODIFIED_HEADER = "Last-Modified";
	/** The HTTP pragma header. */
	public static final String PRAGMA_HEADER = "Pragma";

	/** The HTTP no-cache pragma. */
	public static final String NO_CACHE_PRAGMA = "no-cache";

	/** The HTTP header field indicating the referring location. */
	public static final String REFERER_HEADER = "Referer";
	/**
	 * The HTTP header field indicating what extension transfer-codings it is willing to accept in the response and whether or not it is willing to accept trailer
	 * fields in a chunked transfer-coding.
	 */
	public static final String TE_HEADER = "TE";
	/**
	 * The HTTP header field indicating the what (if any) type of transformation has been applied to the message body in order to safely transfer it between the
	 * sender and the recipient.
	 */
	public static final String TRANSFER_ENCODING_HEADER = "Transfer-Encoding";

	/** The HTTP chunked transfer coding. */
	public static final String CHUNKED_TRANSFER_CODING = "chunked";
	/** The HTTP compress transfer coding. */
	public static final String COMPRESS_TRANSFER_CODING = "compress";
	/** The HTTP deflate transfer coding. */
	public static final String DEFLATE_TRANSFER_CODING = "deflate";
	/** The HTTP gzip transfer coding. */
	public static final String GZIP_TRANSFER_CODING = "gzip";
	/** The HTTP identity transfer coding. */
	public static final String IDENTITY_TRANSFER_CODING = "identity";

	/** The HTTP header field indicating the user agent. */
	public static final String USER_AGENT_HEADER = "User-Agent";
	/** The HTTP header field indicating the credentials expected by the server. */
	public static final String WWW_AUTHENTICATE_HEADER = "WWW-Authenticate";

	/** The HTTP <code>GET</code> method. */
	public static final String GET_METHOD = "GET";
	/** The HTTP <code>HEAD</code> method. */
	public static final String HEAD_METHOD = "HEAD";
	/** The HTTP <code>POST</code> method. */
	public static final String POST_METHOD = "POST";
	/** The HTTP <code>PUT</code> method. */
	public static final String PUT_METHOD = "PUT";
	/** The HTTP <code>DELETE</code> method. */
	public static final String DELETE_METHOD = "DELETE";
	/** The HTTP <code>CONNECT</code> method. */
	public static final String CONNECT_METHOD = "CONNECT";
	/** The HTTP <code>OPTIONS</code> method. */
	public static final String OPTIONS_METHOD = "OPTIONS";
	/** The HTTP <code>TRACE</code> method. */
	public static final String TRACE_METHOD = "TRACE";

	/** The "realm" parameter used in headers such as WWW-Authenticate. */
	public static final String REALM_PARAMETER = "realm";

	/** The status code classes (categories) of response. */
	public static enum ResponseClass {
		/** <code>1xx</code> (Informational): The request was received, continuing process. */
		INFORMATIONAL,
		/** <code>2xx</code> (Successful): The request was successfully received, understood, and accepted. */
		SUCCESSFUL,
		/** <code>3xx</code> (Redirection): Further action needs to be taken in order to complete the request. */
		REDIRECTION,
		/** <code>4xx</code> (Client Error): The request contains bad syntax or cannot be fulfilled. */
		CLIENT_ERROR,
		/** <code>*5xx</code> (Server Error): The server failed to fulfill an apparently valid request. */
		SERVER_ERROR;

		/**
		 * Determines the class of response of the given status code.
		 * @param statusCode The status code in the response class.
		 * @return The response class for the status code.
		 * @throws IllegalArgumentException if the status code is not within the valid range of <code>100</code> to <code>599</code>, inclusive.
		 */
		public static ResponseClass forStatusCode(final int statusCode) {
			switch(statusCode / 100) {
				case 1:
					return INFORMATIONAL;
				case 2:
					return SUCCESSFUL;
				case 3:
					return REDIRECTION;
				case 4:
					return CLIENT_ERROR;
				case 5:
					return SERVER_ERROR;
				default:
					throw new IllegalArgumentException(format("Status code %d not within the valid range of 100 to 599, inclusive.", statusCode));
			}
		}
	}

	/** Status code <code>100 Continue</code>. */
	public static final int SC_CONTINUE = 100;
	/** Status code <code>101 Switching Protocols</code>. */
	public static final int SC_SWITCHING_PROTOCOLS = 101;

	/** Status code <code>200 OK</code>. */
	public static final int SC_OK = 200;
	/** Status code <code>201 Created</code>. */
	public static final int SC_CREATED = 201;
	/** Status code <code>202 Accepted</code>. */
	public static final int SC_ACCEPTED = 202;
	/** Status code <code>203 Non-Authoritative Information</code>. */
	public static final int SC_NON_AUTHORITATIVE_INFORMATION = 203;
	/** Status code <code>204 No Content</code>. */
	public static final int SC_NO_CONTENT = 204;
	/** Status code <code>205 Reset Content</code>. */
	public static final int SC_RESET_CONTENT = 205;
	/** Status code <code>206 Partial Content</code>. */
	public static final int SC_PARTIAL_CONTENT = 206;

	/** Status code <code>300 Multiple Choices</code>. */
	public static final int SC_MULTIPLE_CHOICES = 300;
	/** Status code <code>301 Moved Permanently</code>. */
	public static final int SC_MOVED_PERMANENTLY = 301;
	/** Status code <code>302 Found</code> (moved temporarily). */
	public static final int SC_MOVED_TEMPORARILY = 302;
	/** Status code <code>303 See Other</code>. */
	public static final int SC_SEE_OTHER = 303;
	/** Status code <code>304 Not Modified</code>. */
	public static final int SC_NOT_MODIFIED = 304;
	/** Status code <code>305 Use Proxy</code>. */
	public static final int SC_USE_PROXY = 305;
	/** Status code <code>307 Temporary Redirect</code>. */
	public static final int SC_TEMPORARY_REDIRECT = 307;
	/** Status code <code>308 Permanent Redirect</code>. */
	public static final int SC_PERMANENT_REDIRECT = 308;

	/** Status code <code>400 Bad Request</code>. */
	public static final int SC_BAD_REQUEST = 400;
	/** Status code <code>401 Unauthorized</code>. */
	public static final int SC_UNAUTHORIZED = 401;
	/** Status code <code>402 Payment Required</code>. */
	public static final int SC_PAYMENT_REQUIRED = 402;
	/** Status code <code>403 Forbidden</code>. */
	public static final int SC_FORBIDDEN = 403;
	/** Status code <code>404 Not Found</code>. */
	public static final int SC_NOT_FOUND = 404;
	/** Status code <code>405 Method Not Allowed</code>. */
	public static final int SC_METHOD_NOT_ALLOWED = 405;
	/** Status code <code>406 Not Acceptable</code>. */
	public static final int SC_NOT_ACCEPTABLE = 406;
	/** Status code <code>407 Proxy Authentication Required</code>. */
	public static final int SC_PROXY_AUTHENTICATION_REQUIRED = 407;
	/** Status code <code>408 Request Timeout</code>. */
	public static final int SC_REQUEST_TIMEOUT = 408;
	/** Status code <code>409 Conflict</code>. */
	public static final int SC_CONFLICT = 409;
	/** Status code <code>410 Gone</code>. */
	public static final int SC_GONE = 410;
	/** Status code <code>411 Length Required</code>. */
	public static final int SC_LENGTH_REQUIRED = 411;
	/** Status code <code>412 Precondition Failed</code>. */
	public static final int SC_PRECONDITION_FAILED = 412;
	/** Status code <code>413 Content Too Large</code>. */
	public static final int SC_CONTENT_TOO_LARGE = 413;
	/** Status code <code>414 URI Too Long</code>. */
	public static final int SC_URI_TOO_LONG = 414;
	/** Status code <code>415 Unsupported Media Type</code>. */
	public static final int SC_UNSUPPORTED_MEDIA_TYPE = 415;
	/** Status code <code>416 Range Not Satisfiable</code>. */
	public static final int SC_RANGE_NOT_SATISFIABLE = 416;
	/** Status code <code>417 Expectation Failed</code>. */
	public static final int SC_EXPECTATION_FAILED = 417;
	/** Status code <code>421 Misdirected Request</code>. */
	public static final int SC_MISDIRECTED_REQUEST = 421;
	/** Status code <code>422 Unprocessable Content</code>. */
	public static final int SC_UNPROCESSABLE_CONTENT = 422;
	/** Status code <code>426 Upgrade Required</code>. */
	public static final int SC_UPGRADE_REQUIRED = 426;
	/**
	 * Status code <code>428 Precondition Required</code>.
	 * @see <a href="https://www.rfc-editor.org/rfc/rfc6585">RFC 6585: Additional HTTP Status Codes</a>
	 */
	public static final int SC_PRECONDITION_REQUIRED = 428;
	/**
	 * Status code <code>429 Too Many Requests</code>.
	 * @see <a href="https://www.rfc-editor.org/rfc/rfc6585">RFC 6585: Additional HTTP Status Codes</a>
	 */
	public static final int SC_TOO_MANY_REQUESTS = 429;
	/**
	 * Status code <code>431 Request Header Fields Too Large</code>.
	 * @see <a href="https://www.rfc-editor.org/rfc/rfc6585">RFC 6585: Additional HTTP Status Codes</a>
	 */
	public static final int SC_REQUEST_HEADER_FIELDS_TOO_LARGE = 431;
	/**
	 * Status code <code>511 Network Authentication Required</code>.
	 * @see <a href="https://www.rfc-editor.org/rfc/rfc6585">RFC 6585: Additional HTTP Status Codes</a>
	 */
	public static final int SC_NETWORK_AUTHENTICATION_REQUIRED = 511;

	/** Status code <code>500 Internal Server Error</code>. */
	public static final int SC_INTERNAL_SERVER_ERROR = 500;
	/** Status code <code>501 Not Implemented </code>. */
	public static final int SC_NOT_IMPLEMENTED = 501;
	/** Status code <code>502 Bad Gateway</code>. */
	public static final int SC_BAD_GATEWAY = 502;
	/** Status code <code>503 Service Unavailable</code>. */
	public static final int SC_SERVICE_UNAVAILABLE = 503;
	/** Status code <code>504 Gateway Timeout</code>. */
	public static final int SC_GATEWAY_TIMEOUT = 504;
	/** Status code <code>505 HTTP Version Not Supported</code>. */
	public static final int SC_HTTP_VERSION_NOT_SUPPORTED = 505;

	/** The character '.' which separates components of an HTTP version. */
	public static final char VERSION_DELIMITER = '.';

	/** The character '/' which separates the letters "HTTP" from the HTTP version. */
	public static final char VERSION_SEPARATOR = '/';

	/** The string "HTTP" begins an HTTP version. */
	public static final String VERSION_IDENTIFIER = "HTTP";

	/** The character for delimiting HTTP list items. */
	public static final char LIST_DELIMITER = COMMA_CHAR;

	/** The regular expression pattern for the character for delimiting HTTP list items. */
	public static final Pattern LIST_DELIMITER_PATTERN = Pattern.compile(String.valueOf(LIST_DELIMITER));

	/** The character for delimiting parameters, such as qvalue weights in lists. */
	public static final char PARAMETER_DELIMITER = SEMICOLON_CHAR;

	/** The character which separates a header name from a header value. */
	public static final char HEADER_SEPARATOR = ':';

	/** The character used to escape quotation marks. */
	public static final char ESCAPE_CHAR = '\\';

	/** The HTTP character used as a wildcard. */
	public static final char WILDCARD_CHAR = '*';

	/** US-ASCII control characters. */
	public static final Characters CTL_CHARACTERS = Characters.ofRange((char)0, (char)31).add((char)127);

	/** US-ASCII carriage return. */
	public static final char CR = (char)13;
	/** US-ASCII linefeed. */
	public static final char LF = (char)10;
	/** US-ASCII space. */
	public static final char SP = (char)32;
	/** US-ASCII horizontal tab. */
	public static final char HT = (char)9;
	/** US-ASCII double-quote mark. */
	public static final char QUOTE = (char)34;

	/** The CR+LF carriage return + linefeed combination. */
	public static final String CRLF = "" + CR + LF;

	/** Characters that must be in a quoted string to be included in a parameter value. */
	public static final Characters SEPARATOR_CHARACTERS = Characters.of('(', ')', '<', '>', '@', ',', ';', ':', '\\', QUOTE, '/', '[', ']', '?', '=', '{', '}',
			SP, HT);

	/** Characters that delimit tokens. */
	public static final Characters DELIMITER_CHARACTERS = CTL_CHARACTERS.add(SEPARATOR_CHARACTERS);

	/** Linear whitespace characters which can, in the correct sequence, be folded into single spaces. */
	public static final Characters LWS_CHARACTERS = Characters.of(CR, LF, SP, HT);

	/** The regular expression string matching linear whitespace. */
	public static final String LWS_REGULAR_EXPRESSION = "[\\r\\f \\t]";

	/**
	 * The regular expression pattern for a weighted list element. Matching group 1 is the list element. Matching group 2 is the quality value, which may be null
	 * if no quality value is present.
	 */
	public static final Pattern LIST_ELEMENT_WEIGHT_PATTERN = Pattern.compile("([^;]*)(?:;" + LWS_REGULAR_EXPRESSION + "*q=(\\d(?:\\.\\d{1,3})?))?");

	/**
	 * The regular expression for a product version in the form X.X.X, where all but the first version is optional. Each group represents a numeric segment of the
	 * version string. This regular expression accepts versions with letters, such as "7.0b" and "1.23b2", in which case the letters and everything after are not
	 * included in the groups. This regular expression therefore accepts "6.0", "7.0b", "8.9.10", and "2.17b3".
	 */
	public static final String PRODUCT_VERSION_REGEX = "(\\d+)(?:\\.(\\d+)[a-z]*\\d*(?:\\.(\\d+)[a-z]*\\d?)?)?";

	/**
	 * Determines if the given URI has an HTTP scheme, either {@value #HTTP_URI_SCHEME} or {@value #HTTPS_URI_SCHEME}.
	 * @param uri The URI the scheme of which to test.
	 * @return <code>true</code> if the given URI has a scheme designating HTTP or secure HTTP.
	 * @see #isHTTPScheme(String)
	 * @see #HTTP_URI_SCHEME
	 * @see #HTTPS_URI_SCHEME
	 */
	public static boolean isHTTPURI(final URI uri) {
		final String scheme = uri.getScheme(); //get the URI scheme
		return scheme != null && isHTTPScheme(scheme);
	}

	/**
	 * Determines if the given scheme is an HTTP scheme, either {@value #HTTP_URI_SCHEME} or {@value #HTTPS_URI_SCHEME}.
	 * @param scheme The scheme to test.
	 * @return <code>true</code> if the given scheme designates HTTP or secure HTTP.
	 * @throws NullPointerException if the given scheme is <code>null</code>.
	 * @see #HTTP_URI_SCHEME
	 * @see #HTTPS_URI_SCHEME
	 */
	public static boolean isHTTPScheme(final String scheme) {
		return scheme.equals(HTTP_URI_SCHEME) || scheme.equals(HTTPS_URI_SCHEME); //see if the scheme is "http" or "https"
	}

	/**
	 * Makes sure that a URI matches the security or non-security of its scheme with that of another URI. That is, if both URIs have HTTP schemes, the scheme of
	 * the URI is made to match the scheme ({@value #HTTP_URI_SCHEME} or {@value #HTTPS_URI_SCHEME}) of the other URI.
	 * @param uri The URI to change.
	 * @param matchURI The URI the scheme of which to match.
	 * @return A URI with a scheme matching the match URI, if both are already HTTP URIs.
	 * @see #isHTTPURI(URI)
	 * @see #HTTP_URI_SCHEME
	 * @see #HTTPS_URI_SCHEME
	 */
	public static URI matchSchemeSecurity(URI uri, final URI matchURI) {
		final String uriScheme = uri.getScheme();
		final String matchURIScheme = matchURI.getScheme();
		if(!Objects.equals(uriScheme, matchURIScheme)) { //if the schemes are different
			if(uriScheme != null && isHTTPScheme(uriScheme) && matchURIScheme != null && isHTTPScheme(matchURIScheme)) { //if both schemes are HTTP schemes
				uri = changeScheme(uri, matchURIScheme); //set the scheme to match the match URI
			}
		}
		return uri;
	}
}
