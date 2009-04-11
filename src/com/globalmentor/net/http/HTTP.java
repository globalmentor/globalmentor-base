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

package com.globalmentor.net.http;

import java.net.URI;
import java.nio.charset.Charset;
import java.util.regex.Pattern;

import javax.servlet.http.HttpServletResponse;

import static com.globalmentor.io.Charsets.*;
import static com.globalmentor.java.Characters.*;
import static com.globalmentor.java.Strings.*;

/**Constants and utilities relating to the HyperText Transfer Protocol (HTTP) as defined by
<a href="http://www.ietf.org/rfc/rfc2616.txt">RFC 2616</a>,	"Hypertext Transfer Protocol -- HTTP/1.1".
<p>Status code declarations and comments used from Tomcat org.apache.catalina.servlets.WebdavServlet by Remy Maucherat Revision: 1.19 $ $Date: 2004/09/19 01:20:10.</p>
@see <a href="http://www.ietf.org/rfc/rfc2616.txt">RFC 2616</a>  
@author Garret Wilson
*/
public class HTTP
{

	/**The HTTP scheme identifier.*/
	public final static String HTTP_URI_SCHEME="http";
	/**The HTTPS scheme identifier.*/
	public final static String HTTPS_SCHEME="https";

	/**The default HTTP port, 80.*/
	public final static int DEFAULT_PORT=80;
	/**The default HTTPS port, 143.*/
	public final static int DEFAULT_SECURE_PORT=443;

	/**The charset used for HTTP.*/
	public final static Charset HTTP_CHARSET=US_ASCII_CHARSET;
	
	/**The HTTP header indicating the allowed methods.*/
	public final static String ALLOW_HEADER="Allow";
	/**The HTTP header indicating the accepted content types.*/
	public final static String ACCEPT_HEADER="Accept";
	/**The HTTP header indicating the accepted encodings.*/
	public final static String ACCEPT_ENCODING_HEADER="Accept-Encoding";
	/**The HTTP header indicating the accepted language types.*/
	public final static String ACCEPT_LANGUAGE_HEADER="Accept-Language";
	/**The HTTP header indicating authorization information.*/
	public final static String AUTHORIZATION_HEADER="Authorization";
	/**The HTTP header for cache control.*/
	public final static String CACHE_CONTROL_HEADER="Cache-Control";
		/**The HTTP max-age cache control header.*/
		public final static String MAX_AGE_CACHE_CONTROL="max-age";
		/**The HTTP max-stalee cache control header.*/
		public final static String MAX_STALE_CACHE_CONTROL="max-stale";
		/**The HTTP min-fresh cache control header.*/
		public final static String MIN_FRESH_CACHE_CONTROL="min-fresh";
		/**The HTTP must-revalidate cache control header.*/
		public final static String MUST_REVALIDATE_CACHE_CONTROL="must-revalidate";
		/**The HTTP no-cache cache control header.*/
		public final static String NO_CACHE_CACHE_CONTROL="no-cache";
		/**The HTTP no-store cache control header.*/
		public final static String NO_STORE_CACHE_CONTROL="no-store";
		/**The HTTP no-transform cache control header.*/
		public final static String NO_TRANSFORM_CACHE_CONTROL="no-transform";
		/**The HTTP proxy-revalidate cache control header.*/
		public final static String PROXY_REVALIDATE_CACHE_CONTROL="proxy-revalidate";
		/**The HTTP s-maxage cache control header.*/
		public final static String S_MAXAGE_CACHE_CONTROL="s-maxage";
	/**The HTTP header indicating the connection persistency.*/
	public final static String CONNECTION_HEADER="Connection";
		/**The HTTP "close" Connection header value.*/
		public final static String CONNECTION_CLOSE="close";
	/**The HTTP header indicating the content description.*/
	public final static String CONTENT_DESCRIPTION_HEADER="Content-Description";
	/**The HTTP header indicating the content disposition.*/
	public final static String CONTENT_DISPOSITION_HEADER="Content-Disposition";
	/**The HTTP header indicating the content encoding.*/
	public final static String CONTENT_ENCODING_HEADER="Content-Encoding";
		/**The HTTP compress content coding.*/
		public final static String COMPRESS_CONTENT_CODING="compress";
		/**The HTTP deflate content coding.*/
		public final static String DEFLATE_CONTENT_CODING="deflate";
		/**The HTTP gzip content coding.*/
		public final static String GZIP_CONTENT_CODING="gzip";
		/**The HTTP identity content coding.*/
		public final static String IDENTITY_CONTENT_CODING="identity";
	/**The HTTP header indicating the natural language(s) of the intended audience for the enclosed entity.*/
	public final static String CONTENT_LANGUAGE_HEADER="Content-Language";
	/**The HTTP header indicating the size of the entity body.*/
	public final static String CONTENT_LENGTH_HEADER="Content-Length";
	/**The HTTP header indicating the canonical location of the resource.*/
	public final static String CONTENT_LOCATION_HEADER="Content-Location";
	/**The HTTP header indicating the MD5 digest of the body.*/
	public final static String CONTENT_MD5_HEADER="Content-MD5";
	/**The HTTP header indicating the content type.*/
	public final static String CONTENT_TYPE_HEADER="Content-Type";
	/**The HTTP header indicating the date of the message, in RFC 1123 format.*/
	public final static String DATE_HEADER="Date";
	/**The HTTP header indicating the expiration of the content.*/
	public final static String EXPIRES_HEADER="Expires";
	/**The HTTP header indicating Internet host and port number of the resource being requested.*/
	public final static String HOST_HEADER="Host";
	/**The HTTP header indicating content should be retrieved only if it has been modified after a certain date.*/
	public final static String IF_MODIFIED_SINCE_HEADER="If-Modified-Since";
	/**The HTTP header indicating content should be retrieved only if it has not been modified after a certain date.*/
	public final static String IF_UNMODIFIED_SINCE_HEADER="If-Unmodified-Since";	
	/**The HTTP header indicating the date and time at which the origin server believes the variant was last modified.*/
	public final static String LAST_MODIFIED_HEADER="Last-Modified";
	/**The HTTP header indicating the destination of a redirection.*/
	public final static String LOCATION_HEADER="Location";
	/**The HTTP pragma header.*/
	public final static String PRAGMA_HEADER="Pragma";
		/**The HTTP no-cache pragma.*/
		public final static String NO_CACHE_PRAGMA="no-cache";
	/**The HTTP header indicating the referring location.*/
	public final static String REFERER_HEADER="Referer";
	/**The HTTP header indicating what extension transfer-codings it is willing to accept in the response and whether or not it is willing to accept trailer fields in a chunked transfer-coding.*/
	public final static String TE_HEADER="TE";
	/**The HTTP header indicating the what (if any) type of transformation has been applied to the message body in order to safely transfer it between the sender and the recipient.*/
	public final static String TRANSFER_ENCODING_HEADER="Transfer-Encoding";
		/**The HTTP chunked transfer coding.*/
		public final static String CHUNKED_TRANSFER_CODING="chunked";
		/**The HTTP compress transfer coding.*/
		public final static String COMPRESS_TRANSFER_CODING="compress";
		/**The HTTP deflate transfer coding.*/
		public final static String DEFLATE_TRANSFER_CODING="deflate";
		/**The HTTP gzip transfer coding.*/
		public final static String GZIP_TRANSFER_CODING="gzip";
		/**The HTTP identity transfer coding.*/
		public final static String IDENTITY_TRANSFER_CODING="identity";
	/**The HTTP header indicating the user agent.*/
	public final static String USER_AGENT_HEADER="User-Agent";
	/**The HTTP header indicating the credentials expected by the server.*/
	public final static String WWW_AUTHENTICATE_HEADER="WWW-Authenticate";

	/**The HTTP GET method.*/
	public final static String GET_METHOD="GET";
	/**The HTTP POST method.*/
	public final static String POST_METHOD="POST";
	/**The HTTP HEAD method.*/
	public final static String HEAD_METHOD="HEAD";
	/**The HTTP OPTIONS method.*/
	public final static String OPTIONS_METHOD="OPTIONS";
	/**The HTTP PUT method.*/
	public final static String PUT_METHOD="PUT";
	/**The HTTP DELETE method.*/
	public final static String DELETE_METHOD="DELETE";
	/**The HTTP TRACE method.*/
	public final static String TRACE_METHOD="TRACE";

	/**The "realm" parameter used in headers such as WWW-Authenticate.*/
	public final static String REALM_PARAMETER="realm";

	/**
   * Status code (200) indicating the request succeeded normally.
   */
  public static final int SC_OK = HttpServletResponse.SC_OK;


  /**
   * Status code (201) indicating the request succeeded and created
   * a new resource on the server.
   */
  public static final int SC_CREATED = HttpServletResponse.SC_CREATED;


  /**
   * Status code (202) indicating that a request was accepted for
   * processing, but was not completed.
   */
  public static final int SC_ACCEPTED = HttpServletResponse.SC_ACCEPTED;


  /**
   * Status code (204) indicating that the request succeeded but that
   * there was no new information to return.
   */
  public static final int SC_NO_CONTENT = HttpServletResponse.SC_NO_CONTENT;


  /**
   * Status code (301) indicating that the resource has permanently
   * moved to a new location, and that future references should use a
   * new URI with their requests.
   */
  public static final int SC_MOVED_PERMANENTLY =
      HttpServletResponse.SC_MOVED_PERMANENTLY;


  /**
   * Status code (302) indicating that the resource has temporarily
   * moved to another location, but that future references should
   * still use the original URI to access the resource.
   */
  public static final int SC_MOVED_TEMPORARILY =
      HttpServletResponse.SC_MOVED_TEMPORARILY;


  /**
   * Status code (304) indicating that a conditional GET operation
   * found that the resource was available and not modified.
   */
  public static final int SC_NOT_MODIFIED =
      HttpServletResponse.SC_NOT_MODIFIED;


  /**
   * Status code (400) indicating the request sent by the client was
   * syntactically incorrect.
   */
  public static final int SC_BAD_REQUEST =
      HttpServletResponse.SC_BAD_REQUEST;


  /**
   * Status code (401) indicating that the request requires HTTP
   * authentication.
   */
  public static final int SC_UNAUTHORIZED =
      HttpServletResponse.SC_UNAUTHORIZED;


  /**
   * Status code (403) indicating the server understood the request
   * but refused to fulfill it.
   */
  public static final int SC_FORBIDDEN = HttpServletResponse.SC_FORBIDDEN;


  /**
   * Status code (404) indicating that the requested resource is not
   * available.
   */
  public static final int SC_NOT_FOUND = HttpServletResponse.SC_NOT_FOUND;


  /**
   * Status code (500) indicating an error inside the HTTP service
   * which prevented it from fulfilling the request.
   */
  public static final int SC_INTERNAL_SERVER_ERROR =
      HttpServletResponse.SC_INTERNAL_SERVER_ERROR;


  /**
   * Status code (501) indicating the HTTP service does not support
   * the functionality needed to fulfill the request.
   */
  public static final int SC_NOT_IMPLEMENTED =
      HttpServletResponse.SC_NOT_IMPLEMENTED;


  /**
   * Status code (502) indicating that the HTTP server received an
   * invalid response from a server it consulted when acting as a
   * proxy or gateway.
   */
  public static final int SC_BAD_GATEWAY =
      HttpServletResponse.SC_BAD_GATEWAY;


  /**
   * Status code (503) indicating that the HTTP service is
   * temporarily overloaded, and unable to handle the request.
   */
  public static final int SC_SERVICE_UNAVAILABLE =
      HttpServletResponse.SC_SERVICE_UNAVAILABLE;


  /**
   * Status code (100) indicating the client may continue with
   * its request.  This interim response is used to inform the
   * client that the initial part of the request has been
   * received and has not yet been rejected by the server.
   */
  public static final int SC_CONTINUE = 100;


  /**
   * Status code (405) indicating the method specified is not
   * allowed for the resource.
   */
  public static final int SC_METHOD_NOT_ALLOWED = 405;


  /**
   * Status code (409) indicating that the request could not be
   * completed due to a conflict with the current state of the
   * resource.
   */
  public static final int SC_CONFLICT = 409;

  /**Status code (410) indicating that the requested resource is no longer available at the server and no forwarding address is known.*/
  public static final int SC_GONE=410;

  /**Status code (411) indicating that the server refuses to accept the request without a defined Content-Length.*/
  public static final int SC_LENGTH_REQUIRED=411;

  /**
   * Status code (412) indicating the precondition given in one
   * or more of the request-header fields evaluated to false
   * when it was tested on the server.
   */
  public static final int SC_PRECONDITION_FAILED = 412;


  /**
   * Status code (413) indicating the server is refusing to
   * process a request because the request entity is larger
   * than the server is willing or able to process.
   */
  public static final int SC_REQUEST_ENTITY_TOO_LARGE = 413;


  /**
   * Status code (415) indicating the server is refusing to service
   * the request because the entity of the request is in a format
   * not supported by the requested resource for the requested
   * method.
   */
  public static final int SC_UNSUPPORTED_MEDIA_TYPE = 415;

  
  
	/**The character '.' which separates components of an HTTP version.*/
	public final static char VERSION_DELIMITER='.';

	/**The character '/' which separates the letters "HTTP" from the HTTP version.*/
	public final static char VERSION_SEPARATOR='/';

	/**The string "HTTP" begins an HTTP version.*/
	public final static String VERSION_IDENTIFIER="HTTP";
  
  /**The character for delimiting HTTP list items.*/
	public final static char LIST_DELIMITER=COMMA_CHAR;

  /**The regular expression pattern for the character for delimiting HTTP list items.*/
	public final static Pattern LIST_DELIMITER_PATTERN=Pattern.compile(String.valueOf(LIST_DELIMITER));

	/**The character for delimiting parameters, such as qvalue weights in lists.*/
	public final static char PARAMETER_DELIMITER=SEMICOLON_CHAR;

  /**The character which separates a header name from a header value.*/
	public final static char HEADER_SEPARATOR=':';
	
	/**The character used to escape quotation marks.*/
	public final static char ESCAPE_CHAR='\\';

	/**The HTTP character used as a wildcard.*/
	public final static char WILDCARD_CHAR='*';

  /**US-ASCII control characters.*/
	public final static String CTL_CHARS=createString((char)0, (char)31)+(char)127;

  /**US-ASCII carriage return.*/
	public final static char CR=(char)13;
  /**US-ASCII linefeed.*/
	public final static char LF=(char)10;
  /**US-ASCII space.*/
	public final static char SP=(char)32;
  /**US-ASCII horizontal tab.*/
	public final static char HT=(char)9;
  /**US-ASCII double-quote mark.*/
	public final static char QUOTE=(char)34;

  /**The CR+LF carriage return + linefeed combination.*/
	public final static String CRLF=""+CR+LF;

	/**Characters that must be in a quoted string to be included in a parameter value.*/
	public final static String SEPARATOR_CHARS=new StringBuilder().append("()<>@,;:\\").append(QUOTE).append("/[]?={}").append(SP).append(HT).toString();

	/**Characters that delimit tokens.*/
	public final static String DELIMITER_CHARS=CTL_CHARS+SEPARATOR_CHARS; 

	/**Linear whitespace characters which can, in the correct sequence, be folded into single spaces.*/
	public final static String LWS_CHARS=new StringBuilder(CRLF).append(SP).append(HT).toString();

	/**The regular expression string matching linear whitespace.*/
	public final static String LWS_REGULAR_EXPRESSION="[\\r\\f \\t]";

	/**The regular expression attern for a weighted list element.
	Matching group 1 is the list element.
	Matching group 2 is the quality value, which may be null if no quality value is present.
	*/
	public final static Pattern LIST_ELEMENT_WEIGHT_PATTERN=Pattern.compile("([^;]*)(?:;"+LWS_REGULAR_EXPRESSION+"*q=(\\d(?:\\.\\d{1,3})?))?");

	/**The regular expression for a product version in the form X.X.X, where all but the first version is optional.
	Each group represents a numeric segment of the version string.
	This regular expression accepts versions with letters, such as "7.0b" and "1.23b2", in which case the letters and everything after are not included in the groups.
	This regular expression therefore accepts "6.0", "7.0b", "8.9.10", and "2.17b3".
	*/
	public final static String PRODUCT_VERSION_REGEX="(\\d+)(?:\\.(\\d+)[a-z]*\\d*(?:\\.(\\d+)[a-z]*\\d?)?)?";

	/**Determines if the given URI has an HTTP scheme, either {@value #HTTP_URI_SCHEME} or {@value #HTTPS_SCHEME}.
	@param uri The URI the scheme of which to test.
	@return <code>true</code> if the given URI has a scheme designating HTTP or secure HTTP.
	*/
	public static boolean isHTTPURI(final URI uri)
	{
		final String scheme=uri.getScheme();	//get the URI scheme
		return HTTP_URI_SCHEME.equals(scheme) || HTTPS_SCHEME.equals(scheme);	//see if the scheme is "http" or "https"
	}

}