package com.garretwilson.net.http;

/**Constants relating to the HyperText Transfer Protocol (HTTP).
@author Garret Wilson
*/
public class HTTPConstants
{

	/**The HTTP header indicating the accepted content types.*/
	public final static String ACCEPT_HEADER="accept";
	/**The HTTP header for cache control.*/
	public final static String CACHE_CONTROL_HEADER="Cache-Control";
		/**The HTTP no-cache cache control header.*/
		public final static String NO_CACHE_CACHE_CONTROL="no-cache";
	/**The HTTP header indicating the content description.*/
	public final static String CONTENT_DESCRIPTION_HEADER="Content-Description";
	/**The HTTP header indicating the content disposition.*/
	public final static String CONTENT_DISPOSITION_HEADER="Content-Disposition";
	/**The HTTP header indicating the expiration of the content.*/
	public final static String EXPIRES_HEADER="Expires";
	/**The HTTP pragma header.*/
	public final static String PRAGMA_HEADER="Pragma";
		/**The HTTP no-cache pragma header.*/
		public final static String NO_CACHE_PRAGMA="no-cache";
	/**The HTTP header indicating the referring location.*/
	public final static String REFERER_HEADER="Referer";

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

}
