package com.garretwilson.net.http;

/**Constants relating to the HyperText Transfer Protocol (HTTP).
@author Garret Wilson
*/
public interface HTTPConstants
{

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
