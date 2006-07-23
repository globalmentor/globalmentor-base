package com.garretwilson.text.elff;

/**WebTrends-specific logging information.
@author Garret Wilson
*/
public class WebTrendsConstants
{

	/**The name of the WebTrends ID cookie.*/
	public final static String WEBTRENDS_ID_COOKIE_NAME="WEBTRENDS_ID";

	/**The namespace for WebTrends query parameters.*/
	public final static String WEBTRENDS_QUERY_NAMESPACE="WT";

	/**The delimiter seprating the namespace from the query type and attribute local name.*/
	public final static char QUERY_NAMESPACE_SEPARATOR='.';

	/**The attribute signifying the client's hour.*/
	public final static String BROWSING_HOUR_QUERY_ATTRIBUTE_NAME=createQueryAttributeName(WEBTRENDS_QUERY_NAMESPACE, "bh");

	/**The attribute signifying the browser size.*/
	public final static String BROWSER_SIZE_QUERY_ATTRIBUTE_NAME=createQueryAttributeName(WEBTRENDS_QUERY_NAMESPACE, "bs");

	/**The attribute signifying the client's color depth.*/
	public final static String COLOR_DEPTH_QUERY_ATTRIBUTE_NAME=createQueryAttributeName(WEBTRENDS_QUERY_NAMESPACE, "cd");

	/**The attribute signifying whether the client has Java enabled.*/
	public final static String JAVA_ENABLED_QUERY_ATTRIBUTE_NAME=createQueryAttributeName(WEBTRENDS_QUERY_NAMESPACE, "jo");

	/**The attribute signifying whether JavaScript is supported; supports "Yes" or "No".*/
	public final static String JAVASCRIPT_QUERY_ATTRIBUTE_NAME=createQueryAttributeName(WEBTRENDS_QUERY_NAMESPACE, "js");

	/**The attribute signifying the version of JavaScript.*/
	public final static String JAVASCRIPT_VERSION_QUERY_ATTRIBUTE_NAME=createQueryAttributeName(WEBTRENDS_QUERY_NAMESPACE, "jv");

	/**The attribute signifying the client's screen resolution.*/
	public final static String SCREEN_RESOLUTION_QUERY_ATTRIBUTE_NAME=createQueryAttributeName(WEBTRENDS_QUERY_NAMESPACE, "sr");

	/**The attribute signifying the client's time zone offset from GMT.*/
	public final static String TIMEZONE_QUERY_ATTRIBUTE_NAME=createQueryAttributeName(WEBTRENDS_QUERY_NAMESPACE, "tz");

	/**The attribute signifying the client's language.*/
	public final static String USER_LANGUAGE_QUERY_ATTRIBUTE_NAME=createQueryAttributeName(WEBTRENDS_QUERY_NAMESPACE, "ul");

	/**Creates a WebTrends query attribute name from a namespace and attribute local name.
	@param namespace The attribute namespace.
	@param attributeLocalName The local name of the attribute.
	@return An attribute name composed of the given namespace and local name.
	*/
	public static String createQueryAttributeName(final String namespace, final String attributeLocalName)
	{
		return namespace+QUERY_NAMESPACE_SEPARATOR+attributeLocalName;	//namespace.attributeLocalName
	}
}