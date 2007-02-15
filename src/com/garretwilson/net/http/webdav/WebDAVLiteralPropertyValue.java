package com.garretwilson.net.http.webdav;

import com.garretwilson.util.ObjectDecorator;

/**A literal WebDAV value of a resource property.
@author Garret Wilson
*/
public class WebDAVLiteralPropertyValue extends ObjectDecorator<String> implements WebDAVPropertyValue
{

	/**String literal constructor.
	@param literal The literal string this value represents.
	@exception NullPointerException if the given literal is <code>null</code>.
	*/
	public WebDAVLiteralPropertyValue(final String literal)
	{
		super(literal);	//construct the parent class
	}
}
