package com.garretwilson.text.xml;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**A factory to create XML elements from objects.
@author Garret Wilson
*/
public interface ObjectXMLFactory
{

	/**Creates an XML element from the provided object.
	@param object The object from which an XML element should be created.
	@param document The XML document to be used to create an XML element.
	@return A new XML element created from the provided object.
	*/
	public Element create(final Object object, final Document document);

}