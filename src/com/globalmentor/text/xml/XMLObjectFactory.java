package com.globalmentor.text.xml;

import org.w3c.dom.Element;

/**A factory to create objects.
@author Garret Wilson
*/
public interface XMLObjectFactory
{

	/**Creates an object from the provided XML element.
	@param element The element from which an object should be created.
	@return A new object created from the provided XML element.
	*/
	public Object create(final Element element);

}