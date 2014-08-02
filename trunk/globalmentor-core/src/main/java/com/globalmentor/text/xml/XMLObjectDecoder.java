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

package com.globalmentor.text.xml;

import java.util.*;

import org.w3c.dom.*;

/**An object factory to create objects from XML elements.
@author Garret Wilson
*/
public class XMLObjectDecoder
{

	/**A map of object factories, keyed to namespace URI.*/
	private final Map<String, XMLObjectFactory> xmlObjectFactoryMap=new HashMap<String, XMLObjectFactory>();	//TODO change these to URIs eventually, if this code is even useful anymore

		/**Registers an object factory to be used to create objects encoded from
		  elements in the specified namespace. If an object factory is already
			registered for this namespace, it will be replaced.
		@param namespaceURI The namespace of the elements for which this factory
			should be used to create objects.
		@param factory The object factory that will be used to create objects
			stored in elements from the given namespace namespace.
		*/
		public void registerXMLObjectFactory(final String namespaceURI, final XMLObjectFactory factory)
		{
			xmlObjectFactoryMap.put(namespaceURI, factory);
		}

		/**Retrieves a factory to be used for creating objects stored in elements
		  from the specified namespace URI.
		@param namespaceURI The namespace for which an object factory should be
			returned.
		@return The factory registered for this namespace, or <code>null</code>
			if there is no factory registered for this namespace.
		*/
		protected XMLObjectFactory getXMLObjectFactory(final String namespaceURI)
		{
			return (XMLObjectFactory)xmlObjectFactoryMap.get(namespaceURI);  //return any factory registered for this namespace
		}

	/**Decodes an array of objects from the first-level children of an XML document.
	@param document The XML document containing the encoded objects.
	@return An array of object, each representing an XML element or
		<code>null</code> if an XML element was not recognized.
/TODO add DOM exception here
	*/
	public Object[] decode(final Document document)
	{
		final List objectList=new ArrayList();  //create a list to hold the objects we create
		final NodeList documentChildNodeList=document.getDocumentElement().getChildNodes();  //get the root element's children
		for(int i=0; i<documentChildNodeList.getLength(); ++i)  //look at each of the document child nodes
		{
			final Node node=documentChildNodeList.item(i);  //get a reference to this node
			if(node.getNodeType()==node.ELEMENT_NODE) //if this is an element
			{
				objectList.add(decode((Element)node)); //decode this element and add the object to the list
			}
		}
		return objectList.toArray(); //return the created objects in an array
	}

	/**Creates an object from the given XML element.
	@param element The element from which an object should be created.
	@return An object that represents this XML element, or <code>null</code>
		if the element was not recognized.
//TODO fix	@throws SOAPException Thrown if there is an error accessing the SOAP body element
	*/
	public Object decode(final Element element)
	{
		final String namespaceURI=element.getNamespaceURI();  //get the namespace of the element
		final XMLObjectFactory xmlObjectFactory=getXMLObjectFactory(namespaceURI);  //get a factory based on the namespace
		if(xmlObjectFactory!=null)  //if we have a factory for creating elements from this namespace
			return xmlObjectFactory.create(element);  //let the factory create an object from the element
		else  //if we don't have an object factory
			return null;  //show that we can't create an object
	}

}