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

import com.globalmentor.log.Log;
import static com.globalmentor.text.xml.XML.*;

import org.w3c.dom.*;

/**
 * An XML factory to create XML document and element from objects.
 * @author Garret Wilson
 */
public class XMLObjectEncoder {

	/** A map of XML factories, keyed to class package names. */
	private final Map<String, ObjectXMLFactory> objectXMLFactoryMap = new HashMap<String, ObjectXMLFactory>();

	/**
	 * Registers an XML factory to be used to create elements encoded from objects in the specified package. If an XML factory is already registered for this
	 * package, it will be replaced.
	 * @param packageName The package name of the elements for which this factory should be used to create XML elements, such as "java.lang".
	 * @param factory The XML factory that will be used to create XML elements from objects from the given package.
	 */
	public void registerObjectXMLFactory(final String packageName, final ObjectXMLFactory factory) {
		objectXMLFactoryMap.put(packageName, factory);
	}

	/**
	 * Retrieves a factory to be used for creating XML elements from objects from the specified package.
	 * @param packageName The package name for which an XML element factory should be returned.
	 * @return The factory registered for this package, or <code>null</code> if there is no factory registered for this package.
	 */
	protected ObjectXMLFactory getObjectXMLFactory(final String packageName) {
		return (ObjectXMLFactory)objectXMLFactoryMap.get(packageName); //return any factory registered for this package
	}

	/**
	 * Encodes an object and stores it in an XML document as the document element.
	 * @param object The object to encode.
	 * @param domImplementation The DOM implementation to be used as a document factory.
	 * @return The new XML document that contains the object information.
	 */
	public Document encode(final Object object, final DOMImplementation domImplementation) {
		final Document document = encode(new Object[] { object }, domImplementation); //store the object in an array and encode it in an XML document, returning the document
		//get the first child element of the document element
		final Element objectElement = (Element)document.getDocumentElement().getChildNodes().item(0);
		return createDocument(objectElement); //create and return a new XML document from the object element
	}

	/**
	 * Encodes objects and stores them in an XML document as a child elements of the document element.
	 * @param objects The objects to encode.
	 * @param domImplementation The DOM implementation to be used as a document factory.
	 * @return The new XML document that contains the object information.
	 */
	public Document encode(final Object[] objects, final DOMImplementation domImplementation) {
		//create a dummy document in which to store the given message TODO fix; use constants
		final Document document = domImplementation.createDocument(null, "objects", null); //TODO use a constant here
		final Element documentElement = document.getDocumentElement();
		for(int i = 0; i < objects.length; ++i) { //look at each of the objects
			final Element element = encode(objects[i], document); //encode this object to an element
			if(element != null) //if we were able to create an element TODO fix error handling
				documentElement.appendChild(element); //encode this object in an element and add it to the document
		}
		return document; //return the document we created
	}

	/**
	 * Encodes an object and stores it in an XML element.
	 * @param object The object to encode.
	 * @param document The XML document to be used as an element factory.
	 * @return The new XML element that contains the object information.
	 */
	public Element encode(final Object object, final Document document) {
		final String packageName = object.getClass().getPackage().getName(); //get the name of the object's package
		final ObjectXMLFactory objectXMLFactory = getObjectXMLFactory(packageName); //get a factory based on the package name
		if(objectXMLFactory != null) //if we have a factory for creating elements from this package
			return objectXMLFactory.create(object, document); //let the factory create an element from the object
		else { //if we don't have an XML factory
			Log.warn("cannot find XML factory for package: " + packageName); //TODO fix
			return null; //show that we can't create an element TODO fix
		}
	}

}