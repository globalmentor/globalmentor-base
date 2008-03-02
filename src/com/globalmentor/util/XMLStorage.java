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

package com.globalmentor.util;

import java.io.*;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.lang.InstantiationException;
import java.lang.reflect.*;
import com.globalmentor.io.Files;
import com.globalmentor.java.Classes;
import com.globalmentor.java.Integers;
import com.globalmentor.java.Java;
import com.globalmentor.text.xml.XMLDOMImplementation;
import com.globalmentor.text.xml.XMLProcessor;
import com.globalmentor.text.xml.XMLSerializer;
import com.globalmentor.text.xml.XMLUtilities;
import com.globalmentor.text.xml.xpath.XPath;

import org.w3c.dom.*;

/**An object that facilitates storing data in an XML document.
<p>Special storage/retrieval logic is added for the following classes and types:</p>
<ul>
	<li>arrays</li>
	<li><code>java.awt.Color</code></li>
	<li><code>java.awt.Dimension</code></li>
	<li><code>java.awt.Point</code></li>
	<li><code>java.io.File</code></li>
	<li><code>java.net.URL</code></li>
</ul>
@author Garret Wilson
@deprecated
*/
public class XMLStorage
{

	/**The prefix to a getXXX() function.*/
	protected final static String GET="get";

	/**The prefix to a isXXX() function.*/
	protected final static String IS="is";

	/**The prefix to a setXXX() function.*/
	protected final static String SET="set";

	/**The name of the valueOf() function.*/
//TODO del if not needed	protected final static String VALUE_OF="valueOf";

	/**An empty array of type <code>Object</code>, representing an empty
		argument list of a method.*/
	protected final static Object[] NO_ARGUMENTS=new Object[]{};

	/**An empty array of type <code>Class</code>, representing an empty
		parameter list of a method or constructor.*/
//TODO del if not needed	protected final static Class NO_PARAMETERS=new Class[]{};

	/**The string used to indicate the value <code>null</code>. The character used
		is the last character in the Unicode private use area, which is guaranteed
		never to be assigned by Unicode.
	*/
	protected final static String NULL_STRING=String.valueOf((char)0xF8FF);

	/**The document used to store the data.*/
	private Document storageDocument;

		/**@return The document used to store the data.*/
		public Document getStorageDocument() {return storageDocument;}

		/**Sets the document used to store the data.
		@param document The document that should now be used for data storage.
		*/
		public void setStorageDocument(final Document document) {storageDocument=document;}

	/**The list of variables stored in the XML document.*/
	private List storedVariableList=new ArrayList();

	//TODO comment
/*TODO del
	public XMLStorage()
	{
		storedVariableList=getStoredVariables();	//TODO fix

	}
*/

	/**Returns the list of variables, stored in <code>#StoredVariable</code> objects,
		to be stored and retrieved in the XML document.
		Currently returns an empty list.
		Meant to be overridden by a descendant class to return the appropriate variables.
	@return The list of variables to be stored and retrieved in the XML document
	@see #StoredVariable
	*/
/*TODO del
	public List listStoredVariables()
	{
		return new ArrayList();
	}
*/

	//TODO comment
	public static void retrieve(final Object object, final Document document, final StoredVariable[] storedVariableArray) throws IOException
	{
		for(int i=0; i<storedVariableArray.length; ++i)
		{
			final StoredVariable storedVariable=storedVariableArray[i];
			//TODO testing; comment
			retrieve(storedVariable.name, storedVariable.type, object, document, storedVariable.locationPath, storedVariable.defaultValue);
		}
	}

	//TODO comment
	public static void store(final Object object, final Document document, final StoredVariable[] storedVariableArray) throws IOException
	{
		for(int i=0; i<storedVariableArray.length; ++i)
		{
			final StoredVariable storedVariable=storedVariableArray[i];
			store(storedVariable.name, storedVariable.type, object, document, storedVariable.locationPath);	//TODO testing
		}
	}

	//TODO comment
/*TODO fix or delete
	public void retrieve(final Object object, final Document document) throws IOException
	{
		for(int i=0; i<storedVariableList.size(); ++i)
		{
			final StoredVariable storedVariable=(StoredVariable)storedVariableList.get(i);
			//TODO testing; comment
			retrieve(storedVariable.name, storedVariable.type, object, document, storedVariable.locationPath, storedVariable.defaultValue);
		}
	}
*/




	/**<p>Retrieves the value represented by <code>name</code> from <code>document</code>
		using the XPath <code>locationPath</code> and stores it in <code>object</code>
		using the object's <code>set<em>Name</em>()</code> function. The value is
		converted automatically from primitive types, and for all others a
		<code>String</code> constructor is expected.</p>
		<p>If a value
		for <code>name</code> does not exist, <code>defaultValue</code> will be used.
		<code>locationPath</code> should be an absolute XPath location path.
		<code>object<code> must have a <code>set<em>name</em>()</code> method, and
		<code>type</code> must have a static <code>valueOf(String)</code> function.</p>
	@param name The name of the variable from which the <code>set<em>name</em>()</code>
		method will be derived.
	@param type The class type of the value to be stored.
	@param object The object in which the value should be stored.
	@param document The XML document which holds the value.
	@param locationPath The XPath location path of the value in the document.
	@param defaultValue The value to use if the value cannot be found in the document.
	@return The actual object retrieved, which will be either the value from the
		document or, if the value did not exist in the document, the default value.
	@exception IOException Thrown if <code>object</code> has no
		<code>set<em>name</em>()</code> method or <code>type</code> has no
		<code>valueOf(String)</code> method. This is thrown in place of such exceptions
		as java.lang.reflect.InvocationTargetException and java.lang.NoSuchMethodException.
		This exception is also thrown if the location path is not found, the default
		value is <code>null</code>, and the appropriate <code>set<em>name</em>()</code>
		method does not accept <code>null</code>.
	*/
	public static Object retrieve(final String name, Class type, final Object object, final Document document, final String locationPath, final Object defaultValue) throws IOException
	{
		try
		{
			final Method setMethod=object.getClass().getMethod(SET+name, new Class[]{type});	//get the object's set method
			final List<Node> nodeList=(List<Node>)XPath.evaluatePathExpression(document.getDocumentElement(), locationPath);	//get the values from the appropriate section of the document
	//TODO del when works		Object returnObject=null;	//this is the object we'll return
			Object convertedValue;	//we'll use this variable to get the converted value which should be used in the setXXX() function
			if(nodeList.size()>0)	//if we retrieved at least one value from the document
			{
				final Node node=nodeList.get(0);	//get the first returned node
				final String stringValue=node.getNodeValue();	//get the string value of the node
				try
				{
//TODO fix					Method convertMethod;	//we'll store here the method to use to convert the string to the object
						//The primitive types need special explicit valueOf() invocation, because
						//	an incoming Integer.TYPE would evaluates to int.valueOf(String), which
						//	will not work. And of course, an incoming String.class needs no String.valueOf(String).
					if(type.equals(Integer.TYPE))	//if this is an integer
						convertedValue=Integer.valueOf(stringValue);	//use Integer to convert the string value
					else if(type.equals(Boolean.TYPE))	//if this is a boolean value
						convertedValue=Boolean.valueOf(stringValue);	//use Boolean to convert the string value
					else if(type.equals(String.class))	//if this is a string value
						convertedValue=stringValue;	//no conversion is necessary
					else	//if it's not a primitive type, we'll try to use a string constructor
					{
						try
						{
							final Constructor convertConstructor=type.getConstructor(new Class[]{String.class});	//get a constructor that uses a string
							convertedValue=convertConstructor.newInstance(new Object[]{stringValue});	//convert the string to the appropriate type of object using the object's string constructor
						}
						catch(InstantiationException e)	//if the string constructor could not be invoked
						{
							throw new IOException(e.toString()+": "+type.getName()+'.'+type.getName()+"("+String.class.getName()+")");	//create and throw a storage exception
						}
/*TODO del when works
						final Method convertMethod=type.getMethod(VALUE_OF, new Class[]{String.class});	//get the conversion method
						convertedValue=convertMethod.invoke(null, new Object[]{stringValue});	//convert the string to the appropriate type of object using valueOf()
*/
					}
//TODO del					final Object convertedValue=convertMethod.invoke(null, new Object[]{stringValue});	//convert the string to the appropriate type of object using valueOf()
						//TODO check the NullPointerException here to make sure the convertMethod is static
				}
				catch(NoSuchMethodException e)	//if the string constructor was not found
				{
					throw new IOException(e.toString()+": "+type.getName()+'.'+type.getName()+"("+String.class.getName()+")");	//create and throw a storage exception
				}
/*TODO del when works
				catch(NoSuchMethodException e)	//if the valueOf() function was not found
				{
					throw new IOException(e.toString()+": "+type.getName()+'.'+VALUE_OF+"("+String.class.getName()+")");	//create and throw a storage exception
				}
*/
			}
			else	//if we couldn't find a value in the document
				convertedValue=defaultValue;	//we'll use the default value
//TODO del				return defaultValue;	//return the default value
			try
			{
				setMethod.invoke(object, new Object[]{convertedValue});	//call the object's setXXX() method to set the converted value
			}
			catch(NullPointerException e)	//if we tried to set a null value (which must must have been a default value) for a setXXX() function that doesn't like a null value
			{
				throw new IOException("Location path "+locationPath+" not found in XML document.");	//show that we couldn't find the value in the document TODO is this the best message to return?
			}
			return convertedValue;	//return the value we set
		}
		catch(NoSuchMethodException e)	//if the setname() function was not found
		{
			throw new IOException(e.toString()+": "+object.getClass().getName()+'.'+SET+name+"("+type.getName()+")");	//create and throw a storage exception
		}
		catch(IllegalAccessException e)	//if the setname() or valueOf() cannot be accessed
		{
			throw new IOException(e.toString());	//create and throw a storage exception
		}
		catch(InvocationTargetException e)	//if there was an error (such as converting a string to an integer) during the conversion process
		{
			throw new IOException(e.toString());	//create and throw a storage exception
		}
	}








	/**Stores the value represented by <code>name</code> from <code>document</code>
		using the XPath <code>locationPath> and stores it in <code>object</code>. If a value
		for <code>name</code> does not exist, <code>defaultValue</code> will be used.
		<code>locationPath</code> should be an absolute XPath location path.
		<code>object<code> must have a <code>set<em>name</em>()</code> method, and
		<code>type</code> must have a static <code>valueOf(String)</code> function.
	@return The actual object retrieved, which will be either the value from the
		document or, if the value did not exist in the document, the default value.
	@exception IOException Thrown if <code>object</code> has no
		<code>set<em>name</em>()</code> method or <code>type</code> has no
		<code>valueOf(String)</code> method. This is thrown in place of such exceptions
		as java.lang.reflect.InvocationTargetException and java.lang.NoSuchMethodException;
//TODO have all these comments been converted?
	*/
	public static void store(final String name, Class type, final Object object, final Document document, final String locationPath) throws IOException
	{
		try
		{
			final Method getMethod=object.getClass().getMethod(GET+name);	//get the object's get method
			final List<Node> nodeList=(List<Node>)XPath.evaluatePathExpression(document.getDocumentElement(), locationPath);	//get the values from the appropriate section of the document
			if(nodeList.size()>0)	//if we retrieved at least one value from the document
			{
				final Node node=nodeList.get(0);	//get the first returned node
					final Object valueObject=getMethod.invoke(object, new Object[]{});	//call the object's getXXX() method to get the value to store
					node.setNodeValue(valueObject.toString());	//convert the object to a string and store it
/*TODO del when works
//TODO fix					Method convertMethod;	//we'll store here the method to use to convert the string to the object
					Object convertedValue;	//we'll use this variable to get the converted value which should be used in the setXXX() function
						//the primitive types need special explicit valueOf() invocation, because
						//an incoming Integer.TYPE would evaluates to int.valueOf(String), which
						//will not work.
					if(type.equals(Integer.TYPE))	//if this is an integer
						convertedValue=Integer.valueOf(stringValue);	//use Integer to convert the string value
					else if(type.equals(Boolean.TYPE))	//if this is an boolean value
						convertedValue=Boolean.valueOf(stringValue);	//use Boolean to convert the string value
					else	//if it's not a primitive type
					{
						final Method convertMethod=type.getMethod(VALUE_OF, new Class[]{String.class});	//get the conversion method
						convertedValue=convertMethod.invoke(null, new Object[]{stringValue});	//convert the string to the appropriate type of object using valueOf()
					}
//TODO del					final Object convertedValue=convertMethod.invoke(null, new Object[]{stringValue});	//convert the string to the appropriate type of object using valueOf()
						//TODO check the NullPointerException here to make sure the convertMethod is static
					setMethod.invoke(object, new Object[]{convertedValue});	//call the object's setXXX() method to set the converted value
					return convertedValue;	//return the value we set


				final String stringValue=node.getNodeValue();	//get the string value of the node
*/

			}
			else	//if we couldn't find a value in the document
				throw new IOException("Unable to store value in XML document at XPath location path \""+locationPath+"\".");	//throw an error showing tha twe can't fin the location in the document
		}
		catch(NoSuchMethodException e)	//if the setname() function was not found
		{
			throw new IOException(e.toString()+": "+object.getClass().getName()+'.'+GET+name+"()");	//create and throw a storage exception
		}
		catch(IllegalAccessException e)	//if the setname() or valueOf() cannot be accessed
		{
			throw new IOException(e.toString());	//create and throw a storage exception
		}
		catch(InvocationTargetException e)	//if there was an error (such as converting a string to an integer) during the conversion process
		{
			throw new IOException(e.toString());	//create and throw a storage exception
		}
	}







	/**Stores the given object in the specified document as the root element.
	@param object The object to be stored; must not be <code>null</code>.
	@param document The XML document in which the object should be stored as the
		document element.
	*/
	public static void store(final Object object, final Document document)
	{
		store(object, document.getDocumentElement());	//store the object in the document element
	}

	/**Creates a child element of the given node and stores the given object in
		the child element. The <code>getXXX()</code> and <code>isXXX()</code>
		methods are enumerated and each resulting object is stored.
	@param object The object to be stored; must not be <code>null</code>.
	@param parentNode The node to use as a parent of the element to be created;
		must be either a <code>Document</code> or have a valid owner document.
	*/
	protected static void storeChild(final Object object, final Node parentNode)
	{
		//TODO check object for null, and throw an invalid argument exception if it is
		storeChild(object, parentNode, getStorageName(object.getClass())); //create a new element as a child of the element and store the object in it
	}

	/**Creates a child element of the given node using the specified name and
		stores the given object in the child element. The <code>getXXX()</code>
		and <code>isXXX()</code> methods are enumerated and each resulting object
		is stored.
	@param object The object to be stored, which may or may not be <code>null</code>.
	@param parentNode The node to use as a parent of the element to be created;
		must be either a <code>Document</code> or have a valid owner document.
	@param elementName The name of the element to create in which the object will
		be stored.
	*/
	protected static void storeChild(final Object object, final Node parentNode, final String elementName)
	{
		final Element element=XMLUtilities.getDocument(parentNode).createElementNS(null, elementName);  //create an element for the object
		parentNode.appendChild(element);  //add the element to the parent node
		store(object, element); //store the object in the new element
	}

	/**Stores the given object in the specified element. The <code>getXXX()</code>
		and <code>isXXX()</code> methods are enumerated and each resulting object
		is stored. Only array properties and properties with corresponding
		<code>setXXX()</code> methods for objects that have default constructors
		are stored.
	@param object The object to be stored, which may or may not be <code>null</code>.
	@param element The XML element in which the object should be stored.
	*/
	protected static void store(final Object object, final Element element)
	{
		if(object!=null)  //if the object isn't null
		{
Debug.trace("ready to store object: ", object.getClass().getName());  //TODO del
Debug.trace("using element: ", element.getNodeName());  //TODO del
			final Class objectClass=object.getClass();  //get the object's class
			if(objectClass.isArray()) //if the object being stored is an array
			{
	Debug.trace("object is array with length: ", Array.getLength(object));			  //TODO del
				final int arrayLength=Array.getLength(object);  //see how long the array is
				for(int i=0; i<arrayLength; ++i)  //look at each element in the array
				{
					final Object item=Array.get(object, i); //get this item in the array
					storeChild(item, element);  //store the array item as a child element of the enclosing element
				}
			}
			else  //if we're storing a normal object
			{
					//save all the getXXX() and isXXX() methods as attributes
				final Method methods[]=objectClass.getMethods();	//get all the methods of this object's class
				for(int i=0; i<methods.length; ++i)	//look at each of the methods passed to us
				{
					final Method method=methods[i];	//get a reference to this method
					final String methodName=method.getName();	//get the name of this method
					final int modifiers=method.getModifiers();	//get the modifiers for this method
					if(Modifier.isPublic(modifiers) && method.getParameterTypes().length==0)	//if this is a public method with no parameters
					{
						final String propertyName;  //we'll attempt to get the name of a property to be stored
						if(methodName.startsWith(GET))  //if this is a getXXX() method
							propertyName=methodName.substring(GET.length());  //remove the "get" from the method name to get the property name
						else if(methodName.startsWith(IS))  //if this is a getXXX() method
							propertyName=methodName.substring(IS.length());  //remove the "is" from the method name to get the property name
						else  //if this method name starts with neither "get" nor "is"
							continue; //skip this method
						final Class returnType=method.getReturnType();	//find the return type
						try
						{
							if(shouldStoreProperty(objectClass, propertyName, returnType)) //see if we should store this property
						  {
								final String storageName=Java.getVariableName(propertyName); //get the name of the attribute or element
/*TODO del when works
							if(!returnType.isArray())  //if this is not an array, make sure there is a corresponding setXXX() method
							{
									//make sure we can retrieve a corresponding setXXX() method; if not, this will throw a NoSuchMethodException in response to which we'll simply not save this property
								final Method setMethod=objectClass.getMethod(SET+propertyName, new Class[]{returnType});
							}
							final String storageName=JavaUtilities.getVariableName(propertyName); //get the name of the attribute or element
	Debug.trace("invoking method: ", method.getName()); //TODO del
*/
		/*TODO fix
							if(attributeName.toUpperCase().equals("PCDATA") && returnType.getName().equals("java.lang.String"))	//if this is a PCDATA tag that returns a string
							{
								Element textElement=document.createElement(Element.PCDATA);	//create the element for the text
								final String textString=StringManipulator.makeXMLFriendly(method.invoke(object, new Object[0]).toString());	//get the text, converting all quotes and such to special XML codes
								textElement.setText(textString);	//set the text of the element
								element.addChild(textElement, null);	//add the text element to our object element
							}
							else
		*/
								if(isStringStorage(returnType))  //if this is a type that can be stored as a string (e.g. primitive types and strings)
								{
									final Object valueObject=method.invoke(object, NO_ARGUMENTS);	//call the object's getXXX() method to get the value to store
										//get the value that should be stored in the attribute, using the special null string if the value is null
									final String attributeValue=valueObject!=null ? valueObject.toString() : NULL_STRING;
									element.setAttributeNS(null, storageName, attributeValue);  //set the attribute value of the element
								}
								else if(!returnType.isInterface())  //if the return type isn't an interface
								{
										//if this is an array or the return type has a public default constructor,
										//  meaning we can reconstruct it (this correctly leaves out getClass()),
										//  or some other class we specifically want stored
									if(isStorable(returnType))
									{
										final Object valueObject=method.invoke(object, NO_ARGUMENTS);	//call the object's getXXX() method to get the value to store
										if(!valueObject.equals(object)) //if the property doesn't point back to the object (such as Point.getLocation()), making a circular reference TODO maybe have some sort of better checking for circular references
											storeChild(valueObject, element, storageName); //store the object as a child element of this element
									}
								}
						  }
		/*TODO fix
							else	//since this is not a primitive type, see if it is itself storable
							{
								final Object childObject=method.invoke(object, new Object[]{});	//invoke the get method and retrieve a reference to its return value TODO let's not do this unless we know the return type is instanceof XMLStorable
								if(childObject instanceof XMLStorable || childObject instanceof java.util.Vector)	//if it returns something we can store, or it returns a Vector, which is by default storable
								{
									final Element childElement=store((XMLStorable)childObject, document, attributeName);	//store this child object store in a new element, using the attribute name for the tag name of the new element
									element.addChild(childElement, null);	//add the child element created by the child object to this object's element
								}
							}
		*/
						}
/*TODO del when works
						catch(NoSuchMethodException e)  //if we weren't able to find a setXXX() method
						{
							//ignore the error, having skipped saving the property
						}
*/
						catch(IllegalAccessException e)	//if we can't access the method
						{
							Debug.warn(e); //we should not normally receive this error, but if we do normal processing should still occur
						}
						catch(InvocationTargetException e)	//if the underlying method throws an exception
						{
							Debug.warn(e); //we should not normally receive this error, but if we do normal processing should still occur
						}
					}
				}
			}
		}
		else  //if the object passed is null
		{
			XMLUtilities.appendText(element, NULL_STRING);  //append a string that identifies the object as being null
		}
	}

	/**Stores the given object as an XML document in the specified output stream.
	@param object The object to be stored; must not be <code>null</code>.
	@param outputStream The output stream to which the object should be written.
	@exception IOException Thrown if there is an error writing to the output stream.
	*/
	protected static void store(final Object object, final OutputStream outputStream) throws IOException
	{
		final DOMImplementation domImplementation=new XMLDOMImplementation();	//create a new DOM implementation TODO use a standard way of getting the DOM implementation
		final Document document=domImplementation.createDocument(null, getStorageName(object.getClass()), null);	//create an XML document for the object
		store(object, document); //store the object in the document
		final XMLSerializer xmlSerializer=new XMLSerializer(true);  //create a formatted serializer
		xmlSerializer.setXMLEncodePrivateUse(true);  //show that we should encode Unicode private use characteres, which will show the null character as a character reference
		try
		{
			xmlSerializer.serialize(document, outputStream); //serialize the document to the stream
		}
		catch(UnsupportedEncodingException e) //currently we only support UTF-8, which should always be supported
		{
			throw new AssertionError(e);
		}
	}

	/**Retrieves an object of the given type from the XML document stored in the
		input stream.
	@param inputStream The source of the XML document in which the object is stored.
	@param type The type of object stored in the XML document.
	@exception IOException Thrown if there is an error reading from the input stream.
	@exception InstantiationException Thrown if an instance of the given object
		type could not be created.
	@exception IllegalAccessException Thrown if the application cannot access the
		given object type.
	@exception InvocationTargetException Thrown if the object constructor throws
		an exception.
	@exception NoSuchMethodException Thrown if an initialization method, if
		needed, could not be found for the class.
	*/
	public static Object retrieve(final InputStream inputStream, final Class type) throws FileNotFoundException, IOException, InstantiationException, IllegalAccessException, InvocationTargetException, NoSuchMethodException
	{
		return retrieve(inputStream, inputStream, type);  //retrieve the object, specifying the input stream as the source
	}

	/**Retrieves an object of the given type from the XML document stored in the
		input stream.
	@param inputStream The source of the XML document in which the object is stored.
	@param sourceObject An object (such as a <code>URL</code> or <code>File</code)
		representing the source of the document.
	@param type The type of object stored in the XML document.
	@exception IOException Thrown if there is an error reading from the input stream.
	@exception InstantiationException Thrown if an instance of the given object
		type could not be created.
	@exception IllegalAccessException Thrown if the application cannot access the
		given object type.
	@exception InvocationTargetException Thrown if the object constructor throws
		an exception.
	@exception NoSuchMethodException Thrown if an initialization method, if
		needed, could not be found for the class.
	*/
	protected static Object retrieve(final InputStream inputStream, final Object sourceObject, final Class type) throws IOException, InstantiationException, IllegalAccessException, InvocationTargetException, NoSuchMethodException
	{
		final XMLProcessor xmlProcessor=new XMLProcessor(); //create a new XML processor TODO use JAXP or something
		final Document document=xmlProcessor.parseDocument(inputStream, sourceObject);  //retrieve an XML document from the input stream
		return retrieve(document, type);  //return the object from the document
	}

	/**Retrieves an object of the given type from the XML document stored in the
		file.
	@param file The file in which the object is stored.
	@param type The type of object stored in the XML document.
	@exception FileNotFoundException Thrown if the specified file does not exist.
	@exception IOException Thrown if there is an error reading from the file.
	@exception InstantiationException Thrown if an instance of the given object
		type could not be created.
	@exception IllegalAccessException Thrown if the application cannot access the
		given object type.
	@exception InvocationTargetException Thrown if the object constructor throws
		an exception.
	@exception NoSuchMethodException Thrown if an initialization method, if
		needed, could not be found for the class.
	*/
	public static Object retrieve(final File file, final Class type) throws FileNotFoundException, IOException, InstantiationException, IllegalAccessException, InvocationTargetException, NoSuchMethodException
	{
		return retrieve(file, type, false); //retrieve the object from the file without checking for a backup
	}

	/**Retrieves an object of the given type from the XML document stored in the
		file.
	@param file The file in which the object is stored.
	@param type The type of object stored in the XML document.
	@param useBackup Whether a backup file should be used if the file doesn't exist.
	@exception FileNotFoundException Thrown if the specified file does not exist.
	@exception IOException Thrown if there is an error reading from the file.
	@exception InstantiationException Thrown if an instance of the given object
		type could not be created.
	@exception IllegalAccessException Thrown if the application cannot access the
		given object type.
	@exception InvocationTargetException Thrown if the object constructor throws
		an exception.
	@exception NoSuchMethodException Thrown if an initialization method, if
		needed, could not be found for the class.
	@see Files#checkExists
	*/
	public static Object retrieve(final File file, final Class type, final boolean useBackup) throws FileNotFoundException, IOException, InstantiationException, IllegalAccessException, InvocationTargetException, NoSuchMethodException
	{
		if(useBackup) //if we should use a backup file if the file doesn't exist
			Files.checkExists(file);  //see if we can use a backup file if the file doesn't exist
		final InputStream inputStream=new BufferedInputStream(new FileInputStream(file));  //create a buffered input stream for the file
		try
		{
		  return retrieve(inputStream, file, type); //read and return the object, passing the file as the source of the information
		}
		finally
		{
		  inputStream.close(); //always close the input stream
		}
	}

	/**Stores the given object in the file as an XML document.
	@param object The object to store.
	@param file The file in which the object should be stored.
	@exception FileNotFoundException Thrown if the specified file is invalid.
	@exception IOException Thrown if there is an error writing to the file.
	*/
	public static void store(final Object object, final File file) throws FileNotFoundException, IOException
	{
		store(object, file, false); //store the object without creating a backup
	}

	/**Stores the given object to the file as an XML document. A temporary file
		is used and then copied to ensure error do not affect the origina file.
		If a backup is created, its filename is formed by adding a ".backup"
		extension to the filename.
	@param object The object to store.
	@param file The file in which the object should be stored.
	@param createBackup Whether existing files should be saved in a backup file.
	@exception FileNotFoundException Thrown if the specified file is invalid.
	@exception IOException Thrown if there is an error writing to the file.
	*/  //TODO add a boolean return value which has the status of the actual file move
	public static void store(final Object object, final File file, final boolean createBackup) throws FileNotFoundException, IOException
	{
Debug.trace("Storing in file: ", file); //TODO del
		final File tempFile=Files.getTempFile(file);  //get a temporary file to write to
		final File backupFile=createBackup ? Files.getBackupFile(file) : null;  //get a backup file, if we should create a backup, or null if we shouldn't
		final OutputStream outputStream=new BufferedOutputStream(new FileOutputStream(tempFile)); //create a buffered output stream to the temporary file
		try
		{
			store(object, outputStream);  //store the object in the file output stream
		}
		finally
		{
			outputStream.close(); //always close the output stream
		}
		Files.moveFile(tempFile, file, backupFile); //move the temp file to the normal file, creating a backup if necessary
	}

	/**Constructs an instance of the specified class from the information stored
		in the given XML document.
	@param document The document that holds the information about the object.
	@param objectClass The class for which an instance should be created.
	@return An instance of the object class constructed from the given document.
	@exception IllegalAccessException Thrown if the class or initializer of the
		object is not accessible.
	@exception InstantiationException Thrown if the class represents an abstract
		class, an interface, an array class, a primitive type, or void; or if the
		instantiation fails for some other reason.
	@exception InvocationTargetException Thrown if the object constructor throws
		an exception.
	@exception NoSuchMethodException Thrown if an initialization method, if
		needed, could not be found for the class.
	*/
	public static Object retrieve(final Document document, final Class objectClass) throws IllegalAccessException, InstantiationException, InvocationTargetException, NoSuchMethodException
	{
		return retrieve(document.getDocumentElement(), objectClass);  //construct and return the object from the root element
	}

	/**Constructs an instance of the specified class from the information stored
		in the given element.
	@param element The element that holds the information about the object.
	@param objectClass The class for which an instance should be created.
	@return An instance of the object class constructed from the given element;
		<code>null</code> may be returned if the stored object value was
		<code>null</code>.
	@exception IllegalAccessException Thrown if the class or initializer of the
		object is not accessible.
	@exception InstantiationException Thrown if the class represents an abstract
		class, an interface, an array class, a primitive type, or void; or if the
		instantiation fails for some other reason.
	@exception InvocationTargetException Thrown if the object constructor throws
		an exception.
	@exception NoSuchMethodException Thrown if an initialization method, if
		needed, could not be found for the class.
	*/
	protected static Object retrieve(final Element element, final Class objectClass) throws IllegalAccessException, InstantiationException, InvocationTargetException, NoSuchMethodException
	{
Debug.trace("ready to retrieve object: ", objectClass.getName());  //TODO del
Debug.trace("using element: ", element.getNodeName());  //TODO del
		final NodeList childNodeList=element.getChildNodes(); //get a list of child nodes
			//see if this element encodes the special null value
		if(childNodeList.getLength()==1)  //if this element only has one child node
		{
			final Node childNode=childNodeList.item(0); //get the first child
			if(childNode.getNodeType()==childNode.TEXT_NODE)  //if the only child is a text node
			{
				if(((Text)childNode).getData().equals(NULL_STRING))  //if the element has the null string as its only child node
				  return null;  //show that this element represents a null value
			}
		}
		final Object object;  //we'll create a new instance of the object and store it here
		if(objectClass.isArray()) //if they are wanting to retrieve the element into an array
		{
Debug.trace("object is array");
				//get a list of all the first-level child elements
			final List childElementList=XMLUtilities.getNodesByNameNS(element, Node.ELEMENT_NODE, null, "*", false);  //TODO use a constant here
//TODO del when works			final NodeList childElementList=element.getElementsByTagNameNS(null, "*"); //get all the child elements
			final int childElementCount=childElementList.size();  //find out how many child elements there are
Debug.trace("there are child elements: ", childElementCount);
			final Class componentType=objectClass.getComponentType(); //find out what type of object the array holds
		  object=Array.newInstance(componentType, childElementCount);  //create an array to hold the objects we'll construct from the child elements
		  final Iterator childElementIterator=childElementList.iterator();  //get an iterator to look at the child elements
			int arrayIndex=0; //show that we're starting at the first index of the array
		  while(childElementIterator.hasNext()) //while there are more child elements
//TODO del when works		  for(int i=0; i<childElementCount; ++i)  //construct an object for each child element
			{
				try
				{
					final Object item=retrieve((Element)childElementIterator.next(), componentType);  //retrieve the array item from the child element
				  Array.set(object, arrayIndex, item); //store the item in the array
					arrayIndex++; //go to the next index in the array
//TODO del					componentType.newInstance();  //create a new instance of the object
				}
				catch(IllegalAccessException e) //if we aren't allowed to call this method
				{
					Debug.warn(e);  //we checked a few things already, but if we can't call this method, just warn and continue constructing the object
				}
				catch(InstantiationException e)  //if instantiation fails
				{
					Debug.warn(e);  //warn of the error but try to construct the rest of the object TODO should we do something more severe here?
				}
				catch(InvocationTargetException e) //if the constructor of the element threw an exception
				{
					Debug.warn(e);  //warn and continue constructing the object
				}
			}
		}
		else  //if this is not an array, but a normal object type
		{
Debug.trace("Ready to create object: ", objectClass.getName()); //TODO del
		  object=createObject(element, objectClass);  //create an object for this element
Debug.trace("Created object: ", object);  //TODO del
//TODO del when works			object=objectClass.newInstance(); //create a new instance of the class, using its default constructor
			final Method[] objectMethods=objectClass.getMethods();  //get an array of the object's methods
				//retrieve the objects stored as attributes
			final NamedNodeMap attributeMap=element.getAttributes();  //get a map of attributes
			for(int attributeIndex=attributeMap.getLength()-1; attributeIndex>=0; --attributeIndex) //look at each attribute
			{
				final Attr attribute=(Attr)attributeMap.item(attributeIndex); //get a reference to this attribute
				final String attributeName=attribute.getName(); //get the attribute name
				final String attributeValue=attribute.getValue(); //get the attribute value
Debug.trace("looking at attribute: ", attributeName); //TODO del
Debug.trace("attribute has value: ", attributeValue); //TODO del
				final String properName=Java.getProperName(attributeName); //convert the attribute name into a name appropriate for getXXX() or setXXX()
				final String methodName=SET+properName; //the accessor method is "set" plus the proper name (e.g. setStatus())
Debug.trace("looking for method name: ", methodName);
				for(int methodIndex=objectMethods.length-1; methodIndex>=0; --methodIndex)  //look at each method
				{
					final Method method=objectMethods[methodIndex]; //get a reference to this method
Debug.trace("checking method name: ", method.getName());
					final int modifiers=method.getModifiers();	//get the modifiers for this method
					if(Modifier.isPublic(modifiers) && method.getName().equals(methodName)) //if this method is public and has the correct name
					{
Debug.trace("checking parameters");
						final Class[] parameterTypes=method.getParameterTypes();  //get an array of parameter types
						if(parameterTypes.length==1)  //if this method takes one parameter
						{
							try
							{
								final Object argumentObject=createObject(attributeValue, parameterTypes[0]);  //change the attribute value into the correct type of object, if we can
/*TODO del when works
							Object argumentObject=null;	//we'll create a parameter from the attribute value and store it here
							if(!attributeValue.equals(NULL_STRING))  //only try to convert the value if it doesn't represent null
							{
								final Class parameterType=parameterTypes[0];  //get the parameter type of the setXXX() method
	Debug.trace("has one parameter of type: ", parameterType.getName());
								argumentObject=createObject(attributeValue, parameterType);  //change the attribute value into the correct type of object, if we can
								if(argumentObject==null)	//if we didn't find a way to convert the attribute value to an argument object
									continue; //try another method; don't use this setXXX() method
							}
*/
								method.invoke(object, new Object[]{argumentObject}); //invoke the setXXX() method with the constructed parameter as an argument
								break;  //don't look for any more setXXX() methods for this element
							}
							catch(InstantiationException e)	//if we can't create the object, for some reason
							{
								Debug.warn(e); //warn that we couldn't find a way to construct an object from the string
							}
							catch(IllegalAccessException e) //if we aren't allowed to call this method
							{
								Debug.warn(e);  //we checked a few things already, but if we can't call this method, just warn and continue constructing the object
								break;  //don't try to construct this object anymore
							}
							catch(InvocationTargetException e)  //if the underlying method throws an exception
							{
								Debug.warn(e);  //warn of the error but try to construct the rest of the object TODO should we do something more severe here?
								break;  //don't try to construct this object anymore
							}
						}
					}
					//TODO somehow warn if the object was not stored
				}
			}


				//retrieve the objects stored as child elements
			for(int childIndex=childNodeList.getLength()-1; childIndex>=0; --childIndex) //look at each child node
			{
				final Node childNode=childNodeList.item(childIndex); //get a reference to this child node
				if(childNode.getNodeType()==childNode.ELEMENT_NODE) //if this is an element
				{
					final Element childElement=(Element)childNode;  //cast the node to an element
					final String elementName=childElement.getLocalName(); //get the element name
Debug.trace("looking at child element: ", elementName); //TODO del
					final String properName=Java.getProperName(elementName); //convert the element name into a name appropriate for getXXX() or setXXX()
					final String methodName=SET+properName; //the accessor method is "set" plus the proper name (e.g. setStatus())
Debug.trace("looking for method name: ", methodName);
					for(int methodIndex=objectMethods.length-1; methodIndex>=0; --methodIndex)  //look at each method
					{
						final Method method=objectMethods[methodIndex]; //get a reference to this method
Debug.trace("checking method name: ", method.getName());
						final int modifiers=method.getModifiers();	//get the modifiers for this method
						if(Modifier.isPublic(modifiers) && method.getName().equals(methodName)) //if this method is public and has the correct name
						{
Debug.trace("checking parameters");
							final Class[] parameterTypes=method.getParameterTypes();  //get an array of parameter types
							if(parameterTypes.length==1)  //if this method takes one parameter
							{
								final Class parameterType=parameterTypes[0];  //get the parameter type of the setXXX() method
Debug.trace("has one parameter of type: ", parameterType.getName());
								if(isStorable(parameterType)) //if this parameter is an object that can be stored
								{
									try
									{
										final Object argumentObject=retrieve(childElement, parameterType);  //attempt to retrieve this type of object from the child element
										method.invoke(object, new Object[]{argumentObject}); //invoke the setXXX() method with the constructed parameter as an argument
										break;  //don't look for any more setXXX() methods for this element
									}
									catch(IllegalAccessException e) //if we aren't allowed to call this method
									{
										Debug.warn(e);  //we checked a few things already, but if we can't call this method, just warn and continue constructing the object
									}
									catch(InvocationTargetException e)  //if the underlying method throws an exception
									{
										Debug.warn(e);  //warn of the error but try to construct the rest of the object TODO should we do something more severe here?
									}
									catch(NoSuchMethodException e)  //if a method is not found for initializing the object
									{
										Debug.warn(e);  //warn of the error but try to construct the rest of the object
									}
								}
							}
						}
					}
					//TODO somehow warn if the object was not stored
				}
			}
		}
		return object;  //return the object we constructed
	}

	/**Creates an instance of the specified type, using information in the given
		element if needed. In most cases and for most JavaBeans, the default
		constructor is called and information in the element is ignored. Some
		legacy classes, such as <code>java.awt.Dimension</code>, do not require
		appropriate <code>setXXX()</code> methods and require a special constructor
		to be called with information from the elemenet.
	@param element The element which contains initialization information for the
		object.
	@param type The type of class to create, which must have a default constructor.
	@exception IllegalAccessException Thrown if the constructor could not be accessed.
	@exception InstantiationException Thrown if there was an error creating the object.
	@exception InvocationTargetException Thrown if the constructor throws an exception.
	@exception NoSuchMethodException Thrown if an initialization method, if
		needed, could not be found for the class.
	*/
	protected static Object createObject(final Element element, final Class type) throws IllegalAccessException, InstantiationException, InvocationTargetException, NoSuchMethodException
	{
//TODO del		final Object object;  //we know we'll be able to at least create the object
		if(type.equals(java.awt.Color.class))  //if this is a Color
		{
			//use the constructor that takes integers, because that's what the getRed(), etc. accessor methods return and would have been saved as
		  return createObject(element, type, new Class[]{Integer.TYPE, Integer.TYPE, Integer.TYPE, Integer.TYPE}, new String[]{"Red", "Green", "Blue", "Alpha"});  //construct a Color from its red, green, blue, and alpha properties
		}
		else if(type.equals(java.awt.Dimension.class))  //if this is a Dimension
		{
				//we must use Dimension.setSize(int, int) because in JDK<=1.3 Dimension.setSize(double, double) does not work
		  return createObject(element, type, "setSize", new Class[]{Integer.TYPE, Integer.TYPE}, new String[]{"Width", "Height"});  //construct a Dimension and initialize it from the width and height properties
		}
		else if(type.equals(java.awt.Point.class))  //if this is a Point
		{
		  return createObject(element, type, "setLocation", new Class[]{Double.TYPE, Double.TYPE}, new String[]{"X", "Y"});  //construct a Point and initialize it from the x and y properties
		}
		else  //if this class doesn't need special initialization
			return type.newInstance(); //for all other classes, create a new instance of the class, using its default constructor
	}

	/**Creates an instance of the specified type, using an array of property
		names for its constructor, getting information from the given element.
		A <code>java.awt.Color</code> object, for example, would be created
		with property names of <code>{"Red", "Green", "Blue", "Alpha"}</code>.
		Each of these properties must have previously been stored in the element.
	@param element The element which contains initialization information for the
		object.
	@param type The type of class to create.
	@param constructorParameterTypes The types of parameters used by the constructor.
	@param constructorPropertyNames An array of property names, capitalized as
		appropriate for a <code>set</code> metehod, for initializing the object
		in a constructor.
	@return An object created from the appropriate constructor.
	@exception IllegalAccessException Thrown if the constructor could not be accessed.
	@exception InstantiationException Thrown if there was an error creating the object.
	@exception InvocationTargetException Thrown if the constructor throws an exception.
	@exception NoSuchMethodException Thrown if the given constructor could not be found
		for the class.
	*/
	protected static Object createObject(final Element element, final Class type, final Class[] constructorParameterTypes, final String[] constructorPropertyNames) throws IllegalAccessException, InstantiationException, InvocationTargetException, NoSuchMethodException
	{
		final Object[] arguments=new Object[constructorParameterTypes.length]; //create an array to hold our arguments
		//TODO maybe make sure the parameter type array and name array are the same length
		final Constructor constructor=type.getConstructor(constructorParameterTypes); //get the constructor with the given parameter types
/*TODO del when works
		final Constructor[] constructors=type.getConstructors();  //get an array of all constructors for this type
		for(int constructorIndex=constructors.length-1; constructorIndex>=0; --constructorIndex) //look at each constructor
		{
			final Constructor constructor=constructors[constructorIndex];  //get a reference to this constructor
			final Class[] parameterTypes=constructor.getParameterTypes(); //get an array of all the parameters this constructor takes
		  if(parameterTypes.length==constructorPropertyNames.length) //if this constructor has the same number of parameters as the number of properties passed to us
		  {
				int parameterIndex; //this variable goes outside our loop so that its scope will allow it to be checked to verify the loop completed
*/
				for(int parameterIndex=constructorParameterTypes.length-1; parameterIndex>=0; --parameterIndex)  //look at each parameter
				{
//TODO del					try
					{
//TODO del Debug.trace("Getting property "+constructorPropertyNames[parameterIndex]+" for type "+parameterTypes[parameterIndex]);  //TODO del
						arguments[parameterIndex]=getProperty(element, constructorPropertyNames[parameterIndex], constructorParameterTypes[parameterIndex]); //get this property value from the element
//TODO del Debug.trace("Got property: ", arguments[parameterIndex]); //TODO del
					}
/*TODO del
					catch(Exception e) //if one of the arguments could not be constructed
					{
						break;  //ignore this error but try the next method
					}
*/
				}
//TODO del when works				if(parameterIndex<0)  //if we successfully created all arguments
					return constructor.newInstance(arguments); //invoke the constructor with the arguments we created
/*TODO del when works
		  }
		}
		throw new InstantiationException("Could not find an appropriate constructor for "+type.getName()+" for the properties "+constructorPropertyNames+".");  //show that we couldn't find an appropriate constructor TODO do we want to throw an error, or just warn here?
*/
	}

	/**Creates an instance of the specified type, using the default constructor
		and then calling a method with certain properties as arguments, getting
		information from the given element.
		Each of these properties must have previously been stored in the element.
	@param element The element which contains initialization information for the
		object.
	@param type The type of class to create. The class must have a default
		constructor.
	@param methodName The name of the method to call for initialization.
	@param methodParameterTypes The types of parameters used by the given method.
	@param methodPropertyNames An array of property names, capitalized as
		appropriate for a <code>set</code> method, for initializing the object
		in a constructor.
	@return An object created and initialized from the appropriate method.
	@exception IllegalAccessException Thrown if the constructor could not be accessed.
	@exception InstantiationException Thrown if there was an error creating the object.
	@exception InvocationTargetException Thrown if the constructor throws an exception.
	@exception NoSuchMethodException Thrown if the given method could not be found
		for the class.
	*/
	protected static Object createObject(final Element element, final Class type, final String methodName, final Class[] methodParameterTypes, final String[] methodPropertyNames) throws IllegalAccessException, InstantiationException, InvocationTargetException, NoSuchMethodException
	{
Debug.trace("Using default constructor to create instance of ", type.getName());  //TODO del
Debug.trace("Looking for method: ", methodName);  //TODO del
		final Object object=type.newInstance(); //create a new instance of the object using its default constructor
		final Object[] arguments=new Object[methodParameterTypes.length]; //create an array to hold our arguments
		//TODO maybe make sure the parameter type array and name array are the same length
		final Method method=type.getMethod(methodName, methodParameterTypes); //get the initialization method


		int parameterIndex; //this variable goes outside our loop so that its scope will allow it to be checked to verify the loop completed
		for(parameterIndex=methodParameterTypes.length-1; parameterIndex>=0; --parameterIndex)  //look at each parameter
		{
//TODO fix			try
			{
				arguments[parameterIndex]=getProperty(element, methodPropertyNames[parameterIndex], methodParameterTypes[parameterIndex]); //get this property value from the element
Debug.trace("constructed argument: ", arguments[parameterIndex]);
Debug.trace("argument type: ", arguments[parameterIndex].getClass().getName());
			}
/*TODO fix
			catch(Exception e) //if one of the arguments could not be constructed
			{
				Debug.warn(e);  //warn of the error
				return
				break;  //ignore this error but try the next method
			}
*/
		}
		if(parameterIndex<0)  //if we successfully created all arguments
		{
Debug.trace("Ready to initialize object: ", object);  //TODO del
			method.invoke(object, arguments);  //invoke the method of the object using the arguments we created
Debug.trace("Initialized object: ", object);  //TODO del
			return object;  //return the object we created, now that we've initialized it
		}



/*TODO fix
		final Method[] methods=type.getMethods();  //get an array of all methods for this type
		for(int methodIndex=methods.length-1; methodIndex>=0; --methodIndex) //look at each method
		{
			final Method method=methods[methodIndex];  //get a reference to this method
Debug.trace("Found method: ", method);  //TODO del
			final Class[] parameterTypes=method.getParameterTypes(); //get an array of all the parameters this method takes
		  if(parameterTypes.length==methodPropertyNames.length) //if this method has the same number of parameters as the number of properties passed to us
		  {
				int parameterIndex; //this variable goes outside our loop so that its scope will allow it to be checked to verify the loop completed
				for(parameterIndex=parameterTypes.length-1; parameterIndex>=0; --parameterIndex)  //look at each parameter
				{
					try
					{
						arguments[parameterIndex]=getProperty(element, methodPropertyNames[parameterIndex], parameterTypes[parameterIndex]); //get this property value from the element
Debug.trace("constructed argument: ", arguments[parameterIndex]);
Debug.trace("argument type: ", arguments[parameterIndex].getClass().getName());
					}
					catch(Exception e) //if one of the arguments could not be constructed
					{
						break;  //ignore this error but try the next method
					}
				}
				if(parameterIndex<0)  //if we successfully created all arguments
				{
Debug.trace("Ready to initialize object: ", object);  //TODO del
					method.invoke(object, arguments);  //invoke the method of the object using the arguments we created
Debug.trace("Initialized object: ", object);  //TODO del
					return object;  //return the object we created, now that we've initialized it
				}
		  }
		}
*/
		throw new InstantiationException("Could not find an appropriate initialization method \""+methodName+"\" for "+type.getName()+" for the properties "+methodPropertyNames+".");  //show that we couldn't find an appropriate method
	}

	/**Retrieves a property value from the specified element with the appropriate
		name, assuming the specified type. Currently only attributes are searched
		for property values. TODO fix for child elements, if needed
	@param element The element that holds the information about the object.
	@param propertyName The name of the property, capitalized appropriately for
		a <code>get</code> method.
	@param propertyType The type of object to create.
	@return An object representing the value of the property, created from the
		information in the element; this value may be <code>null</code>, based upon
		the value stored in the element.
	@exception InstantiationException Thrown if a value could not be created for
		the property from the information in the element.
	*/
	protected static Object getProperty(final Element element, final String propertyName, final Class propertyType) throws InstantiationException
	{
		final String storageName=Java.getVariableName(propertyName); //get the name of the attribute or element
		final String attributeValue=XMLUtilities.getDefinedAttributeNS(element, null, storageName); //get the attribute value if it is defined
		if(attributeValue!=null)  //if the attribute is defined
		{
			return createObject(attributeValue, propertyType);  //create an object from the attribute value
		}
		//TODO check here for child elements: else
		throw new InstantiationException("Could not construct a "+propertyType.getName()+" for the property , \""+propertyName+"\".");  //show that we couldn't get an object with the correct type from the element
	}

	/**Determines the storage name for the given class. This name is used for the
		names of elements in the XML document. The name is determined for objects
		by removing the package name, if present, and the name of any enclosing
		class (for internal classes), and decapitalizing the first letter. Names for
		arrays are created by adding an "s" or "es" to the name of the type of object
		they store, after the name of the object has been processed as above.
	@param objectClass The class for which a storage name should be generated.
	@return The name to use for storage, such as for the name of an XML element.
	@see Classes#getSimpleName
	*/
	protected static String getStorageName(final Class objectClass)
	{
		if(objectClass.isArray()) //if this is an array object
		{
				//start with a storage name that is the same as the storage name for the type of object stored in the array
			final StringBuffer stringBuffer=new StringBuffer(getStorageName(objectClass.getComponentType()));
		  if(stringBuffer.charAt(stringBuffer.length()-1)=='s') //if the name ends with "s" already
				stringBuffer.append('e'); //append an 'e' before the 's'
			stringBuffer.append('s'); //append an 's' to the name
			return stringBuffer.toString(); //return the storage name we constructed
		}
		else  //if this isn't an array object, the element name will be the decapitalized version of the simple class name
			return Java.getVariableName(Classes.getSimpleName(objectClass));  //get the decapiatlized version of the simple class name
	}

	/**Finds or constructs an object of the specified type that represents the
		given string value. If the type is one of the primitive types, an appropriate
		primitive wrapper class is constructed. Otherwise, a string constructor for
		the type is searched. Values for string types are simply returned as-is.
	@param value The string value to be converted to an object of the appropriate
		type.
	@param type The type of object which should represent the given string value.
	@return An object of the appropriate type representing the given string value,
		which may be null, based upon the value.
	@exception InstantiationException Thrown if the object cannot be created for
		some reason.
	@see NULL_STRING
	*/
	protected static Object createObject(final String value, final Class type) throws InstantiationException
	{
		if(!value.equals(NULL_STRING))  //only try to convert the value if it doesn't represent null
		{
			if(type.equals(Boolean.TYPE))	//if the set method accepts a boolean
				return Boolean.valueOf(value);	//create a boolean with the correct value
			else if(type.equals(Byte.TYPE))	//if the set method accepts a byte
				return Byte.valueOf(value);	//create a byte with the correct value
			else if(type.equals(Character.TYPE) && value.length()>0)	//if the set method accepts a character and we have a least one character in the attribute value
				return new Character(value.charAt(0));	//create a character with the value of the first character of tha attribute value
			else if(type.equals(Short.TYPE))	//if the set method accepts a short
				return Short.valueOf(value);	//create a short with the correct value
			else if(type.equals(Integer.TYPE))	//if the set method accepts an integer
			{
					//parse an integer from the string, correctly returning an integer
					//  even if the integer is in double form (i.e. 123.0); this is
					//  for Dimension, which stores its integer values as doubles but
					//  must use Dimension.setSize(int, int) instead of
					//  Dimension.setSize(double, double) because the latter does not
					//  work in the JDK1.3.
				return new Integer(Integers.parseIntValue(value));
//TODO del when works				return Integer.valueOf(value);	//create an integer with the correct value
			}
			else if(type.equals(Long.TYPE))	//if the set method accepts a long
				return Long.valueOf(value);	//create a long with the correct value
			else if(type.equals(Float.TYPE))	//if the set method accepts a float
				return Float.valueOf(value);	//create a float with the correct value
			else if(type.equals(Double.TYPE))	//if the set method accepts a double
				return Double.valueOf(value);	//create a double with the correct value
			else if(type.equals(String.class))	//if the set method accepts a string
				return value;	//we can just pass the value without modification
			else  //if this isn't a primitive type or a string, see if there is a string constructor
			{
				try
				{
					final Constructor stringConstructor=type.getConstructor(new Class[]{String.class});	//get a constructor that uses a string
					return stringConstructor.newInstance(new Object[]{value});	//convert the string to the appropriate type of object using the object's string constructor
				}
				catch(NoSuchMethodException e)  //if there isn't a string constructor
				{
					//do nothing if there isn't a string constructor
				}
				catch(IllegalAccessException e)	//if we can't access the string constructor
				{
					Debug.warn(e); //warn of the error and return null
				}
	/*TODO del
				catch(InstantiationException e)	//if we can't create the object, for some reason
				{
					Debug.warn(e); //warn of the error and return null
					throw e;  //rethrow the object
				}
	*/
				catch(InvocationTargetException e)	//if the underlying method throws an exception
				{
					Debug.warn(e); //warn of the error and return null
				}
			}
			throw new InstantiationException("Could not construct a "+type.getName()+" from the value, \""+value+"\".");  //show that we couldn't get an object with the correct type from the string
		}
		else  //if the value represents null
			return null;  //return null, because that's the value represented
	}

	/**Determines whether an object that has a particular class should be stored
		as a string value (i.e. using the object's <code>toString()</code> method)
		in an attribute instead of as a child element. These classes include:
		<ul>
		  <li>primitive types besides <code>void</code></li>
			<li><code>java.io.File</code></li>
			<li><code>java.lang.String</code></li>
			<li><code>java.net.URL</code></li>
		</ul>
	@param type The class of the object being stored.
	@return
	*/
	protected static boolean isStringStorage(final Class type)
	{
		return ((type.isPrimitive() && !type.equals(Void.TYPE)) //if this is a primitive type besides void
			  || type.equals(java.io.File.class)
			  || type.equals(java.lang.String.class)
			  || type.equals(java.net.URL.class)
			) ? true : false; //return true if the class is one of our recognized types for string storage
	}

	/**Determines whether an object type is one that should be stored.
	@param objectType The type of object to be stored or retrieved.
	@return <code>true</code> if the type is that of an object that should be stored.
	*/
	protected static boolean isStorable(final Class objectType)
	{
		return objectType.isArray() //if this is an array
			  //or if return type has a public default constructor,
				//  meaning we can reconstruct it (this correctly leaves out getClass()),
			|| Classes.getPublicDefaultConstructor(objectType)!=null
		  || objectType.equals(java.awt.Color.class); //or if this is a Color
	}

	/**Determines whether an object's property should be stored.
	@param objectType The type of object being stored.
	@param propertyName The name (suitable for concatenation with "set") of the
		property to be stored.
	@param propertyType The type of property being stored.
	@return Whether the property should be stored.
	*/
	protected static boolean shouldStoreProperty(final Class objectType, final String propertyName, final Class propertyType)
	{
		if(propertyType.isArray()) //if the property is an array
			return true;  //arrays always get stored
//TODO del		final Class objectType=object.getClass(); //find out what type of object has properties to be stored
		if(objectType.equals(java.awt.Color.class)) //if they are storing a Color
		{
			if(propertyName.equals("Red") || propertyName.equals("Green") || propertyName.equals("Blue") || propertyName.equals("Alpha"))  //if the red, green, blue, or alpha values are being stored
				return true;  //store them, even though they don't have corresonding setXXX() methods
		}
		else if(objectType.equals(java.awt.Dimension.class)) //if they are storing a Dimension
		{
			if(propertyName.equals("Height") || propertyName.equals("Width"))  //if the height or width values are being stored
				return true;  //store them, even though they don't have corresonding setXXX() methods
		}
		if(objectType.equals(java.awt.Point.class)) //if they are storing a Point
		{
			if(propertyName.equals("X") || propertyName.equals("Y"))  //if the X or Y values are being stored
				return true;  //store them, even though they don't have corresonding setXXX() methods
//TODO del			if(propertyName.equals("Location")) //the recursive "location
		}
		try //for all other types, make sure we can retrieve a corresponding setXXX() method; if not, this will throw a NoSuchMethodException in response to which we'll simply not save this property
		{
			final Method setMethod=objectType.getMethod(SET+propertyName, new Class[]{propertyType});
			return true;  //show that, because there is a setXXX() property, we should store this property
		}
		catch(NoSuchMethodException e)  //if we weren't able to find a setXXX() method
		{
			return false; //show that we shouldn't store this property
		}
	}

	/**The encapsulation of the information needed to store and retrieve a variable
		from an XML document.
		If a particular XPath location path is invalid for a particular document,
		the default value object will be used. If the default value is
		<code>null</code>, that value will be used if the <code>set<em>name</em>()</code>
		method supports it. If the method does not support <code>null</code>
		(it is a primitive value, for example), an <code>IOException</code>
		will be thrown.
	*/
	public static class StoredVariable
	{
		/**The name of the variable. This is conventionally in uppercase, as the
			appropriate object's code>get<em>name</em>()</code> and code>set<em>name</em>()</code>
			methods will be used to store and retrieve the value.
		*/
		public String name;

		/*The type of value stored.*/
		public Class type;

		/**The XPath locationPath of the element or attribute used to store this value.*/
		public String locationPath;

		/**The default value of the variable if it isn't found in the document.*/
		public Object defaultValue;

		/**Constructs a stored variable.
		@param newName The name of the variable.
		@param newType The type of value stored.
		@param newLocationPath The XPath location path of the value's location in
			the document.
		@param newDefaultValue The default value of the variable.
		*/
		public StoredVariable(final String newName, final Class newType, final String newLocationPath, final Object newDefaultValue)
		{
			name=newName;
			type=newType;
			locationPath=newLocationPath;
			defaultValue=newDefaultValue;
		}

	}

}