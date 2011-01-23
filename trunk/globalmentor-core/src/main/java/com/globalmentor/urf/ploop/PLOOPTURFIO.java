package com.globalmentor.urf.ploop;

import java.io.*;
import java.lang.reflect.InvocationTargetException;
import java.net.*;

import com.globalmentor.java.Classes;
import com.globalmentor.java.Java;
import com.globalmentor.urf.*;
import com.globalmentor.util.DataException;

/**Class for loading and saving a type of Java object to and from a TURF instance.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@param <T> The type to read and write.
@author Garret Wilson
*/
public class PLOOPTURFIO<T> extends AbstractTURFIO<T>
{

	/**Class constructor.
	@param objectClass The class representing the type of object being loaded and saved.
	@exception NullPointerException if the given class is <code>null</code>.
	*/
	public PLOOPTURFIO(final Class<T> objectClass)
	{
		super(objectClass);	//construct the parent class
	}

	/**Reads an object from an input stream using an existing URF instance.
	@param urf The URF instance to use in creating new resources.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The resource read from the input stream.
	@exception NullPointerException if the given URF instance and/or input stream is <code>null</code>.
	@exception IOException if there is an error reading the data.
	@exception ClassCastException if no appropriate resource factory was installed, and the loaded resource is not of the correct Java class.
	*/ 
	public T read(final URF urf, final InputStream inputStream, final URI baseURI) throws IOException
	{
		readURF(urf, inputStream, baseURI);	//read URF from the input stream
		final PLOOPURFProcessor ploopProcessor=new PLOOPURFProcessor();	//create a new PLOOP processor
		final T object;
		try
		{
			object=ploopProcessor.getObject(urf, getObjectClass());	//create and retrieve the object using the URF instance
		}
		catch(final DataException dataException)	//if the data was incorrect
		{
			throw new IOException(dataException);			
		}
		catch(final InvocationTargetException invocationTargetException)	//if a Java constructor threw an exception
		{
			throw new IOException(invocationTargetException);			
		}
		if(object==null)	//if there is no resource
		{
			throw new IOException("No object found with type "+getObjectClass()+".");
		}
		return object;	//return the object
	}

	/**Merges the description of a resource from an input stream using an existing URF instance.
	The object description from the input must have the identical type as the given object.
	This version delegates to {@link #merge(Object, URF, InputStream, URI)} using {@link #createURF()} to create a new URF instance.
	@param object The object into which the read description should be merged.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The object after the property merging.
	@exception NullPointerException if the given object and/or input stream is <code>null</code>.
	@exception IOException if there is an error reading the data.
	@exception ClassCastException if no appropriate resource factory was installed, and the loaded resource is not of the correct Java class.
	*/ 
	public final T merge(final T object, final InputStream inputStream, final URI baseURI) throws IOException
	{
		return merge(object, createURF(), inputStream, baseURI);	//create a new URF data model, showing the base URI, and merge and return the object
	}
	
	/**Merges the description of a resource from an input stream using an existing URF instance.
	The object description from the input must have the identical type as the given object.
	@param object The object into which the read description should be merged.
	@param urf The URF instance to use in creating new resources.
	@param inputStream The input stream from which to read the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The object after the property merging.
	@exception NullPointerException if the given object, URF instance, and/or input stream is <code>null</code>.
	@exception IOException if there is an error reading the data.
	@exception ClassCastException if no appropriate resource factory was installed, and the loaded resource is not of the correct Java class.
	*/ 
	public T merge(final T object, final URF urf, final InputStream inputStream, final URI baseURI) throws IOException
	{
		readURF(urf, inputStream, baseURI);	//read URF from the input stream
		final Class<?> objectClass=object.getClass();	//get the class of the objeect
		final URFResource resource=urf.getResourceByTypeURI(Classes.createJavaURI(objectClass));	//get the resource for this class
		if(resource==null)	//if there is no resource
		{
			throw new IOException("No resource found with type "+objectClass+".");
		}
		final PLOOPURFProcessor ploopProcessor=new PLOOPURFProcessor();	//create a new PLOOP processor
		try
		{
			ploopProcessor.setObjectProperties(object, resource);	//set the object's properties based upon the read description
		}
		catch(final DataException dataException)	//if the data was incorrect
		{
			throw new IOException(dataException);			
		}
		catch(final InvocationTargetException invocationTargetException)	//if a Java constructor threw an exception
		{
			throw new IOException(invocationTargetException);			
		}
		return object;	//return the object after the merge
	}
	
	/**Writes an object to an output stream.
	@param outputStream The output stream to which to write the data.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@param object The object to write to the given output stream.
	@throws IOException Thrown if there is an error writing the data.
	*/
	public void write(final OutputStream outputStream, final URI baseURI, final T object) throws IOException
	{
		final PLOOPURFGenerator urfGenerator=new PLOOPURFGenerator();	//create a new PLOOP URF generator
		final URFResource resource;
		try
		{
			resource=urfGenerator.generateURFResource(object);	//generate the URF resource
		}
		catch(final InvocationTargetException invocationTargetException)	//if a Java constructor throw an exception
		{
			throw new IOException(invocationTargetException);			
		}
		writeURFResource(outputStream, baseURI, resource);	//write the generated URF resource
	}

}
