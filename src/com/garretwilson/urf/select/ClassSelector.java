package com.garretwilson.urf.select;

import java.net.URI;

/**A selector that selects an object based upon its runtime class (as opposed to its URF resource type).
This implementation lazily caches any selected class, so that any later updates to the selected class will not be reflected in {@link #getSelectClass()}.
@author Garret Wilson
*/
public class ClassSelector extends AbstractSelector
{

	/**The lazily-cached select class.*/
	private Class<?> selectClass=null;

	/**Default constructor.*/
	public ClassSelector()
	{
		this(null);	//construct the class with no URI
	}

	/**URI constructor.
	@param uri The URI for the new resource.
	*/
	public ClassSelector(final URI uri)
	{
		super(uri);  //construct the parent class
	}

	/**Returns the select class identified by this selector.
	@return This selector's class designation, or <code>null</code> if this rule has no <code>select:selectClass</code> property that is a Java class.
	@exception ClassNotFoundException if the class identified by the <code>select:selectClass</code> property cannot be found.
	*/
	public Class<?> getSelectClass() throws ClassNotFoundException
	{
/*TODO fix with info:java/ URIs
		if(selectClass==null)	//if we haven't cached a select class (or there has never been a select class found); the race condition here is benign
		{
			final RDFLiteral classLiteral=(RDFLiteral)getPropertyValue(THEME_NAMESPACE_URI, CLASS_PROPERTY_NAME);	//get the literal value
			selectClass=classLiteral!=null ? Class.forName(classLiteral.getLexicalForm()) : null;	//if there is a class literal, try to get a class by that name
		}
		return selectClass;	//return the class, if any
*/
		throw new UnsupportedOperationException("TODO: implement getSelectClass()");
	}

	/**Determines if this selector selects a given object.
	This version checks to see if the object is an instance of the selected class.
	@param object The object to test for a match, or <code>null</code> if there is no object.
	@return <code>true</code> if this selector selects the given object.
	@exception IllegalStateException if this selector doesn't specify a selected class.
	@see #getSelectClass()
	@see Class#isInstance(Object)
	*/
	public boolean selects(final Object object)
	{
		try
		{
			final Class<?> objectClass=getSelectClass();	//get the designated class
			if(objectClass!=null)	//if a class is specified
			{
				if(!objectClass.isInstance(object))	//if the object is not an instance of the specified class
				{
					return false;	//the object doesn't match
				}
			}
			else
			{
				throw new IllegalStateException("Class selector missing class selection property.");
			}
			return true;	//the object passed all the tests
		}
		catch(final ClassNotFoundException classNotFoundException)	//if an unknown class is found
		{
			throw new IllegalStateException(classNotFoundException);	//this error would have probably already been found by now
		}
	}
}