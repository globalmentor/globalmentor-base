package com.garretwilson.util;

/**An object that holds another object.
@param <T> The type of object being held.
@author Garret Wilson
*/
public class ObjectHolder<T> extends AbstractObjectDecorator<T>
{

	/**@return The held object.*/
	public T getObject() {return super.getObject();}

	/**Sets the held object.
	@param object The object to hold.
	*/
	public void setObject(final T object) {super.setObject(object);}

	/**Default constructor to hold <code>null</code>.*/
	public ObjectHolder()
	{
		this(null);	//hold null
	}

	/**Held object constructor.
	@param object The object to hold.
	*/
	public ObjectHolder(final T object)
	{
		super(object);	//construct the parent class
	}

}
