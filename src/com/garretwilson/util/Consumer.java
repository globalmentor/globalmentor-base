package com.garretwilson.util;

/**An object that consumes other objects.
@param <T> The type of objects being consumed.
@author Garret Wilson
*/
public interface Consumer<T>
{

	/**Consumes an object.
	@param object The object to consume.
	*/
	public void consume(final T object);
}
