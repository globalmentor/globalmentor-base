package com.garretwilson.util;

/**The interface for a class that can perform some processing on an object.
@author Garret Wilson
*/
public interface ObjectProcessor
{

	/**Processes a single object.
	@param object The object to process.
	*/
	public void process(final Object object);
}