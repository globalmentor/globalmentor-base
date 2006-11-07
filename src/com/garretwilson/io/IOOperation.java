package com.garretwilson.io;

import java.io.*;

/**A class that encapsulates an I/O-related operation on a particular object.
For example, a method might retrieve a {@link Writer} from a method, and that method might allow an {@link IOOperation} to be specified that determines how the writer will be initialized if it doesn't already exist.
@param <T> The type of object on which the operation will take place.
@author Garret Wilson
*/
public interface IOOperation<T>
{

	/**Performs an operation on the indicated object.
	@param object The object on which the operation will occur.
	@throws IOException if there is an error during the operation.
	*/
	public void perform(final T object) throws IOException;
}
