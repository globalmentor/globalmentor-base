package com.garretwilson.io;

import java.io.IOException;
import java.io.OutputStream;

/**An output stream that throws away its data.
@author Garret Wilson
*/
public class NullOutputStream extends OutputStream
{
	/**Writes the specified byte to this output stream.
	This version does nothing.
	@param b The <code>byte</code>.
	@exception IOException if an I/O error occurs.
	*/
	public void write(int b) throws IOException {}

}
