package com.garretwilson.util;

/**Interface for displaying debug information, such as in a graphical user
	interface. This allows debugging both in Swing and the AWT, for example.
@author Garret Wilson
@see com.garretwilson.awt.DebugAWTDisplay
@see com.garretwilson.swing.DebugSwingDisplay
*/
public interface DebugDisplay
{

	/**@return Whether this debug displayer is enabled.*/
	public boolean isEnabled();

	/**Sets whether displaying of debug information should be enabled. Classes
		which implement this interface should default to not being enabled.
	@param newEnabled Whether this displayer will display information.
	*/
	public void setEnabled(final boolean newEnabled);

	/**Outputs a debug trace message.
	Meant for messages that show the path of program execution.
	@param traceString The string to output.
	@see Debug#trace
	*/
	public void trace(final String traceString);

	/**Displays a message dialog with the given message.
	@param message The message to display
	@see Debug#notify
	*/
	public void notify(final String message);

	/**Displays an error message.
	Meant for errors that are not expected to occur during normal program operations
		 -- program logic errors, and exceptions that are not expected to be thrown.
	@param errorString The error message.
	@see Debug#error
	*/
	public void error(final String errorString);

}