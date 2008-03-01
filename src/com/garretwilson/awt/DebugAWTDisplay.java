package com.garretwilson.awt;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.TextArea;
/*G***del when not needed
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
*/
import com.globalmentor.java.Strings;
import com.globalmentor.util.DebugDisplay;

/**Interface for displaying debug information using the AWT.
@author Garret Wilson
@see com.garretwilson.swing.DebugSwingDisplay
*/
public class DebugAWTDisplay implements DebugDisplay
{

	/**The single copy of the debug frame class that's allowed to be created if needed.*/
	private Frame debugFrame=null;

	/**The text area used to log visible output.*/
	private TextArea debugTextArea;

	/**Whether this displayer is enabled.*/
//G***del if not needed	private boolean enabled=false;

	/**Default constructor.*/
	public DebugAWTDisplay()
	{
	}

	/**@return Whether this debug displayer is enabled.*/
	public boolean isEnabled()
	{
		return debugFrame!=null;	//if we have a debug frame, we're enabled
	}

	/**Sets whether displaying of debug information should be enabled. The first
		time this displayer is enabled (that is, <code>newEnabled</code> is set to
		<code>true</code>), the appropriate window will be created for displaying
		information. When <code>newEnabled</code> is set to <code>false</code>,
		that window is destroyed.
	@param newEnabled Whether this displayer will display information.
	*/
	public void setEnabled(final boolean newEnabled)
	{
		if(newEnabled)	//if they want to enable this displayer
		{
			if(!isEnabled())	//if we're not already enabled
			{
				final Frame frame=new Frame();	//create a new frame
				final TextArea textArea=new TextArea(100, 100);	//create a new text area
				frame.setSize(new Dimension(800, 600));	//give the frame a default size
		    textArea.setText("Debug output\n");	//G***put the current date here or something
		    textArea.setEditable(false);	//don't allow the text to be edited
		    frame.add(textArea, BorderLayout.CENTER);
				frame.validate();	//validate the frame, now that we've added all the components
				frame.setVisible(true);	//show the debug frame
				debugFrame=frame;	//set the debug frame
				debugTextArea=textArea;	//set the debug frame
			}
			else	//if they want to disable this displayer
			{
				if(isEnabled())	//if we're enabled now
				{
				  debugFrame.dispose();	//dispose the frame
					debugFrame=null;	//turn off visible debugging
					debugTextArea=null;	//remove our reference to the debug text area
				}
			}
		}
	}

	/**Outputs a debug trace message.
	Meant for messages that show the path of program execution.
	@param traceString The string to output.
	@see Debug#trace
	*/
	public void trace(final String traceString)
	{
		if(debugTextArea!=null)  //if we have a debug text area
		  debugTextArea.append(traceString+'\n');	//append this text to the debug text area, along with a line break
	}

	/**Displays a message dialog with the given message.
	@param message The message to display
	@see Debug#notify
	*/
	public void notify(final String message)
	{
/*G***fix
		final String wrappedMessage=StringUtilities.wrap(message, 100);	//wrap the error message at 100 characters G***probably use a constant here
			//G***maybe later use a specified frame as the parent
		JOptionPane.showMessageDialog(null, wrappedMessage, "Debug Message", JOptionPane.INFORMATION_MESSAGE);	//G***i18n; comment
*/
	}

	/**Displays an error message.
	Meant for errors that are not expected to occur during normal program operations
		 -- program logic errors, and exceptions that are not expected to be thrown.
	@param errorString The error message.
	@see Debug#error
	*/
	public void error(final String errorString)
	{
/*G***fix
		final String wrappedErrorString=StringUtilities.wrap(errorString, 100);	//wrap the error message at 100 characters G***probably use a constant here
			//G***maybe later use a specified frame as the parent
	JOptionPane.showMessageDialog(null, wrappedErrorString, "Error", JOptionPane.ERROR_MESSAGE);	//G***i18n; comment
*/
	}

}