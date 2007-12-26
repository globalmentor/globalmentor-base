package com.garretwilson.text.xml.stylesheets.css;

/**Standard cursors supported by CSS.
@author Garret Wilson
@see <a href="http://www.w3.org/TR/CSS21/ui.html">CSS 2.1 User Interface: Cursors</a>
*/
public enum XMLCSSCursor
{

	/**The UA determines the cursor to display based on the current context.*/
	AUTO,
	/**A simple crosshair (e.g., short line segments resembling a "+" sign).*/
	CROSSHAIR,
	/**The platform-dependent default cursor. Often rendered as an arrow.*/
	DEFAULT,
	/**The cursor is a pointer that indicates a link.*/
	POINTER,
	/**Indicates something is to be moved.*/
	MOVE,
	/**Indicate that some edge is to be moved from the east of the box.*/
	E_RESIZE,
	/**Indicate that some edge is to be moved from the north-east corner of the box.*/
	NE_RESIZE,
	/**Indicate that some edge is to be moved from the north-west corner of the box.*/
	NW_RESIZE,
	/**Indicate that some edge is to be moved from the north of the box.*/
	N_RESIZE,
	/**Indicate that some edge is to be moved from the south-east corner of the box.*/
	SE_RESIZE,
	/**Indicate that some edge is to be moved from the south-west corner of the box.*/
	SW_RESIZE,
	/**Indicate that some edge is to be moved from the south corner of the box.*/
	S_RESIZE,
	/**Indicate that some edge is to be moved from the west of the box.*/
	W_RESIZE,
	/**Indicates text that may be selected. Often rendered as an I-beam.*/
	TEXT,
	/**Indicates that the program is busy and the user should wait. Often rendered as a watch or hourglass.*/
	WAIT,
	/**A progress indicator. The program is performing some processing, but is different from {@link #WAIT} in that the user may still interact with the program. Often rendered as a spinning beach ball, or an arrow with a watch or hourglass.*/
	PROGRESS,
	/**Help is available for the object under the cursor. Often rendered as a question mark or a balloon.*/
	HELP;
}
