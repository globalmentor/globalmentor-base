package com.garretwilson.event;

import java.util.*;
import com.garretwilson.util.*;

/**Class that stores and retrieves event listeners, facilitating the creation
	of another class that allows event listeners to be registered with it.
<p>This class uses little memory if there are no registered event listeners.</p>
<p>This class was inspired by <code>javax.swing.EventListenerList</code>
	1.33 12/03/01 by Georges Saab, Hans Muller, and James Gosling.</p>
@author Garret Wilson
@see javax.swing.EventListenerList
*/
public class EventListenerManager
{
	/**The map containing weak sets of event listeners; only allocated when needed.*/
	private Map listenerSetMap=null;




}
