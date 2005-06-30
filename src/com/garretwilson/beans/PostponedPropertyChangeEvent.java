package com.garretwilson.beans;

import java.beans.*;
import com.garretwilson.event.PostponedEvent;
import static com.garretwilson.lang.ObjectUtilities.*;

/**Encapsulation of a property change event and its associated property change support, queued for later firing.
@author Garret Wilson
*/
public class PostponedPropertyChangeEvent implements PostponedEvent<PropertyChangeEvent>
{

	/**Whether the postponed event has been fired.*/
	private boolean fired=false;

		/**@return Whether the postponed event has been fired.*/
		public boolean isFired() {return fired;}

	/**The property change support that will ultimately fire the property change event, or <code>null</code> if there is no property change support and therefore no listeners.*/
	private final PropertyChangeSupport propertyChangeSupport;

		/**@return The property change support that will ultimately fire the property change event, or <code>null</code> if there is no property change support and therefore no listeners.*/
		protected PropertyChangeSupport getPropertyChangeSupport() {return propertyChangeSupport;}

	/**The property change event that has been postponed.*/
	private final PropertyChangeEvent propertyChangeEvent;

		/**@return The property change event that has been postponed.*/
		public PropertyChangeEvent getEvent() {return propertyChangeEvent;}

	/**Creates a postponed property change event.
	@param propertyChangeSupport The property change support that will ultimately fire the property change event, or <code>null</code> if there is no property change support and therefore no listeners.
	@param propertyChangeEvent The property change event that has been postponed.
	@exception NullPointerException if the property change event is <code>null</code>.
	*/
	public PostponedPropertyChangeEvent(final PropertyChangeSupport propertyChangeSupport, final PropertyChangeEvent propertyChangeEvent)
	{
		this.propertyChangeSupport=propertyChangeSupport;
		this.propertyChangeEvent=checkNull(propertyChangeEvent, "Property change event cannot be null.");
	}

	/**Reports that a bound property has changed.
	This method does the actual delegation to the property change support.
	@exception IllegalStateException if the postponed property change event has already been fired.
	*/
	public void fireEvent()
	{
		synchronized(this)	//prevent race conditions around checking the fired status
		{
			if(isFired())	//if we've already fired the event
			{
				throw new IllegalStateException("Postponed property change event "+getEvent()+" has already been fired.");
			}
			fired=true;	//show that we've started to fire the event
		}
		final PropertyChangeSupport propertyChangeSupport=getPropertyChangeSupport();	//get our property change support
		if(propertyChangeSupport!=null)	//if we have property change support (if not, we couldn't have listeners)
		{
			propertyChangeSupport.firePropertyChange(getEvent());	//delegate to the property change support to fire the event
		}
	}
}
